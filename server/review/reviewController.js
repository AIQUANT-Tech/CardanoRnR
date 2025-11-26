import Review from "./Reviews.js";
import BookingInfo from "../Hotel_Booking_System/Hbs_Booking_Info_Schema.js";
import UserGuestMap from "../../server/user/UserGuestMap.js";
import User from "../user/UserMast.js";
import ReviewCategory from "../reviewCategory/ReviewCategories.js";
import responses from "../utils/responses.js";
import roles from "../utils/roles.js";
import {
  lockFunds,
  redeemFunds,
  scriptAddress,
} from "../cardano_transaction/cardanoLucid.js";
import reviewQueue from "./reviewQueue.js";

import { Constr, Data, Lucid, Blockfrost } from "lucid-cardano";
import review from "./Reviews.js";
import { log } from "console";

const lucid = await Lucid.new(
  new Blockfrost(
    "https://cardano-preprod.blockfrost.io/api/v0",
    process.env.BLOCKFROST_KEY
  ),
  "Preprod"
);

lucid.selectWalletFromSeed(process.env.MNEMONIC);

function convertBigInts(obj) {
  if (typeof obj === "bigint") {
    return obj.toString();
  }
  if (Array.isArray(obj)) {
    return obj.map(convertBigInts);
  }
  if (typeof obj === "object" && obj !== null) {
    const res = {};
    for (const key in obj) {
      res[key] = convertBigInts(obj[key]);
    }
    return res;
  }
  return obj;
}

export async function waitForUTxOWithTimeout(
  scriptAddress,
  targetDatum,
  expectedTxHash,
  maxWaitTime = 600000,
  pollInterval = 10000
) {
  const expectedDatumHex = Data.to(targetDatum);
  const startTime = Date.now();
  while (Date.now() - startTime < maxWaitTime) {
    const utxos = await lucid.utxosAt(scriptAddress);
    const matchingUtxo = utxos.find(
      (utxo) =>
        utxo.datum === expectedDatumHex && utxo.txHash === expectedTxHash
    );
    if (matchingUtxo) {
      console.log("Target UTxO with expected txHash found on-chain.");
      return matchingUtxo;
    }
    console.log(
      `UTxO not found. Waiting for ${pollInterval / 1000} seconds...`
    );
    await new Promise((resolve) => setTimeout(resolve, pollInterval));
  }
  throw new Error(
    "Timed out waiting for UTxO with the target datum and expected txHash"
  );
}



export const createReview = async (req, res) => {
  try {
    const { new_review_rating_create_rq } = req.body;
    if (!new_review_rating_create_rq) {
      return res.status(400).json({ error: "Invalid request format" });
    }

    const {
      header: { request_type, user_name },
      user_email_id,
      overall_rating,
      overall_review,
      category_wise_review_rating,
    } = new_review_rating_create_rq;

    if (request_type !== "CREATE_NEW_REVIEW_RATING") {
      return res.status(400).json({ error: "Invalid request type" });
    }
    if (
      !user_email_id ||
      overall_rating == null ||
      !overall_review ||
      !Array.isArray(category_wise_review_rating) ||
      category_wise_review_rating.length === 0
    ) {
      return res.status(400).json({ error: "All fields are required" });
    }

    // Retrieve user by email.
    const user = await User.findOne({ email: user_email_id, status: true });
    if (!user) {
      return res.status(404).json({ error: "User not found" });
    }

    // Validate category-wise reviews.
    const categoryIds = category_wise_review_rating.map(
      (catReview) => catReview.category_id
    );
    const validCategories = await ReviewCategory.find({
      category_id: { $in: categoryIds },
    });
    if (validCategories.length !== categoryIds.length) {
      return res
        .status(404)
        .json({ error: "One or more categories not found" });
    }

    // Calculate dynamic fields.
    const overallRatingCount = await Review.find({ category_id: null });
    // console.log(overallRatingCount);

    const ratingCount = BigInt(overallRatingCount.length);
    // console.log(ratingCount);

    const totalScore = overallRatingCount.reduce(
      (acc, catReview) => acc + BigInt(catReview.overall_rating),
      0n
    );
    console.log("Controller Rating Count: ", ratingCount);
    console.log("Controller total score : ", totalScore);

    const reputationScore = ratingCount > 0n ? totalScore / ratingCount : 0n;
    const timestamp = BigInt(Date.now());

    // Use user's _id as reviewId (converted to hex)
    const reviewId = Buffer.from(user._id.toString()).toString("hex");

    // Build the on-chain review datum and redeemer.
    const reviewDatum = new Constr(0, [
      reviewId,
      new Constr(1, []),
      BigInt(Math.floor(overall_rating)),
      timestamp,
      totalScore,
      ratingCount,
      reputationScore,
    ]);
    const reviewRedeemer = new Constr(0, [reviewId]);

    console.log("Locking review data on-chain...");
    const lockTxHash = await lockFunds(reviewDatum);
    if (!lockTxHash) {
      return res
        .status(500)
        .json({ error: "Lock review funds failed on blockchain" });
    }
    console.log("Review data locked, transaction hash:", lockTxHash);

    // Create the overall review record (for overall review, category_id is null).
    const overallReviewDoc = new Review({
      user_id: user._id,
      category_id: null,
      overall_review,
      overall_rating,
      blockchain_tx: lockTxHash,
      status: false, // pending blockchain processing
      created_at: new Date(),
    });
    const savedOverall = await overallReviewDoc.save();

    // Prepare and insert category-wise review documents.
    const reviewsToInsert = category_wise_review_rating.map((catReview) => {
      const validCategory = validCategories.find(
        (cat) => cat.category_id === catReview.category_id.toString()
      );
      if (!validCategory) {
        throw new Error(`Category with ID ${catReview.category_id} not found.`);
      }
      return {
        user_id: user._id,
        category_id: validCategory._id,
        review: catReview.review,
        rating: catReview.rating,
        overall_review,
        overall_rating,
        blockchain_tx: "", // Not processed on-chain for category reviews.
        status: true, // Immediately mark these as stored.
        created_at: new Date(),
      };
    });
    const savedCategoryReviews = await Review.insertMany(reviewsToInsert);

    // Serialize the on-chain datum and redeemer as hex strings using Data.to().
    const serializedReviewDatum = Data.to(reviewDatum);
    const serializedReviewRedeemer = Data.to(reviewRedeemer);

    // Enqueue a background job to process the blockchain redemption.
    reviewQueue.add({
      reviewId: savedOverall._id.toString(),
      serializedReviewDatum,
      serializedReviewRedeemer,
      lockTxHash,
    });

    return res.status(201).json({
      status: "pending",
      message:
        "Review submitted. Blockchain locking is pending; redemption will complete later.",
      overall: savedOverall,
      category_reviews: savedCategoryReviews,
    });
  } catch (error) {
    console.error("Error creating review:", error.message);
    return res.status(500).json({ error: "Internal server error" });
  }
};

export const getAllReviews = async (req, res) => {
  try {
    // Fetch all reviews with populated user and category fields
    const reviews = await Review.find();
    //.populate('user', 'name email')
    //.populate('category');

    return res.status(200).json(reviews);
  } catch (error) {
    console.error(error);
    return res.status(500).json({
      new_review_rating_create_rs: { status: responses.error.retrieveReview },
      error: error.message,
    });
  }
};

// Get a review by ID
export const getReviewById = async (req, res) => {
  try {
    const { id } = req.params;

    const review = await Review.findById({
      _id: id,
    });
    // .populate('review_list.category_id');
    if (!review) {
      return res.status(404).json({
        success: false,
        message: "Review not found",
      });
    }
    const data = {
      user_id: review.user_id,
      review: review,
      status: review.status,
    };

    res.status(200).json({
      success: true,
      data,
    });
  } catch (error) {
    res.status(500).json({
      success: false,
      message: "Failed to fetch review",
      error: error.message,
    });
  }
};

export const getReviewsForBusinessUser = async (req, res) => {
  try {
    const { review_rating_info_rq } = req.body;

    // Validate the incoming request body
    if (!review_rating_info_rq) {
      return res.status(400).json({
        review_rating_info_rs: {
          review_rating_info_by_user: [],
          status: responses.validation.invalidRequest,
        },
      });
    }

    const {
      header: { user_name, product, request_type },
    } = review_rating_info_rq;

    // Validate request_type and product
    if (request_type !== "REVIEW_RATING_INFO" || product !== "rnr") {
      return res.status(400).json({
        review_rating_info_rs: {
          review_rating_info_by_user: [],
          status: responses.validation.invalidRequest,
        },
      });
    }

    // 1. Fetch all reviews with lean() to return plain objects
    const reviews = await Review.find()
      .populate("user_id", "display_name")
      .populate("category_id", "category_name")
      .select(
        "_id user_id category_id review rating overall_review overall_rating is_responded created_at"
      )
      .lean();

    if (!reviews || reviews.length === 0) {
      return res.status(404).json({
        review_rating_info_rs: {
          review_rating_info_by_user: [],
          status: responses.validation.NoReview,
        },
      });
    }

    // 2. Extract unique user IDs (from the populated user _id)
    const userMongoIds = [
      ...new Set(
        reviews.map((review) => review.user_id?._id.toString()).filter(Boolean)
      ),
    ];

    // 3. Fetch full user documents to get the custom "user_id" field used in mapping
    const users = await User.find({ _id: { $in: userMongoIds } }).lean();

    // Build a lookup: map from Mongo _id to custom user id (e.g., user.user_id)
    const customIdByMongoId = {};
    users.forEach((user) => {
      customIdByMongoId[user._id.toString()] = user.user_id; // adjust if the field name differs
    });

    // 4. Get the array of custom user ids (those used in UserGuestMap)
    const customUserIds = Object.values(customIdByMongoId).filter(Boolean);

    // 5. Batch fetch all UserGuestMap documents using the custom user ids
    const userGuestMaps = await UserGuestMap.find({
      user_id: { $in: customUserIds },
    }).lean();

    // Build a lookup: map from custom user id to the mapping
    const guestMappingByCustomId = {};
    userGuestMaps.forEach((mapping) => {
      guestMappingByCustomId[mapping.user_id.toString()] = mapping;
    });

    // 6. Extract unique guest IDs from the mappings
    const guestIds = [
      ...new Set(
        userGuestMaps
          .map((mapping) => mapping.guest_id?.toString())
          .filter(Boolean)
      ),
    ];

    // 7. Batch fetch all BookingInfo records for these guest IDs
    const bookings = await BookingInfo.find({ guest_id: { $in: guestIds } })
      .select("room_type check_in_date check_out_date guest_id")
      .lean();

    // Build a lookup: map guest_id to booking info
    const bookingByGuestId = {};
    bookings.forEach((booking) => {
      bookingByGuestId[booking.guest_id.toString()] = booking;
    });

    // 8. Helper to compute time of stay in days
    const computeTimeOfStay = (checkIn, checkOut) => {
      if (!checkIn || !checkOut) return "N/A";
      const diffInMs = new Date(checkOut) - new Date(checkIn);
      const diffInDays = Math.ceil(diffInMs / (1000 * 60 * 60 * 24));
      return `${diffInDays} ${diffInDays === 1 ? "day" : "days"}`;
    };

    // 9. Merge all data per review
    const reviewRatingInfoByUser = reviews.map((review) => {
      const mongoUserId = review.user_id?._id.toString();
      // Get the custom user id for this review
      const customUserId = customIdByMongoId[mongoUserId];
      // Use that to get the mapping
      const guestMapping = guestMappingByCustomId[customUserId];
      let bookingDetails = null;
      let timeOfStay = "N/A";

      if (guestMapping && guestMapping.guest_id) {
        bookingDetails = bookingByGuestId[guestMapping.guest_id.toString()];
        if (
          bookingDetails &&
          bookingDetails.check_in_date &&
          bookingDetails.check_out_date
        ) {
          timeOfStay = computeTimeOfStay(
            bookingDetails.check_in_date,
            bookingDetails.check_out_date
          );
        }
      }

      return {
        review_id: review._id.toString(),
        user_id: mongoUserId,
        user_display_name: review.user_id?.display_name || "Unknown User",
        category_id: review.category_id?._id?.toString() || null,
        category_name: review.category_id?.category_name || "Overall Rating",
        review_responded: !!review.is_responded,
        review: review.review || review.overall_review,
        rating:
          review.rating?.toString() ||
          (review.overall_rating && review.overall_rating.toString()),
        created_at: review.created_at,
        booking_details: bookingDetails,
        time_of_stay: timeOfStay,
      };
    });

    return res.status(200).json({
      review_rating_info_rs: {
        review_rating_info_by_user: reviewRatingInfoByUser,
        status: responses.success.success,
      },
    });
  } catch (error) {
    console.error("Error fetching reviews for business user:", error.message);
    return res.status(500).json({
      review_rating_info_rs: {
        review_rating_info_by_user: [],
        status: responses.error.failedFetchReview,
      },
      error: error.message,
    });
  }
};


export const getUserReviews = async (req, res) => {
  try {
    const { user_id } = req.body;

    if (!user_id) {
      return res.status(400).json({
        status: "fail",
        message: "User ID is required",
      });
    }

    const user = await User.findById(user_id);

    if (!user) {
      return res.status(404).json({
        status: "fail",
        message: "User not found",
      });
    }

    const reviews = await Review.find({
      user_id: user_id,
      status: "true",
    }).select(
      "overall_review overall_rating review rating category_id created_at"
    );

    if (!reviews || reviews.length === 0) {
      return res.status(404).json({
        status: "fail",
        message: "No reviews found for the user",
      });
    }

    const validOverallRatings = reviews
      .map((r) => r.overall_rating)
      .filter((rating) => typeof rating === "number" && !isNaN(rating));

    const totalOverallRating =
      validOverallRatings.length > 0
        ? validOverallRatings.reduce((sum, rating) => sum + rating, 0) /
          validOverallRatings.length
        : 0.0;

    const categoryIds = [...new Set(reviews.map((r) => r.category_id))];

    const categories = await ReviewCategory.find({
      _id: { $in: categoryIds },
    }).select("category_id category_name category_description");

    const categoryDetailsMap = categories.reduce((map, category) => {
      map[category._id.toString()] = {
        category_name: category.category_name,
        category_desc: category.category_description,
      };
      return map;
    }, {});

    const categoryWiseReviews = reviews.reduce((categories, review) => {
      const category = categoryDetailsMap[review.category_id];
      if (!category) return categories;

      const categoryId = review.category_id.toString();
      if (!categories[categoryId]) {
        categories[categoryId] = {
          category_name: category.category_name,
          category_desc: category.category_desc,
          reviews: [],
        };
      }

      categories[categoryId].reviews.push({
        review: review.review,
        rating: review.rating,
      });

      return categories;
    }, {});

    const categoryWiseReviewList = Object.values(categoryWiseReviews);

    return res.status(200).json({
      status: "success",
      data: {
        user: {
          user_id: user._id,
          user_name: user.name,
        },
        overall_review: reviews[0]?.overall_review || "No overall review",
        overall_rating: totalOverallRating,
        created_at: reviews[0].created_at,
        category_wise_reviews: categoryWiseReviewList,
      },
    });
  } catch (error) {
    console.error("Error fetching user reviews:", error.message);
    return res.status(500).json({
      status: "error",
      message: "Failed to fetch reviews",
      error: error.message,
    });
  }
};

//Get review count and total review
export const calculateReviewStats = async (req, res) => {
  try {
    const reviews = await Review.find();

    if (reviews.length === 0) {
      return res
        .status(404)
        .json({ status: "error", message: "No reviews found for this user." });
    }

    let totalReviews = 0;
    let totalReviewAmount = 0;

    reviews.forEach((review) => {
      totalReviews += 1;
      if (review.category_id == null) {
        totalReviewAmount += review.overall_rating;
      } else {
        totalReviewAmount += review.rating;
      }
    });

    return res.status(200).json({
      status: "success",
      reviewStats: {
        totalReviews,
        totalReviewAmount,
      },
    });
  } catch (error) {
    console.error("Error calculating review stats:", error.message);
    return res.status(500).json({
      status: "error",
      message: error.message,
    });
  }
};


export async function fetchReputationScore(userId) {
  try {
    const reviewId = Buffer.from(userId.toString()).toString("hex");
    console.log("ReviewId:", reviewId);

    const businessAddress = await lucid.wallet.address();
    const pkh =
      lucid.utils.getAddressDetails(businessAddress).paymentCredential.hash;

    const enterpriseAddress = lucid.utils.credentialToAddress(
      lucid.utils.keyHashToCredential(pkh)
    );

    const utxos = await lucid.utxosAt(enterpriseAddress);

    let reputationScore = 0;

    for (const utxo of utxos) {
      const inlineData = utxo.inlineDatum || utxo.datum;
      if (!inlineData) continue;

      const datum = Data.from(inlineData);

      if (!datum.fields || datum.fields.length < 7) continue;

      // FIX HERE:
      const onChainReviewId = datum.fields[0];  // already a string

      console.log("OnChain ReviewId:", onChainReviewId);

      if (onChainReviewId === reviewId) {
        // numeric fields also come as strings, so convert them:
        reputationScore = Number(datum.fields[6]);
        console.log("Fetched Reputation Score:", reputationScore);
        break;
      }
    }

    return reputationScore;

  } catch (error) {
    console.error("Error in fetchReputationScore:", error);
    throw error;
  }
}



// --------------------------------------------------------------------
// Express route handler to fetch the reputation score from the blockchain via a POST request.
export const getReputationScoreFromBlockchain = async (req, res) => {
  try {
    if (!req.body || !req.body.userId) {
      return res
        .status(400)
        .json({ error: "userId is required in the request body" });
    }
    const { userId } = req.body;
    console.log("UserId received:", userId);

    const reputationScore = await fetchReputationScore(userId);
    return res.status(200).json({ reputationScore });
  } catch (error) {
    console.error(
      "Error fetching reputation score from blockchain:",
      error.message
    );
    return res.status(500).json({ error: error.message });
  }
};


export const getReviewsForEndUser = async (req, res) => {
  console.log("fn called");

  try {
    if (!req.body || !req.body.review_rating_fetch_rq) {
      return res.status(400).json({
        review_rating_fetch_rs: {
          status: responses.validation.invalidRequest,
        },
      });
    }
    const { review_rating_fetch_rq } = req.body;
    const { request_type } = review_rating_fetch_rq.header || {};
    if (request_type !== "FETCH_REVIEW_RATING") {
      return res.status(400).json({
        review_rating_fetch_rs: {
          status: responses.validation.invalidRequest,
        },
      });
    }

    // Fetch reviews that have been successfully processed (status "true")
    const reviews = await Review.find({ status: "true" })
      .select(
        "_id user_id overall_review overall_rating review rating category_id created_at blockchain_tx"
      )
      .populate("user_id", "display_name");

    console.log("Reviews fetched:", reviews);

    // if (!reviews || reviews.length === 0) {
    //   return res.status(404).json({
    //     review_rating_fetch_rs: {
    //       status: responses.validation.reviewNotFound,
    //     },
    //   });
    // }

    // Filter for overall reviews (assumed to be those without a category_id)
    const overallReviews = reviews.filter((r) => !r.category_id);
    let blockchainReputationScore = 0;

    // Sort overall reviews by creation date (latest first)
    overallReviews.sort(
      (a, b) => new Date(b.created_at) - new Date(a.created_at)
    );

    const lastUserId =
      overallReviews.length > 0 ? overallReviews[0].user_id._id : null;
    console.log(lastUserId);

    if (overallReviews.length > 0) {
      blockchainReputationScore = await fetchReputationScore(lastUserId);
    }

    console.log("ReputationScore 1: ", blockchainReputationScore);

    // For each overall review, fetch its own blockchain reputation score and booking details
    const reviewRatingDetailsOverall = await Promise.all(
      overallReviews.map(async (r) => {
        const userId = r.user_id._id;

        let bookingDetailsForThisUser = null;
        const userFetch = await User.findOne({ _id: userId });
        // console.log("My Data:", userFetch);

        const userGuestMapping = await UserGuestMap.findOne({
          user_id: userFetch.user_id,
        });
        if (userGuestMapping && userGuestMapping.guest_id) {
          bookingDetailsForThisUser = await BookingInfo.findOne({
            guest_id: userGuestMapping.guest_id,
          }).select("room_type check_out_date");
        }

        return {
          review_id: r._id,
          user_name: r.user_id?.display_name || "Anonymous",
          user_id: userId,
          created_at: r.created_at,
          review: r.overall_review,
          rating: r.overall_rating,
          reputation_score: blockchainReputationScore,
          booking_details: bookingDetailsForThisUser, // This includes room_type and check_out_date
        };
      })
    );

    // Category-wise review details (unchanged)
    const categoryIds = [...new Set(reviews.map((r) => r.category_id))];
    const categories = await ReviewCategory.find({
      _id: { $in: categoryIds },
    }).select("category_id category_name category_description");

    const categoryDetailsMap = categories.reduce((map, category) => {
      map[category._id.toString()] = {
        category_name: category.category_name,
        category_desc: category.category_description,
      };
      return map;
    }, {});

    const categoryWiseReviewRating = reviews.reduce((categories, review) => {
      const category = categoryDetailsMap[review.category_id];
      if (!category) return categories;
      const categoryId = review.category_id.toString();
      if (!categories[categoryId]) {
        categories[categoryId] = {
          category_name: category.category_name,
          category_desc: category.category_desc,
          review_rating_details_by_category: [],
        };
      }
      categories[categoryId].review_rating_details_by_category.push({
        review: review.review,
        rating: review.rating,
      });
      return categories;
    }, {});

    const categoryWiseReviewList = Object.values(categoryWiseReviewRating);

    return res.status(200).json({
      review_rating_fetch_rs: {
        overall_rating: overallReviews[0]?.overall_rating || 0,
        overall_review:
          overallReviews[0]?.overall_review || "No overall review",
        created_at: overallReviews[0]?.created_at,
        user_id: overallReviews[0]?.user_id?._id,
        reputation_score: overallReviews.length
          ? reviewRatingDetailsOverall[0].reputation_score
          : 0,
        review_rating_details_overall: reviewRatingDetailsOverall,
        category_wise_review_rating: categoryWiseReviewList,
      },
    });
  } catch (error) {
    console.error("Error fetching reviews:", error.message);
    return res.status(500).json({
      review_rating_fetch_rs: { status: responses.error.failedFetchReview },
      error: error.message,
    });
  }
};
 
