import Review from "./Reviews.js";
import BookingInfo from "../Hotel_Booking_System/Hbs_Booking_Info_Schema.js";
import UserGuestMap from "../../server/user/UserGuestMap.js";
import User from "../user/UserMast.js";
import ReviewCategory from "../reviewCategory/ReviewCategories.js";
import responses from "../utils/responses.js";
import roles from "../utils/roles.js";
import reviewQueue from "./reviewQueue.js";
import { fetchLatestChainState as fetchStateV2 } from "../cardano_transaction/rnrContract.js";

import { Constr, Data, Lucid, Blockfrost, getAddressDetails, credentialToAddress, keyHashToCredential } from "@lucid-evolution/lucid";
import review from "./Reviews.js";
import { log } from "console";

 const lucid = await Lucid(
   new Blockfrost(
     "https://cardano-preprod.blockfrost.io/api/v0",
     process.env.BLOCKFROST_KEY,
   ),
   "Preprod",
 );
 lucid.selectWallet.fromSeed(process.env.MNEMONIC); 

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
      return matchingUtxo;
    }
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
      bookingId,
      overall_rating,
      overall_review,
      category_wise_review_rating,
    } = new_review_rating_create_rq;

    if (request_type !== "CREATE_NEW_REVIEW_RATING") {
      return res.status(400).json({ error: "Invalid request type" });
    }

    if (
      !user_email_id ||
      !bookingId ||
      overall_rating == null ||
      !overall_review ||
      !Array.isArray(category_wise_review_rating) ||
      category_wise_review_rating.length === 0
    ) {
      return res.status(400).json({ error: "All fields are required" });
    }

    // Retrieve user by email
    const user = await User.findOne({ email: user_email_id, status: true });
    if (!user) {
      return res.status(404).json({ error: "User not found" });
    }

    const booking = await BookingInfo.findOne({ booking_id: bookingId });

    if (!booking) {
      return res.status(404).json({ error: "Booking not found" });
    }

    if (booking.booking_status !== "Checkedout") {
      return res
        .status(403)
        .json({ error: "Only checked-out guests can submit reviews" });
    }

    // Validate categories
    const categoryIds = category_wise_review_rating.map(
      (catReview) => catReview.category_id,
    );
    const validCategories = await ReviewCategory.find({
      category_id: { $in: categoryIds },
    });
    if (validCategories.length !== categoryIds.length) {
      return res.status(404).json({ error: "Invalid category IDs" });
    }

    // Deterministic review id — unique per user per booking.
    const reviewId = Buffer.from(`${user._id}${booking._id}`).toString("hex");

    // Reject duplicate reviews for the same booking. This is also enforced at
    // the database level by a unique (sparse) index on Reviews.reviewId, so a
    // race cannot slip a second one through.
    const existingReview = await Review.findOne({ reviewId });
    if (existingReview) {
      return res
        .status(409)
        .json({ error: "A review has already been submitted for this booking" });
    }

    reviewQueue.add({
      reviewId,
      userId: user._id.toString(),
      bookingId,
      overall_rating,
      overall_review,
      category_wise_review_rating,
      validCategories,
    });

    return res.status(202).json({
      status: "processing",
      reviewId,
      message:
        "Review submitted. Blockchain processing started; the review will be stored once the transaction confirms.",
    });
  } catch (error) {
    console.error("Error creating review:", error.message);
    return res.status(500).json({ error: "Internal server error" });
  }
};

// Poll a review's on-chain status by its deterministic reviewId. Returns
// { status: "pending" } until the worker stores the review, then
// { status: "confirmed", blockchain_tx } once the review is on-chain.
export const getReviewStatus = async (req, res) => {
  try {
    const { reviewId } = req.params;
    const review = await Review.findOne({ reviewId }).select(
      "status blockchain_tx",
    );
    if (!review || review.status !== true || !review.blockchain_tx) {
      return res.status(200).json({ status: "pending" });
    }
    return res
      .status(200)
      .json({ status: "confirmed", blockchain_tx: review.blockchain_tx });
  } catch (error) {
    return res.status(500).json({ status: "error", message: error.message });
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

    if (!review_rating_info_rq) {
      return res.status(400).json({
        review_rating_info_rs: {
          review_rating_info_by_user: [],
          status: responses.validation.invalidRequest,
        },
      });
    }

    const {
      header: { product, request_type },
    } = review_rating_info_rq;

    if (product !== "rnr" || request_type !== "REVIEW_RATING_INFO") {
      return res.status(400).json({
        review_rating_info_rs: {
          review_rating_info_by_user: [],
          status: responses.validation.invalidRequest,
        },
      });
    }


    // Fetch only reviews with a valid blockchain_tx (NOT empty string)
    const reviews = await Review.find({
      status: true,
      blockchain_tx: { $ne: "" }, 
    })

      .populate("user_id", "display_name")
      .populate("category_id", "category_name")
      .select(
        "_id user_id category_id review rating overall_review overall_rating is_responded created_at blockchain_tx"
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

    // ----------------------------------------------
    // 2. Extract unique *Mongo User IDs*
    // ----------------------------------------------
    const mongoUserIds = [
      ...new Set(
        reviews.map((r) => r.user_id?._id?.toString()).filter(Boolean)
      ),
    ];


    // ----------------------------------------------
    // 3. Fetch UserGuestMap for these user IDs
    // ----------------------------------------------
    const userGuestMaps = await UserGuestMap.find({
      user_id: { $in: mongoUserIds },
    }).lean();


    // Map user Mongo ID → guest_id
    const guestIdByUserId = {};
    userGuestMaps.forEach((m) => {
      guestIdByUserId[m.user_id.toString()] = m.guest_id?.toString();
    });


    // ----------------------------------------------
    // 4. Extract valid guest IDs
    // ----------------------------------------------
    const guestIds = [
      ...new Set(
        Object.values(guestIdByUserId).filter(
          (id) => id && id.length === 24
        )
      ),
    ];


    // ----------------------------------------------
    // 5. Fetch bookings for guest IDs
    // ----------------------------------------------
    const bookings = await BookingInfo.find({
      guest_id: { $in: guestIds },
    })
      .select("room_type check_in_date check_out_date guest_id")
      .lean();


    const bookingByGuestId = {};
    bookings.forEach((b) => {
      bookingByGuestId[b.guest_id.toString()] = b;
    });

    // ----------------------------------------------
    // Helper: compute stay duration
    // ----------------------------------------------
    const computeTimeOfStay = (inDate, outDate) => {
      if (!inDate || !outDate) return "N/A";
      const diff = new Date(outDate) - new Date(inDate);
      const days = Math.ceil(diff / (1000 * 60 * 60 * 24));
      return `${days} ${days === 1 ? "day" : "days"}`;
    };

    // ----------------------------------------------
    // 6. Merge data into final response
    // ----------------------------------------------
    const reviewRatingInfoByUser = reviews.map((review) => {
      const mongoUserId = review.user_id?._id?.toString();
      const guestId = guestIdByUserId[mongoUserId];
      const bookingDetails =
        guestId && bookingByGuestId[guestId]
          ? bookingByGuestId[guestId]
          : null;

      const timeOfStay =
        bookingDetails &&
        bookingDetails.check_in_date &&
        bookingDetails.check_out_date
          ? computeTimeOfStay(
              bookingDetails.check_in_date,
              bookingDetails.check_out_date
            )
          : "N/A";

      return {
        review_id: review._id.toString(),
        user_id: mongoUserId,
        user_display_name: review.user_id?.display_name || "Unknown User",
        category_id: review.category_id?._id?.toString() || null,
        category_name:
          review.category_id?.category_name || "Overall Rating",
        review_responded: !!review.is_responded,
        review: review.review || review.overall_review,
        rating:
          review.rating?.toString() ||
          review.overall_rating?.toString(),
        created_at: review.created_at,
        booking_details: bookingDetails,
        time_of_stay: timeOfStay,
        blockchain_tx: review.blockchain_tx?.toString(),
      };
    });

    // ----------------------------------------------
    // 7. Return response
    // ----------------------------------------------
    return res.status(200).json({
      review_rating_info_rs: {
        review_rating_info_by_user: reviewRatingInfoByUser,
        status: responses.success.success,
      },
    });

  } catch (error) {
    console.error("🔥 ERROR:", error.message);

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
      status: "Active", // ✅ only active categories
    }).select("category_id category_name category_description status");


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
        review_id: review._id, // ⭐ REQUIRED
        review: review.review,
        rating: review.rating,
      });

      return categories;
    }, {});

    const categoryWiseReviewList = Object.values(categoryWiseReviews);

   return res.status(200).json({
     status: "success",
     data: {
       overall_review_id: reviews[0]._id, // ⭐ REQUIRED
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
  // Reputation score is global — stored in the state UTxO identified by the STT.
  // Read it from the current (single-tx design) state contract. Return a plain
  // Number (0-100) so it JSON-serializes — fetchStateV2 returns BigInts.
  const { reputationScore } = await fetchStateV2();
  return Number(reputationScore);
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
    const reviews = await Review.find({
      status: true,
      blockchain_tx: { $ne: "" }, // NOT empty string
    })
      .select(
        "_id user_id overall_review overall_rating review rating category_id created_at blockchain_tx"
      )
      .populate("user_id", "display_name");



    // Filter for overall reviews (assumed to be those without a category_id)
    const overallReviews = reviews.filter((r) => !r.category_id);
    let blockchainReputationScore = 0;

    // Sort overall reviews by creation date (latest first)
    overallReviews.sort(
      (a, b) => new Date(b.created_at) - new Date(a.created_at)
    );

    const lastUserId =
      overallReviews.length > 0 ? overallReviews[0].user_id._id : null;

    if (overallReviews.length > 0) {
      blockchainReputationScore = await fetchReputationScore(lastUserId);
    }

    // For each overall review, fetch its own blockchain reputation score and booking details
    const reviewRatingDetailsOverall = await Promise.all(
      overallReviews.map(async (r) => {
        const userId = r.user_id._id;

        let bookingDetailsForThisUser = null;
        const userGuestMapping = await UserGuestMap.findOne({
          user_id: userId.toString(),
        });


        if (userGuestMapping && userGuestMapping.guest_id) {
          
         bookingDetailsForThisUser = await BookingInfo.findOne({
           _id: userGuestMapping.booking_id,
         }).select("room_type check_in_date check_out_date");

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
          blockchain_tx:r.blockchain_tx
        };
      })
    );

    // Category-wise review details (unchanged)
    const categoryIds = [...new Set(reviews.map((r) => r.category_id))];
    const categories = await ReviewCategory.find({
      _id: { $in: categoryIds },
      status: "Active",
    }).select("category_id category_name category_description");


    
    const categoryDetailsMap = categories.reduce((map, category) => {
      map[category._id.toString()] = {
        category_name: category.category_name,
        category_desc: category.category_description
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
        // blockchain_tx: overallReviews[0]?.blockchain_tx || "",
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
