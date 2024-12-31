import Review from './Reviews.js';
import User from "../user/UserMast.js";
import ReviewCategory from '../reviewCategory/ReviewCategories.js';
import responses from '../utils/responses.js';
import roles from '../utils/roles.js';
export const createReview = async (req, res) => {
  try {
    const { new_review_rating_create_rq } = req.body;

    if (!new_review_rating_create_rq) {
      return res.status(400).json({ new_review_rating_create_rs: { status: responses.validation.invalidRequest } });
    }

    const {
      header: { request_type, user_name },
      user_email_id,
      overall_rating,
      overall_review,
      category_wise_review_rating,
    } = new_review_rating_create_rq;

    if (request_type !== "CREATE_NEW_REVIEW_RATING") {
      return res.status(400).json({ new_review_rating_create_rs: { status: responses.validation.invalidRequest } });
    }

    if (
      !user_email_id ||
      !overall_rating ||
      !overall_review ||
      !Array.isArray(category_wise_review_rating) ||
      category_wise_review_rating.length === 0
    ) {
      return res
        .status(400)
        .json({ new_review_rating_create_rs: { status: responses.validation.allFieldsRequired } });
    }

    // Check if the user exists and is active
    const user = await User.findOne({ email: user_email_id, status: true });

    if (!user) {
      return res.status(404).json({ new_review_rating_create_rs: { status: responses.validation.NotFound } });
    }

    //   if(presentReview){
    //     return res.status(404).json({ message: "User already Reviewed!! "});
    //   }

    // Extract all category IDs from category_wise_review_rating
    const categoryIds = category_wise_review_rating.map(
      (catReview) => catReview.category_id
    );

    // Fetch all valid categories
    const validCategories = await ReviewCategory.find({
      category_id: { $in: categoryIds }
    });

    if (validCategories.length !== categoryIds.length) {
      return res
        .status(404)
        .json({ new_review_rating_create_rs: { status: responses.validation.NotFound } });
    }

    // Prepare review documents for insertion
    const user_id = user._id;
    const reviewsToInsert = category_wise_review_rating.map((categoryReview) => {
      const category = validCategories.find(
        (cat) => cat.category_id === categoryReview.category_id.toString()
      );

      if (!category) {
        throw new Error(
          `Category with ID ${categoryReview.category_id} not found.`
        );
      }

      return {
        user_id,
        category_id: category._id,
        review: categoryReview.review,
        rating: categoryReview.rating,
        overall_review,
        overall_rating,
        blockchain_tx: " ",
        status: true,
      };
    });

    const overallReview = new Review({
      user_id,
      category_id: null,
      overall_review,
      overall_rating,
      blockchain_tx: " ",
      status: true,
    });
    const savedOverall = await overallReview.save();

    // Insert all reviews
    const savedReviews = await Review.insertMany(reviewsToInsert);

    return res.status(201).json({
      new_review_rating_create_rs: { status: responses.success.success },
      reviews: savedReviews,
      overall: savedOverall
    });
  } catch (error) {
    console.error("Error creating reviews:", error.message);
    return res.status(500).json({
      new_review_rating_create_rs: { status: responses.error.createReview },
      error: error.message,
    });
  }
};



// Get all reviews
export const getAllReviews = async (req, res) => {
  try {
    // Fetch all reviews with populated user and category fields
    const reviews = await Review.find()
    //.populate('user', 'name email') 
    //.populate('category'); 

    return res.status(200).json(reviews);
  } catch (error) {
    console.error(error);
    return res.status(500).json({
      new_review_rating_create_rs: { status: responses.error.retrieveReview },
      error: error.message
    });
  }
};

// Get a review by ID
// export const getReviewById = async (req, res) => {
//     try {
//         const {
//             id
//         } = req.params;

//         const review = await Review.findById({
//             _id: id
//         })
//         // .populate('review_list.category_id');
//         if (!review) {
//             return res.status(404).json({
//                 success: false,
//                 message: 'Review not found'
//             });
//         }
//         const data = {
//             user_id: review.user_id,
//             review_list: review.review_list
//         };

//         res.status(200).json({
//             success: true,
//             data
//         });
//     } catch (error) {
//         res.status(500).json({
//             success: false,
//             message: 'Failed to fetch review',
//             error: error.message
//         });
//     }
// };

// Get all reviews - Business user
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

    // Query all reviews and populate relevant details
    const reviews = await Review.find()
      .populate("user_id", "display_name")
      .populate("category_id", "category_name")
      .select("_id user_id category_id review rating overall_review overall_rating is_responded");

    // Check if no reviews are found
    if (!reviews || reviews.length === 0) {
      return res.status(404).json({
        review_rating_info_rs: {
          review_rating_info_by_user: [],
          status: responses.validation.NoReview,
        },
      });
    }

    // Format the reviews as per the API specification
    const reviewRatingInfoByUser = reviews.map((review) => ({
      review_id: review._id?.toString(),
      user_id: review.user_id?._id?.toString() || null,
      user_display_name: review.user_id?.display_name || "Unknown User",
      category_id: review.category_id?._id?.toString() || null,
      review_responded: !!review.is_responded, 
      review: review.review || review.overall_review,
      rating: review.rating?.toString() || review.overall_rating?.toString(),
    }));

    // Return the response
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


//Get all reviews- end user
export const getReviewsForEndUser = async (req, res) => {
  try {
    const { review_rating_fetch_rq } = req.body;

    if (!review_rating_fetch_rq) {
      return res.status(400).json({ review_rating_fetch_rs: { status: responses.validation.invalidRequest } });
    }

    const {
      header: { request_type },
    } = review_rating_fetch_rq;

    if (request_type !== "FETCH_REVIEW_RATING") {
      return res.status(400).json({ review_rating_fetch_rs: { status: responses.validation.invalidRequest } });
    }

    const reviews = await Review.find({ status: "true", })
    .select("_id user_id overall_review overall_rating review rating category_id created_at")
    .populate("user_id", "display_name");    

    if (!reviews || reviews.length === 0) {
      return res.status(404).json({ review_rating_fetch_rs: { status: responses.validation.reviewNotFound } });
    }

    const validOverallRatings = reviews
      .map((r) => r.overall_rating)
      .filter((rating) => typeof rating === "number" && !isNaN(rating));

    const totalOverallRating = validOverallRatings.length > 0
      ? validOverallRatings.reduce((sum, overall_rating) => sum + overall_rating, 0) / validOverallRatings.length
      : 0.0;


    const reputationScore =
      validOverallRatings.length > 0
        ? validOverallRatings.reduce((sum, rating) => sum + rating, 0) / validOverallRatings.length
        : 0.0;

    const reviewRatingDetailsOverall = reviews
      .filter((r) => !r.category_id)
      .map((r) => ({
        review_id: r._id,
        user_name: r.user_id?.display_name || "Anonymous",
        user_id: r.user_id?._id,
        created_at: r.created_at,
        review: r.overall_review,
        rating: r.overall_rating,
      }));

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
        overall_rating: totalOverallRating,
        overall_review: reviews[0]?.overall_review || "No overall review",
        created_at: reviews[0]?.created_at,
        user_id: reviews[0]?.user_id?._id,
        reputation_score: reputationScore,
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

export const getUserReviews = async (req, res) => {
  try {
    const { user_id } = req.body;

    if (!user_id) {
      return res.status(400).json({
        status: "fail",
        message: "User ID is required",
      });
    }

    // Fetch the user to ensure the user exists
    const user = await User.findById(user_id);

    if (!user) {
      return res.status(404).json({
        status: "fail",
        message: "User not found",
      });
    }

    // Fetch all reviews for the user
    const reviews = await Review.find({ user_id: user_id, status: "true" }).select(
      "overall_review overall_rating review rating category_id created_at"
    );

    if (!reviews || reviews.length === 0) {
      return res.status(404).json({
        status: "fail",
        message: "No reviews found for the user",
      });
    }

    // Calculate overall rating
    const validOverallRatings = reviews
      .map((r) => r.overall_rating)
      .filter((rating) => typeof rating === "number" && !isNaN(rating));

    const totalOverallRating =
      validOverallRatings.length > 0
        ? validOverallRatings.reduce((sum, rating) => sum + rating, 0) /
          validOverallRatings.length
        : 0.0;

    // Fetch category details for reviews
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

    // Organize category-wise review ratings
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