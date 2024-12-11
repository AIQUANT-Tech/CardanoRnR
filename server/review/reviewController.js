import Review from './Reviews.js'; 
import User from "../user/UserMast.js";
import ReviewCategory from '../reviewCatagory/ReviewCategories.js';

export const createReview = async (req, res) => {
    try {
      const { new_review_rating_create_rq } = req.body;
  
      if (!new_review_rating_create_rq) {
        return res.status(400).json({ message: "Invalid request format" });
      }
  
      const {
        header: { request_type, user_name },
        user_email_id,
        overall_rating,
        overall_review,
        category_wise_review_rating,
      } = new_review_rating_create_rq;
  
      if (request_type !== "CREATE_NEW_REVIEW_RATING") {
        return res.status(400).json({ message: "Invalid request type" });
      }
  
      if (
        !user_email_id ||
        !overall_rating ||
        !overall_review ||
        !Array.isArray(category_wise_review_rating) ||
        category_wise_review_rating.length === 0
      ) {
        return res.status(400).json({ message: "Missing or invalid input fields." });
      }
  
      const user = await User.findOne({ email: user_email_id, status: "Active" });
      const reviewCatagory = await ReviewCategory.findOne({ category_id: category_wise_review_rating.category_id });
  
      if (!user) {
        return res.status(404).json({ message: "User not found or inactive." });
      }
      if (!reviewCatagory) {
        return res.status(404).json({ message: "Catagory not found." });
      }
  
      const user_id = user._id;  
      const category_id = reviewCatagory._id;     
  
      const categoryReviews = category_wise_review_rating.map((categoryReview) => ({
        user_id,
        category_id,
        review: categoryReview.review,
        rating: categoryReview.rating,
        overall_review,
        overall_rating,
        blockchain_tx: " ", 
        status: "Active",
      }));
  
      const savedReviews = await Review.insertMany(categoryReviews);
  
      return res.status(201).json({
        message: "Reviews created successfully.",
        reviews: savedReviews,
      });
    } catch (error) {
      console.error("Error creating reviews:", error.message);
      return res.status(500).json({
        message: "Error creating reviews.",
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
            message: 'Error retrieving reviews.',
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

// Get all reviews- Business user
export const getReviewsForBusinessUser = async (req, res) => {
  try {
    const { review_rating_info_rq } = req.body;

    if (!review_rating_info_rq) {
      return res.status(400).json({ message: "Invalid request format." });
    }

    const {
      header: { user_name, product, request_type },
    } = review_rating_info_rq;

    if (request_type !== "REVIEW_RATING_INFO" || product !== "rnr") {
      return res.status(400).json({ message: "Invalid request type or product." });
    }

    // Query all reviews with populated user and category details
    const reviews = await Review.find()
      .populate("user_id", "display_name") //`display_name` exists in User schema
      .populate("category_id", "category_name") // `category_name` exists in Category schema
      .select("_id user_id category_id review rating is_responded");

    // If no reviews found
    if (!reviews || reviews.length === 0) {
      return res.status(404).json({ message: "No reviews found." });
    }

    // Map reviews to the required response format
    const reviewRatingInfo = reviews.map((review) => ({
      review_id: review._id.toString(),
      user_id: review.user_id?._id.toString() || null,
      user_display_name: review.user_id?.display_name || "Unknown User",
      category_id: review.category_id?._id.toString() || null,
      review_responded: !!review.is_responded,
      review: review.review,
      rating: review.rating?.toString() || "0",
    }));

    // Return the mapped data as response
    return res.status(200).json({
      review_rating_info_rs: {
        review_rating_info_by_user: reviewRatingInfo,
      },
    });
  } catch (error) {
    console.error("Error fetching reviews for business user:", error.message);
    return res.status(500).json({
      message: "Error fetching reviews.",
      error: error.message,
    });
  }
};

  //Get all reviews- end user
  export const getReviewsForEndUser = async (req, res) => {
    try {
      const { review_rating_fetch_rq } = req.body;
  
      if (!review_rating_fetch_rq) {
        return res.status(400).json({ message: "Invalid request format." });
      }
  
      const {
        header: { request_type, user_name },
      } = review_rating_fetch_rq;
  
      if (request_type !== "FETCH_REVIEW_RATING") {
        return res.status(400).json({ message: "Invalid request type." });
      }
  
      const reviews = await Review.find({ status: "Active" })
        .populate("category_id", "category_name category_description")
        .select("overall_review overall_rating review rating category_id");
  
      if (!reviews || reviews.length === 0) {
        return res.status(404).json({ message: "No reviews found." });
      }
  
      const overallRatings = reviews.map((r) => r.overall_rating);
      const reputationScore =
        overallRatings.reduce((sum, rating) => sum + rating, 0) /
        overallRatings.length;
  
      const reviewRatingDetailsOverall = reviews.map((r) => ({
        review: r.overall_review,
        rating: r.overall_rating,
      }));
  
      const categoryWiseReviewRating = reviews.reduce((categories, review) => {
        const categoryId = review.category_id?._id.toString();
        if (!categories[categoryId]) {
          categories[categoryId] = {
            category_name: review.category_id.category_name,
            category_desc: review.category_id.category_description,
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
          overall_rating: reputationScore.toFixed(1),
          overall_review: reviews[0]?.overall_review || "No overall review",
          reputation_score: reputationScore.toFixed(1),
          review_rating_details_overall: reviewRatingDetailsOverall,
          category_wise_review_rating: categoryWiseReviewList,
        },
      });
    } catch (error) {
      console.error("Error fetching reviews:", error.message);
      return res.status(500).json({
        message: "Error fetching reviews.",
        error: error.message,
      });
    }
  };