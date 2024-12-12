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
        return res
          .status(400)
          .json({ message: "Missing or invalid input fields." });
      }
  
      // Check if the user exists and is active
      const user = await User.findOne({ email: user_email_id, status: "Active" });
      if (!user) {
        return res.status(404).json({ message: "User not found or inactive." });
      }
  
      // Extract all category IDs from category_wise_review_rating
      const categoryIds = category_wise_review_rating.map(
        (catReview) => catReview.category_id
      );
  
      // Fetch all valid categories
      const validCategories = await ReviewCategory.find({
        category_id: { $in: categoryIds },
        status: "Active",
      });
  
      if (validCategories.length !== categoryIds.length) {
        return res
          .status(404)
          .json({ message: "One or more categories not found or inactive." });
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
          status: "Active",
        };
      });
  
      // Insert all reviews
      const savedReviews = await Review.insertMany(reviewsToInsert);
  
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
            header: { request_type, user_name },
        } = review_rating_info_rq;

        if (request_type !== "REVIEW_RATING_INFO") {
            return res.status(400).json({ message: "Invalid request type." });
        }

        const reviews = await Review.find()
            .populate("user_id", "display_name")
            .populate("category_id", "category_name")
            .select(
                "_id user_id category_id review is_responded rating overall_review status"
            );

        console.log(reviews);


        const reviewRatingInfo = reviews.map((review) => ({
            review_id: review._id.toString(),
            user_id: review.user_id._id.toString(),
            user_display_name: review.user_id.display_name,
            category_id: review.category_id ? review.category_id._id.toString() : null,
            review_responded: review.is_responded,
            review: review.overall_review,
            rating: review.rating,
        }));

        return res.status(200).json({
            review_rating_info_rs: {
                review_rating_info_by_user: reviewRatingInfo,
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

//Get all reviews- end user
export const getReviewsForEndUser = async (req, res) => {
    try {
        const { review_rating_fetch_rq } = req.body;

        if (!review_rating_fetch_rq) {
            return res.status(400).json({ message: "Invalid request format." });
        }

        const {
            header: { request_type },
        } = review_rating_fetch_rq;

        if (request_type !== "FETCH_REVIEW_RATING") {
            return res.status(400).json({ message: "Invalid request type." });
        }

        const reviews = await Review.find({ status: "Active" }).select(
            "overall_review overall_rating review rating category_id"
        );

        if (!reviews || reviews.length === 0) {
            return res.status(404).json({ message: "No reviews found." });
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

        const reviewRatingDetailsOverall = reviews.map((r) => ({
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
                reputation_score: reputationScore,
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
