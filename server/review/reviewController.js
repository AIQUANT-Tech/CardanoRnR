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
export const getReviewById = async (req, res) => {
    try {
        const {
            id
        } = req.params;

        const review = await Review.findById({
            _id: id
        })
        // .populate('review_list.category_id');
        if (!review) {
            return res.status(404).json({
                success: false,
                message: 'Review not found'
            });
        }
        const data = {
            user_id: review.user_id,
            review_list: review.review_list
        };

        res.status(200).json({
            success: true,
            data
        });
    } catch (error) {
        res.status(500).json({
            success: false,
            message: 'Failed to fetch review',
            error: error.message
        });
    }
};