import Review from '../models/Reviews.js'; // Adjust the path if needed

// Create a new review
export const createReview = async (req, res) => {
    try {
        const {
            user_id,
            overall_content,
            overall_rating,
            review_list,
            blockchain_tx
        } = req.body;

        // Validate required fields
        if (!overall_content || !overall_rating || !user_id || !review_list) {
            return res.status(400).json({
                message: 'All fields are required.'
            });
        }

        // Create a new review
        const newReview = new Review({
            user_id,
            overall_content,
            overall_rating,
            review_list,
            blockchain_tx
        });
        const savedReview = await newReview.save();

        return res.status(201).json({
            message: 'Review created successfully.',
            review: savedReview
        });
    } catch (error) {
        console.error(error);
        return res.status(500).json({
            message: 'Error creating review.',
            error: error.message
        });
    }
};

// Get all reviews
export const getAllReviews = async (req, res) => {
    try {
        // Fetch all reviews with populated user and category fields
        const reviews = await Review.find()
        //.populate('user', 'name email') // Adjust based on your user model's fields
        //.populate('category'); // Adjust based on your category model's fields

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