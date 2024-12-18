import express from 'express';
import { createReview, getAllReviews, getReviewsForBusinessUser, getReviewsForEndUser } from './reviewController.js';
import { verifyToken, allowBusinessUser, allowEndUser } from '../auth/jwtUtils.js';

const router = express.Router();

// Route to create a review
router.post('/CreateReview', verifyToken, allowEndUser, createReview);

// Route to get all reviews
router.get('/reviews', getAllReviews);

// Get a review by ID
// router.get('/reviews/:id', getReviewById);

//Get all review - business user
router.get('/reviews/business/FetchReviews', verifyToken, allowBusinessUser, getReviewsForBusinessUser);

//Get all review - end user
router.get('/reviews/user/FetchReviews', verifyToken, allowEndUser, getReviewsForEndUser);


export default router;
