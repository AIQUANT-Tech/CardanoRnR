import express from 'express';
import { createReview, getAllReviews, getReviewsForBusinessUser, getReviewsForEndUser } from './reviewController.js';
import { verifyToken } from '../auth/jwtUtils.js';

const router = express.Router();

// Route to create a review
router.post('/reviews', createReview);

// Route to get all reviews
router.get('/reviews', getAllReviews);

// Get a review by ID
// router.get('/reviews/:id', getReviewById);

//Get all review - business user
router.get('/reviews/business', getReviewsForBusinessUser);

//Get all review - end user
router.get('/reviews/user', getReviewsForEndUser);


export default router;
