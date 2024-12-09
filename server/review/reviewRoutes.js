import express from 'express';
import { createReview, getAllReviews,getReviewById } from './reviewController.js';

const router = express.Router();

// Route to create a review
router.post('/reviews', createReview);

// Route to get all reviews
router.get('/reviews', getAllReviews);

// Get a review by ID
router.get('/reviews/:id', getReviewById);

export default router;
