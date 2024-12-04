import express from 'express';
import { createReview, getAllReviews } from '../controllers/reviewController.js';

const router = express.Router();

// Route to create a review
router.post('/reviews', createReview);

// Route to get all reviews
router.get('/reviews', getAllReviews);

export default router;
