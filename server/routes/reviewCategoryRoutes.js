import express from 'express';
import { createReviewCategory, getAllReviewCategories } from '../controllers/reviewCategoryController.js';

const router = express.Router();

// Route for creating a new review category
router.post('/review-categories', createReviewCategory);

// Route for getting all review categories
router.get('/review-categories', getAllReviewCategories);

export default router;
