import express from 'express';
import { createReviewCategory, getAllReviewCategories,updateReviewCategory,
    deleteReviewCategory, } from './reviewCategoryController.js';

const router = express.Router();

// Route for creating a new review category
router.post('/review-categories', createReviewCategory);

// Route for getting all review categories
router.get('/review-categories', getAllReviewCategories);

// Update
router.put('/review-categories/:id', updateReviewCategory);

// delete
router.delete('/review-categories/:id', deleteReviewCategory); 

export default router;
