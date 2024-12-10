import express from 'express';
import {
    createReviewCategory, getAllReviewCategories, updateReviewCategory,
    deleteReviewCategory,
} from './reviewCategoryController.js';
import { verifyToken } from '../auth/jwtUtils.js';

const router = express.Router();

// Route for creating a new review category
router.post('/review-categories',verifyToken, createReviewCategory);

// Route for getting all review categories
router.get('/review-categories',verifyToken, getAllReviewCategories);

// Update
router.put('/review-categories/:id',verifyToken, updateReviewCategory);

// delete
router.delete('/review-categories/:id',verifyToken ,deleteReviewCategory);

export default router;
