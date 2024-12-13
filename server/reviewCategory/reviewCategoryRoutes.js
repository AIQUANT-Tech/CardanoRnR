import express from 'express';
import {
    createReviewCategory, getAllReviewCategories, editReviewCategory,
    deleteReviewCategory,
} from './reviewCategoryController.js';
import { verifyToken, allowBusinessUser } from '../auth/jwtUtils.js';

const router = express.Router();

// Route for creating a new review category
router.post('/review-categories',verifyToken, allowBusinessUser, createReviewCategory);

// Route for getting all review categories
router.get('/review-categories',verifyToken, allowBusinessUser, getAllReviewCategories);

// Update
router.put('/review-categories/:id',verifyToken, allowBusinessUser, editReviewCategory);

// delete
router.delete('/review-categories/:id',verifyToken, allowBusinessUser, deleteReviewCategory);




// // Route for creating a new review category
// router.post('/review-categories', createReviewCategory);

// // Route for getting all review categories
// router.get('/review-categories', getAllReviewCategories);

// // Update
// router.put('/review-categories', editReviewCategory);

// // delete
// router.delete('/review-categories' ,deleteReviewCategory);

export default router;
