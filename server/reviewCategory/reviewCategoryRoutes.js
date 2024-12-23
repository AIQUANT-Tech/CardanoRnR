import express from 'express';
import {
    createReviewCategory, getAllReviewCategories, editReviewCategory,
    deleteReviewCategory,
} from './reviewCategoryController.js';
import { verifyToken, allowBusinessUser } from '../auth/jwtUtils.js';

const router = express.Router();

// Route for creating a new review category
router.post('/createReviewCategory', createReviewCategory);

// Route for getting all review categories
router.post('/getReviewCategoryInfo', getAllReviewCategories);

// Update
router.put('/editReviewCategory', editReviewCategory);

// delete
router.delete('/deleteReviewCategory', deleteReviewCategory);




// // Route for creating a new review category
// router.post('/review-categories', createReviewCategory);

// // Route for getting all review categories
// router.get('/review-categories', getAllReviewCategories);

// // Update
// router.put('/review-categories', editReviewCategory);

// // delete
// router.delete('/review-categories' ,deleteReviewCategory);

export default router;
