import express from 'express';
import {
    createReviewCategory, getAllReviewCategories, editReviewCategory,
    deleteReviewCategory,
} from './reviewCategoryController.js';
import { verifyToken, allowBusinessUser } from '../auth/jwtUtils.js';

const router = express.Router();

// Route for creating a new review category
/**
 * @swagger
 * /api/reviewcategory/createReviewCategory:
 *   post:
 *     summary: Create a review category
 *     tags: [Review Category]
 */

router.post('/createReviewCategory', createReviewCategory);

// Route for getting all review categories
/**
 * @swagger
 * /api/reviewcategory/getReviewCategoryInfo:
 *   post:
 *     summary: Get review category details
 *     tags: [Review Category]
 */
router.post('/getReviewCategoryInfo', getAllReviewCategories);

// Update
/**
 * @swagger
 * /api/reviewcategory/editReviewCategory:
 *   put:
 *     summary: Edit a review category
 *     tags: [Review Category]
 */
router.put('/editReviewCategory', editReviewCategory);

// delete
/**
 * @swagger
 * /api/reviewcategory/deleteReviewCategory:
 *   delete:
 *     summary: Delete a review category
 *     tags: [Review Category]
 */
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
