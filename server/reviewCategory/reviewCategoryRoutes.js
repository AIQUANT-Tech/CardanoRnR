import express from 'express';
import {
    createReviewCategory,
    getAllReviewCategories,
    editReviewCategory,
    deleteReviewCategory,
    getActiveReviewCategories
} from './reviewCategoryController.js';

const router = express.Router();

/**
 * @swagger
 * components:
 *   schemas:
 *     ReviewCategory:
 *       type: object
 *       properties:
 *         category_id:
 *           type: string
 *         category_name:
 *           type: string
 *         category_description:
 *           type: string
 *         is_active:
 *           type: boolean
 *           example: true
 *
 *     CreateReviewCategoryRequest:
 *       type: object
 *       properties:
 *         category_name:
 *           type: string
 *           example: "Cleanliness"
 *         category_description:
 *           type: string
 *           example: "Rate the cleanliness of hotel rooms and premises"
 *       required: [category_name]
 *
 *     EditReviewCategoryRequest:
 *       type: object
 *       properties:
 *         category_id:
 *           type: string
 *         category_name:
 *           type: string
 *         category_description:
 *           type: string
 *         is_active:
 *           type: boolean
 *       required: [category_id]
 *
 *     DeleteReviewCategoryRequest:
 *       type: object
 *       properties:
 *         category_id:
 *           type: string
 *       required: [category_id]
 *
 *     ReviewCategoryResponse:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *         message:
 *           type: string
 *         data:
 *           $ref: '#/components/schemas/ReviewCategory'
 *
 *     ReviewCategoryListResponse:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *         categories:
 *           type: array
 *           items:
 *             $ref: '#/components/schemas/ReviewCategory'
 *
 *     ReviewCategoryError:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *           example: false
 *         message:
 *           type: string
 *         error:
 *           type: object
 */


/**
 * @swagger
 * tags:
 *   - name: Review Category
 *     description: CRUD operations for hotel review categories
 */



/**
 * @swagger
 * /api/reviewcategory/createReviewCategory:
 *   post:
 *     summary: Create a new review category
 *     description: Adds a new review category used for rating hotels (e.g., Cleanliness, Staff Behavior, Amenities).
 *     tags: [Review Category]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/CreateReviewCategoryRequest'
 *     responses:
 *       201:
 *         description: Review category created successfully
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/ReviewCategoryResponse'
 *       400:
 *         description: Invalid input or missing category name
 *       500:
 *         description: Server error while creating category
 */
router.post('/createReviewCategory', createReviewCategory);



/**
 * @swagger
 * /api/reviewcategory/getReviewCategoryInfo:
 *   post:
 *     summary: Get all review categories
 *     description: Retrieves a list of all review categories along with descriptions and active status.
 *     tags: [Review Category]
 *     responses:
 *       200:
 *         description: List of review categories retrieved
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/ReviewCategoryListResponse'
 *       500:
 *         description: Error fetching categories
 */
router.post('/getReviewCategoryInfo', getAllReviewCategories);


router.post("/getReviewCategoryInfoOfActive", getActiveReviewCategories);


/**
 * @swagger
 * /api/reviewcategory/editReviewCategory:
 *   put:
 *     summary: Edit an existing review category
 *     description: Modify the name, description, or status of an existing category.
 *     tags: [Review Category]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/EditReviewCategoryRequest'
 *     responses:
 *       200:
 *         description: Category updated successfully
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/ReviewCategoryResponse'
 *       400:
 *         description: Missing category_id or invalid update fields
 *       404:
 *         description: Category not found
 *       500:
 *         description: Server error while updating category
 */
router.put('/editReviewCategory', editReviewCategory);



/**
 * @swagger
 * /api/reviewcategory/deleteReviewCategory:
 *   delete:
 *     summary: Delete a review category
 *     description: Removes a review category from the system using category_id.
 *     tags: [Review Category]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/DeleteReviewCategoryRequest'
 *     responses:
 *       200:
 *         description: Category deleted successfully
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/ReviewCategoryResponse'
 *       400:
 *         description: Missing category_id
 *       404:
 *         description: Category not found
 *       500:
 *         description: Server error while deleting category
 */
router.post('/deleteReviewCategory', deleteReviewCategory);



export default router;
