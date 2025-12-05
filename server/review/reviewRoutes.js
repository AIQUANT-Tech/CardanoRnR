import express from "express";
import {
  calculateReviewStats,
  createReview,
  getAllReviews,
  getReviewsForBusinessUser,
  getReviewsForEndUser,
  getUserReviews,
  getReputationScoreFromBlockchain,
  getReviewById,
} from "./reviewController.js";

const router = express.Router();

/**
 * @swagger
 * components:
 *   schemas:
 *     Review:
 *       type: object
 *       properties:
 *         review_id:
 *           type: string
 *         user_id:
 *           type: string
 *         business_id:
 *           type: string
 *         rating:
 *           type: number
 *           format: float
 *         comment:
 *           type: string
 *         created_at:
 *           type: string
 *           format: date-time
 *
 *     CreateReviewRequest:
 *       type: object
 *       required: [user_id, business_id, rating, comment]
 *       properties:
 *         user_id:
 *           type: string
 *         business_id:
 *           type: string
 *         rating:
 *           type: number
 *           example: 4.5
 *         comment:
 *           type: string
 *           example: "Excellent service and friendly staff"
 *
 *     GenericError:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *           example: false
 *         message:
 *           type: string
 *         error:
 *           type: object
 *
 *     ReviewStatsResponse:
 *       type: object
 *       properties:
 *         totalReviews:
 *           type: number
 *         averageRating:
 *           type: number
 *         positiveCount:
 *           type: number
 *         negativeCount:
 *           type: number
 *
 *     FetchReputationRequest:
 *       type: object
 *       properties:
 *         user_id:
 *           type: string
 *       required: [user_id]
 *
 *     FetchReputationResponse:
 *       type: object
 *       properties:
 *         user_id:
 *           type: string
 *         reputationScore:
 *           type: number
 */

/**
 * @swagger
 * tags:
 *   name: Reviews
 *   description: API endpoints related to user reviews, business reviews, and reputation score.
 */



// ---------------------------------------------
// CREATE REVIEW
// ---------------------------------------------
/**
 * @swagger
 * /api/review/CreateReview:
 *   post:
 *     summary: Create a new review
 *     description: Allows an end user to submit a review for a business. Requires user_id, business_id, rating, and comment.
 *     tags: [Reviews]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/CreateReviewRequest'
 *     responses:
 *       201:
 *         description: Review successfully created
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/Review'
 *       400:
 *         description: Invalid input data
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/GenericError'
 *       500:
 *         description: Server error while saving review
 */
router.post("/CreateReview", createReview);


// ---------------------------------------------
// GET ALL REVIEWS
// ---------------------------------------------
/**
 * @swagger
 * /api/review/reviews:
 *   get:
 *     summary: Retrieve all reviews
 *     description: Returns a complete list of all reviews stored in the system.
 *     tags: [Reviews]
 *     responses:
 *       200:
 *         description: List of all reviews
 *         content:
 *           application/json:
 *             schema:
 *               type: array
 *               items:
 *                 $ref: '#/components/schemas/Review'
 *       500:
 *         description: Failed to fetch reviews
 */
router.get("/reviews", getAllReviews);


// ---------------------------------------------
// GET REVIEW BY ID
// ---------------------------------------------
/**
 * @swagger
 * /api/review/reviews/{id}:
 *   get:
 *     summary: Get a review by ID
 *     description: Fetch a specific review using its unique identifier.
 *     tags: [Reviews]
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *         description: Review ID
 *         schema:
 *           type: string
 *     responses:
 *       200:
 *         description: Review found
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/Review'
 *       404:
 *         description: Review not found
 *       500:
 *         description: Error retrieving review
 */
router.get("/reviews/:id", getReviewById);


// ---------------------------------------------
// BUSINESS USER — FETCH REVIEWS
// ---------------------------------------------
/**
 * @swagger
 * /api/review/reviews/business/FetchReviews:
 *   post:
 *     summary: Get all reviews for a business user
 *     description: Fetches reviews written for the authenticated business user's listings.
 *     tags: [Reviews]
 *     requestBody:
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             properties:
 *               business_id:
 *                 type: string
 *     responses:
 *       200:
 *         description: Reviews for the business user
 *       400:
 *         description: Missing or invalid business_id
 *       500:
 *         description: Internal server error
 */
router.post("/reviews/business/FetchReviews", getReviewsForBusinessUser);


// ---------------------------------------------
// END USER — FETCH THEIR REVIEWS
// ---------------------------------------------
/**
 * @swagger
 * /api/review/reviews/user/FetchReviews:
 *   post:
 *     summary: Get all reviews submitted by an end user
 *     description: Fetches every review written by a specific user.
 *     tags: [Reviews]
 *     requestBody:
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             properties:
 *               user_id:
 *                 type: string
 *     responses:
 *       200:
 *         description: Reviews submitted by the user
 *       400:
 *         description: Missing or invalid user_id
 *       500:
 *         description: Internal server error
 */
router.post("/reviews/user/FetchReviews", getReviewsForEndUser);


// ---------------------------------------------
// END USER — GET SPECIFIC REVIEW
// ---------------------------------------------
/**
 * @swagger
 * /api/review/reviews/user/selected:
 *   post:
 *     summary: Get a specific review submitted by the user
 *     description: Returns details of one selected review based on user_id + review_id.
 *     tags: [Reviews]
 *     requestBody:
 *       content:
 *         application/json:
 *           schema:
 *             type: object
 *             required: [user_id, review_id]
 *             properties:
 *               user_id:
 *                 type: string
 *               review_id:
 *                 type: string
 *     responses:
 *       200:
 *         description: Review found
 *       404:
 *         description: Review not found
 *       500:
 *         description: Error occurred
 */
router.post("/reviews/user/selected", getUserReviews);


// ---------------------------------------------
// REVIEW STATS
// ---------------------------------------------
/**
 * @swagger
 * /api/review/count:
 *   get:
 *     summary: Get overall review statistics
 *     description: Returns total number of reviews, average rating, count of positive and negative reviews.
 *     tags: [Reviews]
 *     responses:
 *       200:
 *         description: Review statistics fetched successfully
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/ReviewStatsResponse'
 *       500:
 *         description: Error fetching statistics
 */
router.get("/count", calculateReviewStats);


// ---------------------------------------------
// BLOCKCHAIN — GET REPUTATION SCORE
// ---------------------------------------------
/**
 * @swagger
 * /api/review/fetchReputation:
 *   post:
 *     summary: Fetch on-chain reputation score
 *     description: Queries blockchain (Cardano / smart contract) to retrieve user's reputation score.
 *     tags: [Reviews]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/FetchReputationRequest'
 *     responses:
 *       200:
 *         description: Reputation score retrieved successfully
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/FetchReputationResponse'
 *       400:
 *         description: Missing or invalid user_id
 *       500:
 *         description: Error querying blockchain
 */
router.post("/fetchReputation", getReputationScoreFromBlockchain);

export default router;
