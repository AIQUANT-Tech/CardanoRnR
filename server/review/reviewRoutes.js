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
import {
  verifyToken,
  allowBusinessUser,
  allowEndUser,
} from "../auth/jwtUtils.js";

const router = express.Router();

// Route to create a review
/**
 * @swagger
 * /api/review/CreateReview:
 *   post:
 *     summary: Create a review
 *     tags: [Reviews]
 */
router.post("/CreateReview", createReview);

// Route to get all reviews
/**
 * @swagger
 * /api/review/reviews:
 *   get:
 *     summary: Get all reviews
 *     tags: [Reviews]
 */
router.get("/reviews", getAllReviews);

// Get a review by ID
/**
 * @swagger
 * /api/review/reviews/{id}:
 *   get:
 *     summary: Get review by ID
 *     tags: [Reviews]
 *     parameters:
 *       - in: path
 *         name: id
 *         required: true
 *     responses:
 *       200:
 *         description: Review found
 */
router.get("/reviews/:id", getReviewById);

//Get all review - business user
/**
 * @swagger
 * /api/review/reviews/business/FetchReviews:
 *   post:
 *     summary: Get all reviews for business user
 *     tags: [Reviews]
 */
router.post("/reviews/business/FetchReviews", getReviewsForBusinessUser);

//Get all review - end user
/**
 * @swagger
 * /api/review/reviews/user/FetchReviews:
 *   post:
 *     summary: Get all reviews for end user
 *     tags: [Reviews]
 */
router.post("/reviews/user/FetchReviews", getReviewsForEndUser);

//Get a user review - end user
/**
 * @swagger
 * /api/review/reviews/user/selected:
 *   post:
 *     summary: Get a user review for end user
 *     tags: [Reviews]
 */
router.post("/reviews/user/selected", getUserReviews);

//Get the total reviews
/**
 * @swagger
 * /api/review/count:
 *   get:
 *     summary: Get review statistics
 *     tags: [Reviews]
 */
router.get("/count", calculateReviewStats);

/**
 * @swagger
 * /api/review/fetchReputation:
 *   post:
 *     summary: Fetch reputation score from blockchain
 *     tags: [Reviews]
 */
router.post("/fetchReputation", getReputationScoreFromBlockchain);

export default router;
