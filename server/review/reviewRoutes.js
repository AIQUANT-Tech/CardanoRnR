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
router.post("/CreateReview", createReview);

// Route to get all reviews
router.get("/reviews", getAllReviews);

// Get a review by ID
router.get("/reviews/:id", getReviewById);

//Get all review - business user
router.post("/reviews/business/FetchReviews", getReviewsForBusinessUser);

//Get all review - end user
router.post("/reviews/user/FetchReviews", getReviewsForEndUser);

//Get a user review - end user
router.post("/reviews/user/selected", getUserReviews);

//Get the total reviews
router.get("/count", calculateReviewStats);

// router.post("/fetchReputation", getReputationScoreFromBlockchain);

export default router;
