import mongoose from "mongoose";

const Reviews = new mongoose.Schema({
  user_id: {
    type: mongoose.Schema.Types.ObjectId,
    ref: "User",
    required: true,
  },
  overall_review: {
    type: String,
    required: true,
    description: "OverallReview content",
  },
  overall_rating: {
    type: Number,
    required: true,
    min: 1,
    max: 5,
    description: "Rating given by the user (1-5 scale) overall",
  },
  category_id: {
    type: mongoose.Schema.Types.ObjectId,
    ref: "ReviewCategory",
    // required: true,
    default: null,
  },
  review: {
    type: String,
    // required: true,
    description: "Review content",
    default: "",
  },
  rating: {
    type: Number,
    // required: true,
    min: 1,
    max: 5,
    description: "Rating given by the user (1-5 scale)",
  },
  created_at: {
    type: Date,
    default: Date.now,
    description: "Review submission timestamp",
  },
  is_responded: {
    type: Boolean,
    default: false,
    description: "Response given or pending",
  },
  booking_id: {
    type: String,
    default: null,
    description: "HBS booking reference ID",
  },
  reputation_score: {
    type: String,
    default: null,
    description: "Reputation score at time of review",
  },
  blockchain_tx: {
    type: String,
    default: "",
    description: "Blockchain transaction ID",
  },
  // Deterministic id = hex(userId + booking._id). Set only on the overall review
  // document; the unique (sparse) index below makes a second review for the same
  // booking impossible at the database level.
  reviewId: {
    type: String,
    default: undefined,
    description: "Unique review id (per user per booking)",
  },
  status: {
    type: Boolean,
    default: true,
  },
});

// DB-level guarantee: one overall review per (user, booking). reviewId is only
// set on the overall document, so the sparse index ignores category rows.
Reviews.index({ reviewId: 1 }, { unique: true, sparse: true });

const review = new mongoose.model("Review", Reviews);
export default review;
