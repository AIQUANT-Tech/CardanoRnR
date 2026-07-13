import Queue from "bull";
import { submitReview } from "../cardano_transaction/rnrContract.js";
import Review from "./Reviews.js";
import dotenv from "dotenv";
import mongoose from "mongoose";
dotenv.config();


mongoose
  .connect(process.env.DB_CNN, {
    useNewUrlParser: true,
    useUnifiedTopology: true,
  })
  .then(() => console.log("Worker connected to MongoDB"))
  .catch((err) => console.error("Worker MongoDB connection error:", err));


const reviewQueue = new Queue("reviewQueue", {
  redis: {
    host: process.env.REDIS_HOST,
    port: Number(process.env.REDIS_PORT),
    maxRetriesPerRequest:
      process.env.REDIS_MAX_RETRIES_PER_REQUEST === "null"
        ? null
        : Number(process.env.REDIS_MAX_RETRIES_PER_REQUEST),
  },
});

reviewQueue.process(async (job, done) => {
  const {
    reviewId,
    userId,
    bookingId,
    overall_rating,
    overall_review,
    category_wise_review_rating,
    validCategories,
  } = job.data;

  try {
    const alreadyProcessed = await Review.findOne({ reviewId });
    if (alreadyProcessed) {
      console.log(
        `Duplicate review ${reviewId} already processed — skipping on-chain submit.`
      );
      return done();
    }

    // One on-chain transaction: spend the current state UTxO and produce the
    // updated one. Running totals are recomputed on-chain from the old state.
    const { txHash, reputationScore } = await submitReview(
      reviewId,
      bookingId,
      overall_rating
    );

    // 1️⃣ Overall review — carries the unique reviewId + booking_id.
    await Review.create({
      user_id: userId,
      category_id: null,
      overall_review,
      overall_rating,
      booking_id: bookingId,
      reviewId,
      blockchain_tx: txHash,
      reputation_score: reputationScore?.toString() || null,
      status: true,
      created_at: new Date(),
    });

    // 2️⃣ Category reviews.
    const categoryReviews = category_wise_review_rating.map((catRev) => {
      const categoryDoc = validCategories.find(
        (cat) => cat.category_id === catRev.category_id
      );

      return {
        user_id: userId,
        category_id: categoryDoc?._id,
        review: catRev.review,
        rating: catRev.rating,
        overall_review,
        overall_rating,
        booking_id: bookingId,
        blockchain_tx: txHash,
        status: true,
        created_at: new Date(),
      };
    });

    await Review.insertMany(categoryReviews);

    console.log("Review stored successfully. tx:", txHash);
    done();
  } catch (error) {
    console.error("Worker error:", error.message);
    done(new Error(error.message));
  }
});


console.log("Bull worker is running and waiting for jobs...");

export default reviewQueue;
