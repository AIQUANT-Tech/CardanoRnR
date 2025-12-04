import Queue from "bull";
import {
  redeemFunds,
  scriptAddress,
} from "../cardano_transaction/cardanoLucid.js";
import { waitForUTxOWithTimeout } from "./reviewController.js";
import { Data } from "lucid-cardano";
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

// reviewQueue.process(async (job, done) => {
//   const {
//     reviewId,
//     serializedReviewDatum,
//     serializedReviewRedeemer,
//     lockTxHash,
//   } = job.data;
//   try {
//     // Convert the serialized hex strings back to Constr objects.
//     const reviewDatum = Data.from(serializedReviewDatum);
//     const reviewRedeemer = Data.from(serializedReviewRedeemer);

//     console.log("Background job: Waiting for UTxO with txHash:", lockTxHash);
//     await waitForUTxOWithTimeout(
//       scriptAddress,
//       reviewDatum,
//       lockTxHash,
//       600000,
//       10000
//     );
//     console.log(
//       "Background job: UTxO found. Redeeming review data on-chain..."
//     );

//     const { txHash: redeemTxHash, reputationScore } = await redeemFunds(
//       reviewDatum,
//       reviewRedeemer
//     );
//     // console.log("Background job: Review data redeemed, tx hash:", redeemTxHash);
//     console.log("Background job: Updated Reputation Score:", reputationScore);

//     // Update the review document. Convert redeemTxHash (and reputationScore if needed) to strings.
//     await Review.findByIdAndUpdate(reviewId, {
//       blockchain_tx: redeemTxHash.toString(),
//       status: true,
//       // Optionally, update reputationScore field if defined:
//       // reputationScore: reputationScore.toString(),
//     });
//     done();
//   } catch (error) {
//     console.error("Background job error:", error.message);
//     await Review.findByIdAndUpdate(reviewId, {
//       error: error.message,
//       status: false,
//     });
//     done(new Error(error));
//   }
// });

reviewQueue.process(async (job, done) => {
  const {
    lockTxHash,
    userId,
    overall_rating,
    overall_review,
    category_wise_review_rating,
    categoryIds,
    serializedReviewDatum,
    serializedReviewRedeemer,
  } = job.data;

  try {
    const reviewDatum = Data.from(serializedReviewDatum);
    const reviewRedeemer = Data.from(serializedReviewRedeemer);

    console.log("Waiting for UTxO:", lockTxHash);

    await waitForUTxOWithTimeout(
      scriptAddress,
      reviewDatum,
      lockTxHash,
      600000,
      10000
    );

    console.log("Redeeming review data...");

    const { txHash: redeemTxHash, reputationScore } = await redeemFunds(
      reviewDatum,
      reviewRedeemer
    );

    console.log("Redeemed with tx:", redeemTxHash);
    console.log("Reputation Score:", reputationScore);

    // ---- SAVE FINAL REVIEW IN MONGODB ----

    // Save main overall review
    const savedOverall = await Review.create({
      user_id: userId,
      category_id: null,
      overall_review,
      overall_rating,
      blockchain_tx: redeemTxHash,
      reputation_score: reputationScore?.toString() || null,
      status: true,
      created_at: new Date(),
    });

    // Save category-wise reviews
    const categoryReviews = category_wise_review_rating.map((catRev) => {
      const categoryDoc = validCategories.find(
        (cat) => cat.category_id === catRev.category_id
      );

      return {
        user_id: userId,
        category_id: categoryDoc._id, 
        review: catRev.review,
        rating: catRev.rating,
        overall_review,
        overall_rating,
        blockchain_tx: redeemTxHash,
        status: true,
        created_at: new Date(),
      };
    });


    await Review.insertMany(categoryReviews);

    console.log("All reviews successfully stored in DB.");

    done();
  } catch (error) {
    console.error("Worker error:", error.message);
    done(new Error(error.message));
  }
});

console.log("Bull worker is running and waiting for jobs...");

export default reviewQueue;
