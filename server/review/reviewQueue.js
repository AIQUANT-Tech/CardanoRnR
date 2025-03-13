import Queue from "bull";
import {
  redeemFunds,
  scriptAddress,
} from "../cardano_transaction/cardanoLucid.js";
import { waitForUTxOWithTimeout } from "./reviewController.js";
import { Data } from "lucid-cardano";
import Review from "./Reviews.js";
import dotenv from "dotenv";
dotenv.config();

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
    serializedReviewDatum,
    serializedReviewRedeemer,
    lockTxHash,
  } = job.data;
  try {
    // Convert the serialized hex strings back to Constr objects.
    const reviewDatum = Data.from(serializedReviewDatum);
    const reviewRedeemer = Data.from(serializedReviewRedeemer);

    console.log("Background job: Waiting for UTxO with txHash:", lockTxHash);
    await waitForUTxOWithTimeout(
      scriptAddress,
      reviewDatum,
      lockTxHash,
      600000,
      10000
    );
    console.log(
      "Background job: UTxO found. Redeeming review data on-chain..."
    );

    const { txHash: redeemTxHash, reputationScore } = await redeemFunds(
      reviewDatum,
      reviewRedeemer
    );
    // console.log("Background job: Review data redeemed, tx hash:", redeemTxHash);
    console.log("Background job: Updated Reputation Score:", reputationScore);

    // Update the review document. Convert redeemTxHash (and reputationScore if needed) to strings.
    await Review.findByIdAndUpdate(reviewId, {
      blockchain_tx: redeemTxHash.toString(),
      status: true,
      // Optionally, update reputationScore field if defined:
      // reputationScore: reputationScore.toString(),
    });
    done();
  } catch (error) {
    console.error("Background job error:", error.message);
    await Review.findByIdAndUpdate(reviewId, {
      error: error.message,
      status: false,
    });
    done(new Error(error));
  }
});
console.log("Bull worker is running and waiting for jobs...");

export default reviewQueue;
