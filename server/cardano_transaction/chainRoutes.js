import express from "express";
import { fetchRedeemers } from "./index.js";
import { lockFundsController, redeemFundsController, TxDetails } from "./cardanoLucid.js";

const router = express.Router();

// // Route to create a transaction
// router.post("/createTransaction", createTransaction);

// // Route to get metadata
// router.post("/getTransactionMetadata",getTransactionMetadata);

// Route to get redeemer
/**
 * @swagger
 * /api/transaction/fetchRedeemers:
 *   post:
 *     summary: Fetch redeemers from Cardano blockchain
 *     tags: [Cardano]
 */
router.post("/fetchRedeemers",fetchRedeemers);


// // Route to get redeemer
// router.post("/demo",demo);

/**
 * @swagger
 * /api/transaction/lockFunds:
 *   post:
 *     summary: Lock funds using Cardano Lucid
 *     tags: [Cardano]
 */
router.post("/lockFunds",lockFundsController);
/**
 * @swagger
 * /api/transaction/redeemFunds:
 *   post:
 *     summary: Redeem funds using Cardano Lucid
 *     tags: [Cardano]
 */
router.post("/redeemFunds",redeemFundsController);
/**
 * @swagger
 * /api/transaction/getTxDetails:
 *   post:
 *     summary: Get Cardano transaction details
 *     tags: [Cardano]
 */
router.post("/getTxDetails", TxDetails);


export default router;
