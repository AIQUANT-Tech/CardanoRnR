import express from "express";
import { createTransaction, fetchRedeemers, getTransactionMetadata } from "./index.js";

const router = express.Router();

// Route to create a transaction
router.post("/createTransaction", createTransaction);

// Route to get metadata
router.post("/getTransactionMetadata",getTransactionMetadata);

// Route to get redeemer
router.post("/fetchRedeemers",fetchRedeemers);


// Route to get redeemer
// router.post("/demo",demo);


export default router;
