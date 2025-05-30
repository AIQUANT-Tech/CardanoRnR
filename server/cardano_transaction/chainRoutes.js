import express from "express";
import { fetchRedeemers } from "./index.js";
import { lockFundsController, redeemFundsController, TxDetails } from "./cardanoLucid.js";

const router = express.Router();

// // Route to create a transaction
// router.post("/createTransaction", createTransaction);

// // Route to get metadata
// router.post("/getTransactionMetadata",getTransactionMetadata);

// Route to get redeemer
router.post("/fetchRedeemers",fetchRedeemers);


// // Route to get redeemer
// router.post("/demo",demo);

router.post("/lockFunds",lockFundsController);

router.post("/redeemFunds",redeemFundsController);

router.post("/getTxDetails", TxDetails);


export default router;
