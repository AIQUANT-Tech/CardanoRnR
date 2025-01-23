import express from "express";
import { lockFundsWithDatum, redeemFundsWithRedeemer } from "./lucidindex.js";

const router = express.Router();

// POST route to submit the transaction
router.post("/lock-funds", lockFundsWithDatum);
router.post("/redeem-funds", redeemFundsWithRedeemer);

export default router;
