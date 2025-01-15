import express from "express";
import { createTransaction, getTransactionMetadata } from "./index.js";

const router = express.Router();

// Route to create a user
router.post("/createTransaction", createTransaction);

// Route to get metadata
router.post("/getTransactionMetadata",getTransactionMetadata);


export default router;
