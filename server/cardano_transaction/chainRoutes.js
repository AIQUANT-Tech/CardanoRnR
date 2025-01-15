import express from "express";
import { createTransaction } from "./index.js";

const router = express.Router();

// Route to create a user
router.post("/createTransaction", createTransaction);


export default router;
