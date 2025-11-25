import express from "express";
import { processUserMappingFeed } from "./schedulerController.js";

const router = express.Router();

router.post("/run", processUserMappingFeed);

export default router;
