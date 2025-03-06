import express from "express";
import { processGuestBookingInfo } from "./Hbs_Controller.js";

const router = express.Router();

router.post("/hbs", processGuestBookingInfo);

export default router;
