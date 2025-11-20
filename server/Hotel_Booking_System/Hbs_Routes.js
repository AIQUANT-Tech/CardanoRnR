import express from "express";
import { processGuestBookingInfo, invokeBEData, bookingEngineData } from "./Hbs_Controller.js";

const router = express.Router();

router.post("/hbs", processGuestBookingInfo);
router.post("/invokeBEData", invokeBEData);
router.post("/bookingEngineData", bookingEngineData);

export default router;
