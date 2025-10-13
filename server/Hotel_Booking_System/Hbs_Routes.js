import express from "express";
import { processGuestBookingInfo, invokeBEData } from "./Hbs_Controller.js";

const router = express.Router();

router.post("/hbs", processGuestBookingInfo);
router.post("/invokeBEData", invokeBEData);

export default router;
