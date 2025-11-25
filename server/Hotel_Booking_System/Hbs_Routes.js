import express from "express";
import { processGuestBookingInfo, invokeBEData, bookingEngineData } from "./Hbs_Controller.js";

const router = express.Router();

/**
 * @swagger
 * /api/hotel_booking_system/hbs:
 *   post:
 *     summary: Process guest booking info
 *     tags: [Hotel Booking System]
 */
router.post("/hbs", processGuestBookingInfo);
/**
 * @swagger
 * /api/hotel_booking_system/invokeBEData:
 *   post:
 *     summary: Invoke booking engine data
 *     tags: [Hotel Booking System]
 */
router.post("/invokeBEData", invokeBEData);
/**
 * @swagger
 * /api/hotel_booking_system/bookingEngineData:
 *   post:
 *     summary: Booking engine data
 *     tags: [Hotel Booking System]
 */
router.post("/bookingEngineData", bookingEngineData);

export default router;
