import express from "express";
import { processGuestBookingInfo, invokeBEData, bookingEngineData } from "./Hbs_Controller.js";

const router = express.Router();

/**
 * @swagger
 * components:
 *   schemas:
 *     GuestBookingRequest:
 *       type: object
 *       properties:
 *         guest_name:
 *           type: string
 *         guest_email:
 *           type: string
 *         phone_number:
 *           type: string
 *         check_in_date:
 *           type: string
 *           format: date
 *         check_out_date:
 *           type: string
 *           format: date
 *         room_type:
 *           type: string
 *         total_guests:
 *           type: number
 *         booking_source:
 *           type: string
 *           example: "web"          
 *       required: [guest_name, guest_email, check_in_date, check_out_date, room_type]
 *
 *     GuestBookingResponse:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *         message:
 *           type: string
 *         booking_reference:
 *           type: string
 *
 *     InvokeBEDataRequest:
 *       type: object
 *       properties:
 *         hotel_id:
 *           type: string
 *         search_params:
 *           type: object
 *           properties:
 *             check_in:
 *               type: string
 *             check_out:
 *               type: string
 *             adults:
 *               type: number
 *             children:
 *               type: number
 *       required: [hotel_id]
 *
 *     InvokeBEDataResponse:
 *       type: object
 *       properties:
 *         hotel_id:
 *           type: string
 *         rooms:
 *           type: array
 *           items:
 *             type: object
 *             properties:
 *               room_type:
 *                 type: string
 *               price:
 *                 type: number
 *               availability:
 *                 type: boolean
 *
 *     BookingEngineRequest:
 *       type: object
 *       properties:
 *         booking_id:
 *           type: string
 *         hotel_id:
 *           type: string
 *       required: [booking_id]
 *
 *     BookingEngineResponse:
 *       type: object
 *       properties:
 *         booking_id:
 *           type: string
 *         hotel_id:
 *           type: string
 *         status:
 *           type: string
 *           example: "confirmed"
 *         total_amount:
 *           type: number
 *
 *     GenericError:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *           example: false
 *         message:
 *           type: string
 *         details:
 *           type: object
 */


/**
 * @swagger
 * tags:
 *   - name: Hotel Booking System
 *     description: Endpoints for guest bookings, hotel engine data, and reservation management
 */



/**
 * @swagger
 * /api/hotel_booking_system/hbs:
 *   post:
 *     summary: Process guest booking information
 *     description: Accepts guest booking details and creates a booking record in the system.
 *     tags: [Hotel Booking System]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/GuestBookingRequest'
 *     responses:
 *       200:
 *         description: Booking processed successfully
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/GuestBookingResponse'
 *       400:
 *         description: Invalid input or missing guest fields
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/GenericError'
 *       500:
 *         description: Server error while processing booking
 */
router.post("/hbs", processGuestBookingInfo);


/**
 * @swagger
 * /api/hotel_booking_system/invokeBEData:
 *   post:
 *     summary: Fetch hotel booking engine data
 *     description: Retrieves real-time room availability, pricing, and policies from the booking engine.
 *     tags: [Hotel Booking System]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/InvokeBEDataRequest'
 *     responses:
 *       200:
 *         description: Booking engine data retrieved successfully
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/InvokeBEDataResponse'
 *       400:
 *         description: Missing hotel_id or invalid search parameters
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/GenericError'
 *       500:
 *         description: Failed to fetch booking engine data
 */
router.post("/invokeBEData", invokeBEData);


/**
 * @swagger
 * /api/hotel_booking_system/bookingEngineData:
 *   post:
 *     summary: Retrieve detailed booking engine reservation data
 *     description: Returns booking confirmation details, final pricing, and reservation metadata for a given booking.
 *     tags: [Hotel Booking System]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/BookingEngineRequest'
 *     responses:
 *       200:
 *         description: Booking engine reservation data fetched successfully
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/BookingEngineResponse'
 *       400:
 *         description: booking_id missing or invalid
 *       404:
 *         description: Booking not found
 *       500:
 *         description: Internal server error
 */
router.post("/bookingEngineData", bookingEngineData);



export default router;
