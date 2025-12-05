import express from "express";
import {
  processUserMappingFeed,
  updateBookingStatusController
} from "./schedulerController.js";

const router = express.Router();

/**
 * @swagger
 * components:
 *   schemas:
 *     SchedulerRunResponse:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *           example: true
 *         message:
 *           type: string
 *           example: "User-mapping scheduler executed successfully"
 *         processed_records:
 *           type: number
 *           example: 120
 *
 *     UpdateBookingStatusRequest:
 *       type: object
 *       properties:
 *         booking_ids:
 *           type: array
 *           items:
 *             type: string
 *           description: List of booking IDs to update
 *           example: ["BK00123", "BK00456"]
 *         status:
 *           type: string
 *           description: New status to be applied
 *           example: "COMPLETED"
 *       required: [status]
 *
 *     UpdateBookingStatusResponse:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *         updated_count:
 *           type: number
 *         message:
 *           type: string
 *           example: "Booking statuses updated successfully"
 *
 *     SchedulerErrorResponse:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *           example: false
 *         message:
 *           type: string
 *         error:
 *           type: object
 */


/**
 * @swagger
 * tags:
 *   - name: Scheduler
 *     description: API endpoints to manually trigger backend schedulers and batch processors
 */



/**
 * @swagger
 * /api/scheduler/run:
 *   post:
 *     summary: Manually trigger the user-mapping scheduler
 *     description: Runs the scheduler responsible for mapping user, guest, and booking data. Useful for manual synchronization and testing.
 *     tags: [Scheduler]
 *     responses:
 *       200:
 *         description: Scheduler executed successfully
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/SchedulerRunResponse'
 *       500:
 *         description: Internal server error while executing scheduler
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/SchedulerErrorResponse'
 */
router.post("/run", processUserMappingFeed);



/**
 * @swagger
 * /api/scheduler/updateBookingStatus:
 *   post:
 *     summary: Update booking status for one or more bookings
 *     description: Executes batch processing to update statuses of pending or expired bookings.
 *     tags: [Scheduler]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/UpdateBookingStatusRequest'
 *     responses:
 *       200:
 *         description: Booking status updated successfully
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/UpdateBookingStatusResponse'
 *       400:
 *         description: Invalid or missing status value
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/SchedulerErrorResponse'
 *       500:
 *         description: Failed to update booking statuses
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/SchedulerErrorResponse'
 */
router.post("/updateBookingStatus", updateBookingStatusController);



export default router;
