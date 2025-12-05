import express from 'express';
import { SendRnREmail } from './Node_Mailer_Controller.js';

const router = express.Router();

/**
 * @swagger
 * components:
 *   schemas:
 *     SendEmailRequest:
 *       type: object
 *       properties:
 *         to:
 *           type: string
 *           description: Recipient email address
 *           example: "customer@example.com"
 *         subject:
 *           type: string
 *           description: Email subject line
 *           example: "Your Hotel Review & Rating Details"
 *         template_id:
 *           type: string
 *           description: Email template identifier (if using dynamic templates)
 *           example: "HBS_RNR_01"
 *         booking_id:
 *           type: string
 *           description: Unique booking ID reference
 *         user_name:
 *           type: string
 *           description: Name of the recipient
 *         email_body:
 *           type: string
 *           description: Custom message or dynamic content for the email
 *           example: "Thank you for your stay! Please rate your experience."
 *       required: [to, subject]
 *
 *     SendEmailResponse:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *           example: true
 *         message:
 *           type: string
 *           example: "Email sent successfully"
 *         message_id:
 *           type: string
 *           example: "<randommsgid@mailer>"
 *
 *     EmailErrorResponse:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *           example: false
 *         message:
 *           type: string
 *           example: "Failed to send email"
 *         error:
 *           type: string
 */


/**
 * @swagger
 * tags:
 *   - name: Email
 *     description: Endpoints for sending transactional and notification emails
 */


/**
 * @swagger
 * /api/emails/sendmail:
 *   post:
 *     summary: Send Hotel Review & Rating (RnR) Email
 *     description: Sends a transactional email to users with booking details, review invitation, or custom message using NodeMailer.
 *     tags: [Email]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/SendEmailRequest'
 *     responses:
 *       200:
 *         description: Email successfully sent
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/SendEmailResponse'
 *       400:
 *         description: Invalid email fields or missing required parameters
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/EmailErrorResponse'
 *       500:
 *         description: Internal server error while sending email
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/EmailErrorResponse'
 */
router.post('/sendmail', SendRnREmail);

export default router;
