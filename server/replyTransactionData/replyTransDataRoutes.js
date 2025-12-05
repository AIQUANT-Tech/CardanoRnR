import express from 'express';
import { createReply, fetchReviewReplyThread, getReplies } from './replyTransDataController.js';

const router = express.Router();

/**
 * @swagger
 * components:
 *   schemas:
 *     CreateReplyRequest:
 *       type: object
 *       properties:
 *         review_id:
 *           type: string
 *           description: ID of the review being replied to
 *         parent_reply_id:
 *           type: string
 *           nullable: true
 *           description: If replying to another reply, provide that reply ID
 *         user_id:
 *           type: string
 *         message:
 *           type: string
 *           description: Reply text body
 *       required: [review_id, user_id, message]
 *
 *     CreateReplyResponse:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *         reply_id:
 *           type: string
 *         message:
 *           type: string
 *           example: "Reply added successfully"
 *
 *     FetchReplyThreadRequest:
 *       type: object
 *       properties:
 *         review_id:
 *           type: string
 *       required: [review_id]
 *
 *     FetchReplyThreadResponse:
 *       type: object
 *       properties:
 *         review_id:
 *           type: string
 *         replies:
 *           type: array
 *           items:
 *             type: object
 *             properties:
 *               reply_id:
 *                 type: string
 *               parent_reply_id:
 *                 type: string
 *                 nullable: true
 *               user_id:
 *                 type: string
 *               message:
 *                 type: string
 *               created_at:
 *                 type: string
 *                 format: date-time
 *
 *     GetMainRepliesRequest:
 *       type: object
 *       properties:
 *         review_id:
 *           type: string
 *       required: [review_id]
 *
 *     GetMainRepliesResponse:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *         replies:
 *           type: array
 *           items:
 *             type: object
 *             properties:
 *               reply_id:
 *                 type: string
 *               user_id:
 *                 type: string
 *               message:
 *                 type: string
 *               created_at:
 *                 type: string
 *                 format: date-time
 *
 *     ReplyErrorResponse:
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
 *   - name: Review Replies
 *     description: Endpoints for replying to reviews and fetching reply threads
 */



/**
 * @swagger
 * /api/reply/ReplyToReviews:
 *   post:
 *     summary: Reply to a review or another reply
 *     description: Creates a new reply for a review. Supports both top-level replies and nested threaded replies.
 *     tags: [Review Replies]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/CreateReplyRequest'
 *     responses:
 *       200:
 *         description: Reply created successfully
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/CreateReplyResponse'
 *       400:
 *         description: Missing required fields or invalid review ID
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/ReplyErrorResponse'
 *       500:
 *         description: Internal server error while creating reply
 */
router.post('/ReplyToReviews', createReply);



/**
 * @swagger
 * /api/reply/ReplyToReview:
 *   post:
 *     summary: Fetch full reply thread for a review
 *     description: Returns all replies (nested threaded structure) associated with a review.
 *     tags: [Review Replies]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/FetchReplyThreadRequest'
 *     responses:
 *       200:
 *         description: Full reply thread fetched successfully
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/FetchReplyThreadResponse'
 *       404:
 *         description: Review or replies not found
 *       500:
 *         description: Server error while fetching reply thread
 */
router.post('/ReplyToReview', fetchReviewReplyThread);



/**
 * @swagger
 * /api/reply/:
 *   post:
 *     summary: Fetch main (top-level) replies for a review
 *     description: Returns only first-level replies (non-nested) associated with a review.
 *     tags: [Review Replies]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/GetMainRepliesRequest'
 *     responses:
 *       200:
 *         description: Main replies fetched successfully
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/GetMainRepliesResponse'
 *       404:
 *         description: No replies found
 *       500:
 *         description: Internal server error
 */
router.post('/', getReplies);



export default router;
