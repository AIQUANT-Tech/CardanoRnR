import express from 'express';
import { createReply, fetchReviewReplyThread, getReplies } from './replyTransDataController.js'
// import { verifyToken, allowBusinessUser, allowEndUser } from '../auth/jwtUtils.js';

const router = express.Router();

// Route to create a reply
/**
 * @swagger
 * /api/reply/ReplyToReviews:
 *   post:
 *     summary: Reply to a review
 *     tags: [Review Replies]
 */
router.post('/ReplyToReviews', createReply);

//fetch reply
/**
 * @swagger
 * /api/reply/ReplyToReview:
 *   post:
 *     summary: Fetch full review reply thread
 *     tags: [Review Replies]
 */
router.post('/ReplyToReview', fetchReviewReplyThread);

//fetch main reply
/**
 * @swagger
 * /api/reply/:
 *   post:
 *     summary: Fetch main replies
 *     tags: [Review Replies]
 */
router.post('/', getReplies);

export default router;
