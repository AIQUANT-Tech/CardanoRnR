import express from 'express';
import { createReply, fetchReviewReplyThread, getReplies } from './replyTransDataController.js'
// import { verifyToken, allowBusinessUser, allowEndUser } from '../auth/jwtUtils.js';

const router = express.Router();

// Route to create a reply
router.post('/ReplyToReviews', createReply);

//fetch reply
router.post('/ReplyToReview', fetchReviewReplyThread);

//fetch main reply
router.post('/', getReplies);

export default router;
