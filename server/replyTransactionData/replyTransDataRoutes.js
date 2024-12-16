import express from 'express';
import { createReply } from './replyTransDataController.js'
// import { verifyToken, allowBusinessUser, allowEndUser } from '../auth/jwtUtils.js';

const router = express.Router();

// Route to create a reply
router.post('/ReplyToReviews', createReply);

export default router;
