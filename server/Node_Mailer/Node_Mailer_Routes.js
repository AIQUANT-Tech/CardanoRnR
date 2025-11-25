import  express from 'express';
import { SendRnREmail } from './Node_Mailer_Controller.js';

const router = express.Router();

/**
 * @swagger
 * /api/emails/sendmail:
 *   post:
 *     summary: Send hotel RnR email
 *     tags: [Email]
 */
router.post('/sendmail', SendRnREmail);

export default router;
