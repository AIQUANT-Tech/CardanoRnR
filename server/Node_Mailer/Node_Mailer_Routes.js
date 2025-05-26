import  express from 'express';
import { SendRnREmail } from './Node_Mailer_Controller.js';

const router = express.Router();


router.post('/sendmail', SendRnREmail);

export default router;
