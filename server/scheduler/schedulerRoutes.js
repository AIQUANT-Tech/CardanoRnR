import express from 'express';
import { processUserMappingFeed } from '../scheduler/schedulerController.js';

const router = express.Router();
/**
 * @swagger
 * /api/scheduler/run:
 *   post:
 *     summary: Run user-mapping scheduler manually
 *     tags: [Scheduler]
 *     responses:
 *       200:
 *         description: Scheduler executed successfully
 *       500:
 *         description: Internal server error
 */


router.post("/run", processUserMappingFeed);

export default router;
