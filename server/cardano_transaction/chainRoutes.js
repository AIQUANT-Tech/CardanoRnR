import express from "express";
import { fetchRedeemers } from "./index.js";
import { TxDetails } from "./txDetails.js";

const router = express.Router();

/**
 * @swagger
 * components:
 *   schemas:
 *     FetchRedeemersRequest:
 *       type: object
 *       properties:
 *         txHash:
 *           type: string
 *           example: "9ab3c...f9d"
 *       required: [txHash]
 *
 *     FetchRedeemersResponse:
 *       type: object
 *       properties:
 *         txHash:
 *           type: string
 *         redeemers:
 *           type: array
 *           items:
 *             type: object
 *             properties:
 *               purpose:
 *                 type: string
 *                 example: "spend"
 *               data:
 *                 type: string
 *                 example: "some-redeemer-data"
 *
 *     TxDetailsRequest:
 *       type: object
 *       properties:
 *         txHash:
 *           type: string
 *           description: Cardano transaction hash
 *       required: [txHash]
 *
 *     TxDetailsResponse:
 *       type: object
 *       properties:
 *         txHash:
 *           type: string
 *         inputs:
 *           type: array
 *           items:
 *             type: string
 *         outputs:
 *           type: array
 *           items:
 *             type: string
 *         metadata:
 *           type: object
 *
 *     GenericError:
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
 *   name: Cardano
 *   description: Cardano blockchain transaction and redeemer APIs
 */


/**
 * @swagger
 * /api/transaction/fetchRedeemers:
 *   post:
 *     summary: Fetch redeemers from the Cardano blockchain
 *     description: Retrieves redeemer data for a given transaction hash by analyzing the on-chain transaction.
 *     tags: [Cardano]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/FetchRedeemersRequest'
 *     responses:
 *       200:
 *         description: Redeemers successfully fetched
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/FetchRedeemersResponse'
 *       400:
 *         description: Missing or invalid txHash
 *       500:
 *         description: Internal blockchain fetch error
 */
router.post("/fetchRedeemers", fetchRedeemers);



/**
 * @swagger
 * /api/transaction/getTxDetails:
 *   post:
 *     summary: Get detailed Cardano transaction information
 *     description: Retrieves inputs, outputs, metadata, and other data for a specific transaction hash.
 *     tags: [Cardano]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/TxDetailsRequest'
 *     responses:
 *       200:
 *         description: Transaction details fetched successfully
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/TxDetailsResponse'
 *       400:
 *         description: Missing or invalid txHash
 *       500:
 *         description: Unable to query transaction from blockchain
 */
router.post("/getTxDetails", TxDetails);


export default router;
