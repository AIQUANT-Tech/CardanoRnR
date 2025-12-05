import express from "express";
import { fetchRedeemers } from "./index.js";
import { lockFundsController, redeemFundsController, TxDetails } from "./cardanoLucid.js";

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
 *     LockFundsRequest:
 *       type: object
 *       properties:
 *         senderAddress:
 *           type: string
 *           description: User's Cardano address that will lock the funds
 *         amount:
 *           type: number
 *           description: Amount to lock in Lovelace
 *         metadata:
 *           type: object
 *           description: Optional metadata to embed in transaction
 *       required: [senderAddress, amount]
 *
 *     LockFundsResponse:
 *       type: object
 *       properties:
 *         txHash:
 *           type: string
 *         status:
 *           type: string
 *           example: "locked"
 *
 *     RedeemFundsRequest:
 *       type: object
 *       properties:
 *         txHash:
 *           type: string
 *           description: Transaction hash of locked funds
 *         redeemer:
 *           type: object
 *           description: Redeemer data required to unlock funds
 *       required: [txHash, redeemer]
 *
 *     RedeemFundsResponse:
 *       type: object
 *       properties:
 *         txHash:
 *           type: string
 *         status:
 *           type: string
 *           example: "redeemed"
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
 * /api/transaction/lockFunds:
 *   post:
 *     summary: Lock funds on the Cardano blockchain
 *     description: Creates a script-based UTxO that locks ADA using Cardano Lucid. Useful for escrow, dApps, or staking workflows.
 *     tags: [Cardano]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/LockFundsRequest'
 *     responses:
 *       200:
 *         description: Funds successfully locked
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/LockFundsResponse'
 *       400:
 *         description: Missing senderAddress or invalid amount
 *       500:
 *         description: Cardano transaction construction/signing error
 */
router.post("/lockFunds", lockFundsController);



/**
 * @swagger
 * /api/transaction/redeemFunds:
 *   post:
 *     summary: Redeem funds locked in a Cardano script
 *     description: Unlocks previously locked UTxOs using redeemer data and Cardano Lucid.
 *     tags: [Cardano]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/RedeemFundsRequest'
 *     responses:
 *       200:
 *         description: Funds successfully redeemed
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/RedeemFundsResponse'
 *       400:
 *         description: Missing txHash or redeemer
 *       500:
 *         description: Redeeming failed due to invalid redeemer or script mismatch
 */
router.post("/redeemFunds", redeemFundsController);



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
