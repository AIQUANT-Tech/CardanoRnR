import express from "express";
import {
  createUser,
  getAllUsers,
  loginUser,
  uploadUser,
  validateEndUser
} from "./userController.js";

import { verifyToken, allowBusinessUser } from "../auth/jwtUtils.js";

const router = express.Router();

/**
 * @swagger
 * components:
 *   securitySchemes:
 *     bearerAuth:
 *       type: http
 *       scheme: bearer
 *       bearerFormat: JWT
 *
 *   schemas:
 *     CreateUserRequest:
 *       type: object
 *       properties:
 *         name:
 *           type: string
 *           example: "Arpan Maitra"
 *         email:
 *           type: string
 *           example: "arpan@example.com"
 *         phone:
 *           type: string
 *           example: "+91-9876543210"
 *         password:
 *           type: string
 *           example: "StrongPassword123"
 *         user_type:
 *           type: string
 *           enum: [END_USER, BUSINESS_USER]
 *       required: [email, password, user_type]
 *
 *     CreateUserResponse:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *           example: true
 *         message:
 *           type: string
 *           example: "User created successfully"
 *         user_id:
 *           type: string
 *
 *     LoginRequest:
 *       type: object
 *       properties:
 *         email:
 *           type: string
 *           example: "arpan@example.com"
 *         password:
 *           type: string
 *           example: "StrongPassword123"
 *       required: [email, password]
 *
 *     LoginResponse:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *         token:
 *           type: string
 *         user:
 *           type: object
 *           properties:
 *             user_id:
 *               type: string
 *             name:
 *               type: string
 *             email:
 *               type: string
 *             user_type:
 *               type: string
 *
 *     ValidateUserRequest:
 *       type: object
 *       properties:
 *         user_id:
 *           type: string
 *         document_number:
 *           type: string
 *           example: "A1234567"
 *         otp:
 *           type: string
 *           example: "123456"
 *       required: [user_id, otp]
 *
 *     ValidateUserResponse:
 *       type: object
 *       properties:
 *         success:
 *           type: boolean
 *         message:
 *           type: string
 *
 *     UploadUserRequest:
 *       type: object
 *       properties:
 *         users:
 *           type: array
 *           items:
 *             type: object
 *             properties:
 *               name:
 *                 type: string
 *               email:
 *                 type: string
 *               phone:
 *                 type: string
 *               role:
 *                 type: string
 *                 example: "STAFF"
 *             required: [email]
 *       required: [users]
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
 *   - name: Users
 *     description: User authentication, identity validation, business-user operations
 */



/**
 * @swagger
 * /api/user/users:
 *   post:
 *     summary: Create a new user account
 *     description: Registers a new end user or business user.
 *     tags: [Users]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/CreateUserRequest'
 *     responses:
 *       201:
 *         description: User created successfully
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/CreateUserResponse'
 *       400:
 *         description: Missing fields or invalid input
 *       500:
 *         description: Server error while creating user
 */
router.post("/users", createUser);



/**
 * @swagger
 * /api/user/login:
 *   post:
 *     summary: Authenticate a user and generate a JWT token
 *     tags: [Users]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/LoginRequest'
 *     responses:
 *       200:
 *         description: Login successful
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/LoginResponse'
 *       401:
 *         description: Invalid credentials
 *       500:
 *         description: Internal login error
 */
router.post("/login", loginUser);



/**
 * @swagger
 * /api/user/users:
 *   get:
 *     summary: Retrieve all registered users
 *     tags: [Users]
 *     responses:
 *       200:
 *         description: List of users returned successfully
 *       500:
 *         description: Error fetching users
 */
router.get("/users", getAllUsers);



/**
 * @swagger
 * /api/user/validate:
 *   post:
 *     summary: Validate end-user identity (OTP, document verification, etc.)
 *     tags: [Users]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/ValidateUserRequest'
 *     responses:
 *       200:
 *         description: Validation successful
 *         content:
 *           application/json:
 *             schema:
 *               $ref: '#/components/schemas/ValidateUserResponse'
 *       400:
 *         description: Invalid OTP or incorrect verification details
 *       500:
 *         description: Server error
 */
router.post("/validate", validateEndUser);



/**
 * @swagger
 * /api/user/uploadUser:
 *   post:
 *     summary: Upload HBS user list (Business User Only)
 *     description: Allows a business user to import multiple HBS staff users into the system.
 *     security:
 *       - bearerAuth: []
 *     tags: [Users]
 *     requestBody:
 *       required: true
 *       content:
 *         application/json:
 *           schema:
 *             $ref: '#/components/schemas/UploadUserRequest'
 *     responses:
 *       200:
 *         description: Users uploaded successfully
 *       403:
 *         description: Forbidden — Only business users can perform this action
 *       500:
 *         description: Upload failed due to server error
 */
router.post("/uploadUser", verifyToken, allowBusinessUser, uploadUser);



export default router;
