import express from "express";
import { createUser, getAllUsers, loginUser, uploadUser, validateEndUser } from "./userController.js";
import { verifyToken, allowBusinessUser, allowEndUser } from '../auth/jwtUtils.js';

const router = express.Router();

/**
 * @swagger
 * /api/user/users:
 *   post:
 *     summary: Create a new user
 *     tags: [Users]
 *     responses:
 *       201:
 *         description: User created successfully
 */



// Route to create a user
router.post("/users", createUser);

/**
 * @swagger
 * /api/user/login:
 *   post:
 *     summary: Authenticate user
 *     tags: [Users]
 *     responses:
 *       200:
 *         description: Login successful
 */

// Login users
router.post("/login", loginUser);

// Route to get all users
/**
 * @swagger
 * /api/user/users:
 *   get:
 *     summary: Get all users
 *     tags: [Users]
 *     responses:
 *       200:
 *         description: List of users returned
 */

router.get("/users", getAllUsers);

//Route to validate end user
/**
 * @swagger
 * /api/user/validate:
 *   post:
 *     summary: Validate end user identity
 *     tags: [Users]
 *     responses:
 *       200:
 *         description: Validation successful
 */
router.post("/validate", validateEndUser);

//Route to validate end user
// router.post("/verify", verifyEndUserOTP);

//Route to input all HBS users
/**
 * @swagger
 * /api/user/uploadUser:
 *   post:
 *     summary: Upload HBS user data (Business User Only)
 *     security:
 *       - bearerAuth: []
 *     tags: [Users]
 *     responses:
 *       200:
 *         description: Users uploaded successfully
 */
router.post("/uploadUser",verifyToken, allowBusinessUser, uploadUser);

export default router;
