import express from "express";
import { createUser, getAllUsers, loginUser, uploadUser, validateEndUser } from "./userController.js";
import { verifyToken, allowBusinessUser, allowEndUser } from '../auth/jwtUtils.js';

const router = express.Router();

// Route to create a user
router.post("/users", createUser);

// Login users
router.post("/login", loginUser);

// Route to get all users
router.get("/users", getAllUsers);

//Route to validate end user
router.post("/validate", validateEndUser);

//Route to validate end user
// router.post("/verify", verifyEndUserOTP);

//Route to input all HBS users
router.post("/uploadUser",verifyToken, allowBusinessUser, uploadUser);

export default router;
