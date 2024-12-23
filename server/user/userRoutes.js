import express from "express";
import { createUser, getAllUsers, loginUser } from "./userController.js";
import { verifyToken, allowBusinessUser, allowEndUser } from '../auth/jwtUtils.js';

const router = express.Router();

// Route to create a user
router.post("/users", createUser);

// Login users
router.post("/login", loginUser);

// Route to get all users
router.get("/users", verifyToken, allowBusinessUser, getAllUsers);

export default router;
