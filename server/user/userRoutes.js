import express from "express";
import { createUser, getAllUsers, loginUser } from "./userController.js";
import { verifyToken, allowBusinessUser, allowEndUser } from '../auth/jwtUtils.js';

const router = express.Router();

// Route to create a user
router.post("/users",allowEndUser, createUser);

// Login users
router.get("/login", allowEndUser, loginUser);

// Route to get all users
router.get("/users", verifyToken, allowBusinessUser, getAllUsers);

export default router;
