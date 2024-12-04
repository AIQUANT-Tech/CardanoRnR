import express from "express";
import { createUser, getAllUsers } from "../controllers/userController.js";

const router = express.Router();

// Route to create a user
router.post("/users", createUser);

// Route to get all users
router.get("/users", getAllUsers);

export default router;
