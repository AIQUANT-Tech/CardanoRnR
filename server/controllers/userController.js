import User from "../models/UserMast.js";
import bcrypt from 'bcryptjs';


// Create a new user
export const createUser = async (req, res) => {
    try {
        const { user_id, email, password_hash, display_name, role, status } = req.body;

        // Validate required fields
        if (!user_id || !display_name || !email || !password_hash || !role) {
            return res.status(400).json({ message: "All fields are required." });
        }

        // Ensure the role and status are valid
        if (!["End User", "Business User"].includes(role)) {
            return res.status(400).json({ message: "Invalid role provided." });
        }

        if (status && !["Active", "Inactive"].includes(status)) {
            return res.status(400).json({ message: "Invalid status provided." });
        }

        // Check if email is already registered
        const existingUser = await User.findOne({ email });
        if (existingUser) {
            return res.status(400).json({ message: "Email is already registered." });
        }

        const salt = await bcrypt.genSalt(10);
        const password = await bcrypt.hash(password_hash, salt);

        // Create the new user
        const newUser = new User({ user_id, email, password_hash: password, display_name, role, status });
        const savedUser = await newUser.save();

        return res.status(201).json({ message: "User created successfully.", user: savedUser });
    } catch (error) {
        console.error(error);
        return res.status(500).json({ message: "Error creating user.", error: error.message });
    }
};

// Login 
export const loginUser = async (req, res) => {
    try {
        const { email, password_hash } = req.body;

        if (!email || !password_hash) {
            return res.status(400).json({ message: "Email and password are required." });
        }

        const user = await User.findOne({ email });
        if (!user) {
            return res.status(400).json({ message: "Invalid credentials." });
        }

        const isMatch = await bcrypt.compare(password_hash, user.password_hash);
        if (!isMatch) {
            return res.status(400).json({ message: "Invalid credentials." });
        }
        return res.status(200).json({
            message: "Login successful",
            user: {
                user_id: user.user_id,
                email: user.email,
                display_name: user.display_name,
                role: user.role,
                status: user.status
            }
        });
    } catch (error) {
        console.error(error);
        return res.status(500).json({ message: "Error retrieving users.", error: error.message });
    }
};

// Get all users
export const getAllUsers = async (req, res) => {
    try {
        // Fetch all users
        const users = await User.find();

        return res.status(200).json(users);
    } catch (error) {
        console.error(error);
        return res.status(500).json({ message: "Error retrieving users.", error: error.message });
    }
};
