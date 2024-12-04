import User from "../models/UserMast.js"; // Adjust the path if needed

// Create a new user
export const createUser = async (req, res) => {
    try {
        const { displayname, email, password, role, status } = req.body;

        // Validate required fields
        if (!displayname || !email || !password || !role) {
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

        // Create the new user
        const newUser = new User({ displayname, email, password, role, status });
        const savedUser = await newUser.save();

        return res.status(201).json({ message: "User created successfully.", user: savedUser });
    } catch (error) {
        console.error(error);
        return res.status(500).json({ message: "Error creating user.", error: error.message });
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
