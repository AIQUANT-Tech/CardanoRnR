import User from "./UserMast.js";
import bcrypt from "bcryptjs";
import { generateToken } from "../auth/jwtUtils.js"; 
import responses from '../utils/responses.js';
import roles from '../utils/roles.js';


// Create a new user
export const createUser = async (req, res) => {
    try {
        const { user_id, email, password_hash, display_name, role, status } = req.body;

        if (!user_id || !display_name || !email || !password_hash || !role) {
            return res.status(400).json({user_crud_rs: {status:responses.validation.allFieldsRequired} });
        }

        if (![roles.endUser, roles.endUser].includes(role)) {
            return res.status(400).json({ user_crud_rs: {status:responses.validation.invalidRole} });
        }

        // Validate the status if provided
    if (status !== undefined && typeof status !== "boolean") {
        return res.status(400).json({ user_crud_rs: {status:responses.validation.statusTypeInvalid} });
      }
        // Check if email is already registered
        const existingUser = await User.findOne({ email });
        if (existingUser) {
            return res.status(400).json({ user_crud_rs: {status:responses.validation.emailExists} });
        }

        const salt = await bcrypt.genSalt(10);
        const password = await bcrypt.hash(password_hash, salt);

        // Create the new user
        const newUser = new User({ user_id, email, password_hash: password, display_name, role, status });
        const savedUser = await newUser.save();  
        

        return res.status(201).json({user_crud_rs: {status:responses.success.userCreated}, user: savedUser });
    } catch (error) {
        console.error(error);
        return res.status(500).json({user_crud_rs: {status:responses.validation.error.createUser}, error: error.message });
    }
};

// Login 
export const loginUser = async (req, res) => {
    try {
        const { email, password_hash } = req.body;

        if (!email || !password_hash) {
            return res.status(400).json({ user_crud_rs: {status:responses.validation.emailPasswordRequired} });
        }

        const user = await User.findOne({ email });
        if (!user) {
            return res.status(400).json({user_crud_rs: {status:responses.validation.invalidCredentials} });
        }

        const isMatch = await bcrypt.compare(password_hash, user.password_hash);
        if (!isMatch) {
            return res.status(400).json({user_crud_rs: {status:responses.validation.invalidCredentials} });
        }

        const token = generateToken(user);

        return res.status(200).json({
            user_crud_rs: {status:responses.success.loginSuccess},
            token, 
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
        return res.status(500).json({ user_crud_rs: {status:responses.error.login}, error: error.message });
    }
};

export const getAllUsers = async (req, res) => {
    try {
        if (req.user.role !== roles.businessUser) {
            return res.status(403).json({ user_crud_rs: {status:responses.validation.accessDeniedBusinessUser} });
        }

        const users = await User.find().select("-password_hash"); 

    
        return res.status(200).json(users);
    } catch (error) {
        console.error(error);
        return res.status(500).json({ user_crud_rs: {status:responses.error.retrieveUsers}, error: error.message });
    }
};
