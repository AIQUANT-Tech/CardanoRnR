import User from "./UserMast.js";
import bcrypt from "bcryptjs";
import { generateToken } from "../auth/jwtUtils.js"; 
import responses from '../utils/responses.js';
import roles from '../utils/roles.js';
import nodemailer from "nodemailer";
import crypto from "crypto";


let otps = {};

// Create a new user
export const createUser = async (req, res) => {
    try {
        const { user_id, email, password_hash, display_name, role } = req.body;

        if (!user_id || !display_name || !email || !password_hash || !role) {
            return res.status(400).json({user_crud_rs: {status:responses.validation.allFieldsRequired} });
        }

        if (![roles.endUser, roles.businessUser].includes(role)) {
            return res.status(400).json({ user_crud_rs: {status:responses.validation.invalidRole} });
        }

        // Check if email is already registered
        const existingUser = await User.findOne({ email });
        if (existingUser) {
            return res.status(400).json({ user_crud_rs: {status:responses.validation.emailExists} });
        }

        const salt = await bcrypt.genSalt(10);
        const password = await bcrypt.hash(password_hash, salt);

        // Create the new user
        const newUser = new User({ user_id, email, password_hash: password, display_name, role });
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
                user_id: user._id,
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
        if (req.body.role !== roles.businessUser) {
            return res.status(403).json({ user_crud_rs: {status:responses.validation.accessDeniedBusinessUser} });
        }

        const users = await User.find().select("-password_hash"); 

    
        return res.status(200).json(users);
    } catch (error) {
        console.error(error);
        return res.status(500).json({ user_crud_rs: {status:responses.error.retrieveUsers}, error: error.message });
    }
};

export const validateEndUser = async (req, res) => {
    try {
        const { email } = req.body;

        if (!email) {
            return res.status(400).json({ user_crud_rs: { status: responses.validation.emailRequired } });
        }

        // Check if the user exists
        const user = await User.findOne({ email: email, role: "End User" });
        if (!user) {
            return res.status(404).json({ user_crud_rs: { status: responses.validation.NotFound } });
        }

        return res.status(200).json({ user_crud_rs: { status: responses.success.success } });
    } catch (error) {
        console.error("Error in validateEndUser:", error);
        return res.status(500).json({ user_crud_rs: { status: responses.error.sendingOTP }, error: error.message });
    }
};

// // Verify OTP
// export const verifyEndUserOTP = async (req, res) => {
//     try {
//         const { email, otp } = req.body;

//         if (!email || !otp) {
//             return res.status(400).json({ user_crud_rs: { status: responses.validation.emailOtpRequired } });
//         }

//         // Check if the OTP exists and is valid
//         const storedOtpDetails = otps[email];
//         if (!storedOtpDetails) {
//             return res.status(400).json({ user_crud_rs: { status: responses.validation.otpNotFound } });
//         }

//         const { otp: storedOtp, timestamp } = storedOtpDetails;

//         // Check if the OTP is expired (10 minutes validity)
//         const isExpired = Date.now() - timestamp > 10 * 60 * 1000; // 10 minutes
//         if (isExpired) {
//             delete otps[email];
//             return res.status(400).json({ user_crud_rs: { status: responses.validation.otpExpired } });
//         }

//         // Verify the OTP
//         if (storedOtp === otp) {
//             delete otps[email]; // OTP is valid, remove it
//             return res.status(200).json({ user_crud_rs: { status: responses.success.otpVerified } });
//         }

//         return res.status(400).json({ user_crud_rs: { status: responses.validation.invalidOtp } });
//     } catch (error) {
//         console.error("Error in verifyEndUserOTP:", error);
//         return res.status(500).json({ user_crud_rs: { status: responses.error.verifyingOTP }, error: error.message });
//     }
// };