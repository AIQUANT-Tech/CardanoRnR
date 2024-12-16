import jwt from 'jsonwebtoken';
import crypto from 'crypto';
import responses from '../utils/responses.js';
import roles from '../utils/roles.js';

const generateRandomSecret = () => {
    return crypto.randomBytes(64).toString('hex'); 
};

const JWT_SECRET = generateRandomSecret(); 

export const generateToken = (user) => {
    return jwt.sign(
        { user_id: user.user_id, email: user.email, role: user.role },
        JWT_SECRET,
        { expiresIn: '1h' }
    );
};

// Middleware to allow only Business Users
export const allowBusinessUser = (req, res, next) => {
    if (req.user.role !== roles.businessUser) {
        return res.status(403).json({ user_crud_rs: {status:responses.validation.accessDeniedBusinessUser} });
    }
    next();
};

export const allowEndUser = (req, res, next) => {
    if (req.user.role !== roles.endUser ) {
        return res.status(403).json({ user_crud_rs: {status:responses.validation.accessDeniedBusinessUser} });
    }
    next();
};

export const verifyToken = (req, res, next) => {
    const token = req.header("Authorization")?.replace("Bearer ", "");

    if (!token) {
        return res.status(401).json({ user_crud_rs: {status:responses.validation.accessDenied} });
    }

    try {
        const decoded = jwt.verify(token, JWT_SECRET);
        req.user = decoded;
        next();
    } catch (error) {
        return res.status(400).json({ user_crud_rs: {status:responses.validation.Token}});
    }
};
