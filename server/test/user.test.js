import { createUser, loginUser, getAllUsers } from '../user/userController.js';
import User from '../user/UserMast.js';
import bcrypt from 'bcryptjs';
import { generateToken } from '../auth/jwtUtils.js';
import { jest } from '@jest/globals';

jest.mock('../auth/jwtUtils.js');

describe('User Controller Tests', () => {
  let req, res;

  beforeEach(() => {
    // Initialize req with both body and user objects
    req = {
      body: {},
      user: {}
    };

    res = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn()
    };

    jest.clearAllMocks();
    // Suppress error logs in tests
    jest.spyOn(console, 'error').mockImplementation(() => {});
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  // ================================================================
  // Tests for createUser
  // ================================================================
  describe('createUser', () => {
    beforeEach(() => {
      req.body = {
        user_id: 'user123',
        email: 'test@example.com',
        password_hash: 'password123',
        display_name: 'Test User',
        role: 'End User',
        status: true,
        booking_id: 'NA',
      };
    });

    it('should return 400 if required fields are missing', async () => {
      req.body.email = null;
      await createUser(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({ user_crud_rs: { status: 'All fields are required.' } });
    });

    it('should return 400 if role is invalid', async () => {
      req.body.role = 'Any';
      await createUser(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({ user_crud_rs: { status: 'Invalid role provided.' } });
    });

    it('should return 400 if email is already registered', async () => {
      jest.spyOn(User, 'findOne').mockResolvedValue({ email: 'test@example.com' });
      await createUser(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({ user_crud_rs: { status: "Email is already registered." } });
    });

    it('should create a user successfully', async () => {
      jest.spyOn(User, 'findOne').mockResolvedValue(null);
      jest.spyOn(bcrypt, 'genSalt').mockResolvedValue('salt');
      jest.spyOn(bcrypt, 'hash').mockResolvedValue('hashed_password');
      jest.spyOn(User.prototype, 'save').mockResolvedValue({
        user_id: 'user123',
        email: 'test@example.com',
        password_hash: 'hashed_password',
        display_name: 'Test User',
        role: 'End User',
        status: true,
      });

      await createUser(req, res);
      expect(res.status).toHaveBeenCalledWith(201);
      expect(res.json).toHaveBeenCalledWith({
        user_crud_rs: { status: 'success' },
        user: {
          user_id: 'user123',
          email: 'test@example.com',
          password_hash: 'hashed_password',
          display_name: 'Test User',
          role: 'End User',
          status: true,
        },
      });
    });
  });

  // ================================================================
  // Tests for loginUser
  // ================================================================
  describe('loginUser', () => {
    beforeEach(() => {
      req.body = {
        email: 'test@example.com',
        password_hash: 'password123',
      };
    });

    it('should return 400 if email or password is missing', async () => {
      req.body.email = null;
      await loginUser(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({ user_crud_rs: { status: 'Email and password are required.' } });
    });

    it('should return 400 if credentials are invalid', async () => {
      jest.spyOn(User, 'findOne').mockResolvedValue(null);
      await loginUser(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({ user_crud_rs: { status: "Invalid credentials fields." } });
    });

    it('should return 400 if password does not match', async () => {
      jest.spyOn(User, 'findOne').mockResolvedValue({ password_hash: 'hashed_password' });
      jest.spyOn(bcrypt, 'compare').mockResolvedValue(false);
      await loginUser(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({ user_crud_rs: { status: "Invalid credentials fields." } });
    });

    it('should login successfully and return a token', async () => {
      const mockUser = {
        user_id: 'user123',
        email: 'test@example.com',
        password_hash: 'hashed_password',
        display_name: 'Test User',
        role: 'End User',
        status: true,
      };

      jest.spyOn(User, 'findOne').mockResolvedValue(mockUser);
      jest.spyOn(bcrypt, 'compare').mockResolvedValue(true);
      generateToken.mockReturnValue('mockToken');

      await loginUser(req, res);
      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith({
        user_crud_rs: { status: "success" },
        token: 'mockToken',
        user: {
          email: 'test@example.com',
          display_name: 'Test User',
          role: 'End User',
          status: true,
        },
      });
    });
  });

  // ================================================================
  // Tests for getAllUsers
  // ================================================================
  describe('getAllUsers', () => {
    beforeEach(() => {
      // Override req.user completely to ensure the role is set correctly.
      req.user = { role: 'Business User' };
    });

    it('should return 403 if user is not a Business User', async () => {
      req.user = { role: 'End User' }; // Non-authorized user
      await getAllUsers(req, res);
      expect(res.status).toHaveBeenCalledWith(403);
      expect(res.json).toHaveBeenCalledWith({
        user_crud_rs: { status: "Access denied. Only Business Users can perform this action." },
      });
    });

    it('should retrieve all users successfully', async () => {
      // Explicitly override req.user to ensure it is a Business User
      req.body.role = 'Business User'; // Ensure req.body.role matches 
      const mockUsers = [
        { user_id: 'user1', email: 'user1@example.com', display_name: 'User 1', role: 'End User', status: true },
        { user_id: 'user2', email: 'user2@example.com', display_name: 'User 2', role: 'Business User', status: true },
      ];

      jest.spyOn(User, 'find').mockReturnValue({
        select: jest.fn().mockResolvedValue(mockUsers),
      });

      await getAllUsers(req, res);
      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith(mockUsers);
    });

    it('should handle server errors gracefully', async () => {
      req.body.role = 'Business User';

      jest.spyOn(User, 'find').mockReturnValue({
        select: jest.fn().mockRejectedValue(new Error('Database error')),
      });

      await getAllUsers(req, res);
      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith({
        user_crud_rs: { status: "Error retrieving users." },
        error: 'Database error',
      });
    });
  });
});
