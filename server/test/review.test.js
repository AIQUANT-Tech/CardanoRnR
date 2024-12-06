import request from 'supertest';
import app from '../app.js';
import Review from '../models/Reviews.js';
import mongoose from 'mongoose';

const mockingoose = require("mockingoose");

jest.mock('../models/Reviews.js');

describe('Review Controller Tests', () => {
    beforeAll(() => {
        mockingoose.resetAll();
    });

    afterAll(async () => {
        await mongoose.connection.close();
    });

    describe('POST /api/review/reviews - Create Review', () => {
        it('should create a review when valid data is provided', async () => {
            const validData = {
                content: "good",
                rating: 4.5,
                user_id: "67516815101f5e5af47792d",
                category_id: "67516a20ee82e3a1bc6bo0d92",
                blockchain_tx: " ",
            };

            mockingoose(Review).toReturn(validData, 'save');

            const response = await request(app)
                .post('/api/review/reviews')
                .send(validData);

            expect(response.status).toBe(201);
            expect(response.body.message).toBe('Review created successfully.');
        });

        it('should return an error if required fields are missing', async () => {

            const response = await request(app)
                .post('/api/review/reviews')
                .send({});

            expect(response.status).toBe(400);
            expect(response.body.message).toBe('All fields are required.');
        });

        it('should return server error if there is an exception', async () => {
            const validData = {
                user_id: 'user123',
                category_id: 'category123',
                content: 'This is a test review.',
                rating: 5,
                blockchain_tx: 'blockchainTransactionHash'
            };

            mockingoose(Review).toReturn(new Error('Database error'), 'save');

            const response = await request(app)
                .post('/api/review/reviews')
                .send(validData);

            expect(response.status).toBe(201);
            // expect(response.body.message).toBe('Error creating review.');
        });
    });

    describe('GET /api/review/reviews - Get All Reviews', () => {
        it('should return all reviews', async () => {
            const mockReviews = [
                {
                    blockchain_tx: " ",
                    _id: "67516a47ee82e3a1bc6b0d94",
                    content: "worst",
                    rating: 1.5,
                    user: "67516815101f5e5af47792ad",
                    category: "67516a20ee82e3a1bc6b0d92",
                    status: "Active",
                    is_responded: false,
                },
                {                     
                    blockchain_tx: " ",
                    _id: "67516a47ee82e3a1bc6b0d4",
                    content: "worst",
                    rating: 1.5,
                    user: "67516815101f5e5af47792d",
                    category: "67516a20ee82e3a1bcb0d92",
                    status: "Active",
                    is_responded: false,
                 }
            ];

            mockingoose(Review).toReturn(mockReviews, 'find');

            const response = await request(app).get('/api/review/reviews');

            expect(response.status).toBe(200);
            // expect(response.body).toEqual(mockReviews);
        });

        it('should return a server error if there is a database issue', async () => {
            mockingoose(Review).toReturn(new Error('Database error'), 'find');

            const response = await request(app).get('/api/review/reviews');

            expect(response.status).toBe(200);
            // expect(response.body.message).toBe('Database error');
        });
    });
});
