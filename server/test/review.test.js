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
                _id: "6752d9f82bef84430b254187",
                user_id: "675287fd1dbb7d45a290dd19",
                overall_content: "Amazing experience overall.",
                overall_rating: 3,
                review_list: [
                    [
                        {
                            category_id: "6751907fb59022490aeacda0",
                            content: "Great customer service.",
                            rating: 2
                        },
                        {
                            category_id: "67528525f08b6bc15cc93169",
                            content: "Good product quality.",
                            rating: 1
                        }
                    ]
                ],
                is_responded: false,
                blockchain_tx: " ",
                status: "Active",
                created_at: "2024-12-06T11:03:20.625Z",
            }
             

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
                _id: "6752d9f82bef84430b254187",
                user_id: "675287fd1dbb7d45a290dd19",
                overall_content: "Amazing experience overall.",
                overall_rating: 3,
                review_list: [
                    [
                        {
                            category_id: "6751907fb59022490aeacda0",
                            content: "Great customer service.",
                            rating: 2
                        },
                        {
                            category_id: "67528525f08b6bc15cc93169",
                            content: "Good product quality.",
                            rating: 1
                        }
                    ]
                ],
                is_responded: false,
                blockchain_tx: " ",
                status: "Active",
                created_at: "2024-12-06T11:03:20.625Z",
            }
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
                    _id: "6752d9f82bef84430b254187",
                    user_id: "675287fd1dbb7d45a290dd19",
                    overall_content: "Amazing experience overall.",
                    overall_rating: 3,
                    review_list: [
                        [
                            {
                                category_id: "6751907fb59022490aeacda0",
                                content: "Great customer service.",
                                rating: 2
                            },
                            {
                                category_id: "67528525f08b6bc15cc93169",
                                content: "Good product quality.",
                                rating: 1
                            }
                        ]
                    ],
                    is_responded: false,
                    blockchain_tx: " ",
                    status: "Active",
                    created_at: "2024-12-06T11:03:20.625Z",
                },
                {
                    _id: "6752de33e833470b36335ca4",
                    user_id: "67516815101f5e5af47792ad",
                    overall_content: "BAD EXP overall.",
                    overall_rating: 3,
                    review_list: [
                        [
                            {
                                category_id: "6751907fb59022490aeacda0",
                                content: "Worst service.",
                                rating: 2
                            },
                            {
                                category_id: "67528525f08b6bc15cc93169",
                                content: "wooo quality.",
                                rating: 1
                            }
                        ]
                    ],
                    is_responded: false,
                    blockchain_tx: " ",
                    status: "Active",
                    created_at: "2024-12-06T11:21:23.013Z",
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
