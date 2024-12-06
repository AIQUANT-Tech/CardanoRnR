import request from 'supertest';
import app from '../app.js'; 
import ReviewCategory from '../models/ReviewCategories.js';
import mongoose from 'mongoose';

const mockingoose = require("mockingoose");

describe('Review Categories Routes', () => {
  afterEach(() => {
    mockingoose.resetAll();
  });

  afterAll(async () => {
    await mongoose.connection.close();
  });
  
  describe('POST /api/reviewcategory/review-categories', () => {
    it('should create a review category when valid data is provided', async () => {
      const categoryData = {
        category_name: "SInDE",
        category_description: "A category for West side seviews.",
        created_by: "Shah Rukh"
      };

      mockingoose(ReviewCategory).toReturn(categoryData, 'save'); 

      const response = await request(app)
        .post('/api/reviewcategory/review-categories') 
        .send(categoryData);

      expect(response.status).toBe(201);
      expect(response.body.message).toBe('Review category created successfully');
      expect(response.body.data.category_name).toBe(categoryData.category_name);
      expect(response.body.data.category_description).toBe(categoryData.category_description);
    });

    it('should return an error if category_name is not provided', async () => {


      const response = await request(app)
        .post('/api/reviewcategory/review-categories') 
        .send({});

      expect(response.status).toBe(400);
      expect(response.body.message).toBe('Category name is required');
    });

    it('should return server error on exception', async () => {
      mockingoose(ReviewCategory).toReturn(new Error('Server error'), 'save'); 

      const categoryData = {
        category_name: 'Health',
        category_description: 'All about health',
        created_by: 'admin',
      };

      const response = await request(app)
        .post('/api/reviewcategory/review-categories')
        .send(categoryData);

      expect(response.status).toBe(500);
      expect(response.body.message).toBe('Server error');
    });
  });

  describe('GET /api/reviewcategory/review-categories', () => {
    it('should return a list of review categories when there are active categories', async () => {
      const mockCategories = [
        { category_name: 'Technology', category_description: 'All about technology' },
        { category_name: 'Health', category_description: 'All about health' },
      ];

      mockingoose(ReviewCategory).toReturn(mockCategories, 'find'); 

      const response = await request(app).get('/api/reviewcategory/review-categories'); 

      expect(response.status).toBe(200);
      expect(response.body.message).toBe('Review categories retrieved successfully');
      expect(response.body.data.length).toBe(2);
      expect(response.body.data[0].category_name).toBe('Technology');
    });

    it('should return a 404 if no active review categories are found', async () => {
      mockingoose(ReviewCategory).toReturn([], 'find'); 

      const response = await request(app).get('/api/reviewcategory/review-categories'); 

      expect(response.status).toBe(404);
      expect(response.body.message).toBe('No active review categories found');
    });

    it('should return server error if there is a database issue', async () => {
      mockingoose(ReviewCategory).toReturn(new Error('Server error'), 'find'); 

      const response = await request(app).get('/api/reviewcategory/review-categories');

      expect(response.status).toBe(500);
      expect(response.body.message).toBe('Server error');
    });
  });
});
