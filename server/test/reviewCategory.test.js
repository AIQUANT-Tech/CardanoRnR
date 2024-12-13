// import request from 'supertest';
// import app from '../app.js'; 
// import ReviewCategory from '../reviewCategory/ReviewCategories.js';
// import mongoose from 'mongoose';

// const mockingoose = require("mockingoose");

// describe('Review Categories Routes', () => {
//   afterEach(() => {
//     mockingoose.resetAll();
//   });

//   afterAll(async () => {
//     await mongoose.connection.close();
//   });
  
//   describe('POST /api/reviewcategory/review-categories', () => {
//     it('should create a review category when valid data is provided', async () => {
//       const categoryData = {
//         category_name: "SInDE",
//         category_description: "A category for West side seviews.",
//         created_by: "Shah Rukh"
//       };

//       mockingoose(ReviewCategory).toReturn(categoryData, 'save'); 

//       const response = await request(app)
//         .post('/api/reviewcategory/review-categories') 
//         .send(categoryData);

//       expect(response.status).toBe(201);
//       expect(response.body.message).toBe('Review category created successfully');
//       expect(response.body.data.category_name).toBe(categoryData.category_name);
//       expect(response.body.data.category_description).toBe(categoryData.category_description);
//     });

//     it('should return an error if category_name is not provided', async () => {


//       const response = await request(app)
//         .post('/api/reviewcategory/review-categories') 
//         .send({});

//       expect(response.status).toBe(400);
//       expect(response.body.message).toBe('Category name is required');
//     });

//     it('should return server error on exception', async () => {
//       mockingoose(ReviewCategory).toReturn(new Error('Server error'), 'save'); 

//       const categoryData = {
//         category_name: 'Health',
//         category_description: 'All about health',
//         created_by: 'admin',
//       };

//       const response = await request(app)
//         .post('/api/reviewcategory/review-categories')
//         .send(categoryData);

//       expect(response.status).toBe(500);
//       expect(response.body.message).toBe('Server error');
//     });
//   });

//   describe('GET /api/reviewcategory/review-categories', () => {
//     it('should return a list of review categories when there are active categories', async () => {
//       const mockCategories = [
//         { category_name: 'Technology', category_description: 'All about technology' },
//         { category_name: 'Health', category_description: 'All about health' },
//       ];

//       mockingoose(ReviewCategory).toReturn(mockCategories, 'find'); 

//       const response = await request(app).get('/api/reviewcategory/review-categories'); 

//       expect(response.status).toBe(200);
//       expect(response.body.message).toBe('Review categories retrieved successfully');
//       expect(response.body.data.length).toBe(2);
//       expect(response.body.data[0].category_name).toBe('Technology');
//     });

//     it('should return a 404 if no active review categories are found', async () => {
//       mockingoose(ReviewCategory).toReturn([], 'find'); 

//       const response = await request(app).get('/api/reviewcategory/review-categories'); 

//       expect(response.status).toBe(404);
//       expect(response.body.message).toBe('No active review categories found');
//     });

//     it('should return server error if there is a database issue', async () => {
//       mockingoose(ReviewCategory).toReturn(new Error('Server error'), 'find'); 

//       const response = await request(app).get('/api/reviewcategory/review-categories');

//       expect(response.status).toBe(500);
//       expect(response.body.message).toBe('Server error');
//     });
//   });
// });

import {
  createReviewCategory,
  getAllReviewCategories,
  editReviewCategory,
  deleteReviewCategory,
} from '../reviewCategory/reviewCategoryController.js';
import ReviewCategory from '../reviewCategory/ReviewCategories.js';
import { jest } from '@jest/globals';

describe('Review Category Controller Tests', () => {
  let req, res;

  beforeEach(() => {
    req = {
      body: {},
    };
    res = {
      status: jest.fn().mockReturnThis(),
      json: jest.fn(),
    };
    jest.clearAllMocks();
  });

  afterEach(() => {
    jest.restoreAllMocks();
  });

  // Tests for createReviewCategory
  describe('createReviewCategory', () => {
    beforeEach(() => {
      req.body = {
        review_category_crud_rq: {
          header: { user_name: 'testUser' },
          category_list: [
            {
              category_name: 'Category 1',
              category_desc: 'Description for Category 1',
              Status: 'Active',
              category_id: 'cat1',
            },
          ],
        },
      };
    });

    it('should return 400 if request structure is invalid', async () => {
      req.body = {};
      await createReviewCategory(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({ message: 'Invalid request structure.' });
    });

    it('should return 400 if a category is missing required fields', async () => {
      req.body.review_category_crud_rq.category_list[0].Status = null;
      await createReviewCategory(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({ message: 'All fields are required in each category.' });
    });

    it('should create review categories successfully', async () => {
      jest.spyOn(ReviewCategory, 'insertMany').mockResolvedValue([
        { _id: 'cat1', category_name: 'Category 1', category_description: 'Description', status: 'Active' },
      ]);

      await createReviewCategory(req, res);
      expect(res.status).toHaveBeenCalledWith(201);
      expect(res.json).toHaveBeenCalledWith({
        message: 'Review categories created successfully.',
        data: [{ _id: 'cat1', category_name: 'Category 1', category_description: 'Description', status: 'Active' }],
      });
    });

    it('should handle server errors gracefully', async () => {
      jest.spyOn(ReviewCategory, 'insertMany').mockRejectedValue(new Error('Database error'));
      await createReviewCategory(req, res);
      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith({ message: 'Server error', error: 'Database error' });
    });
  });

  // Tests for getAllReviewCategories
  describe('getAllReviewCategories', () => {
    beforeEach(() => {
      req.body = {
        review_category_fetch_rq: {
          header: { user_name: 'businessUser', request_type: 'FETCH_REVIEW_CATEGORY' },
        },
      };
    });

    it('should return 400 for invalid request format', async () => {
      req.body = {};
      await getAllReviewCategories(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({ message: 'Invalid request format' });
    });

    it('should return 400 for invalid request type', async () => {
      req.body.review_category_fetch_rq.header.request_type = 'INVALID_TYPE';
      await getAllReviewCategories(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({ message: 'Invalid request type' });
    });

    it('should return 404 if no categories are found', async () => {
      jest.spyOn(ReviewCategory, 'find').mockResolvedValue([]);
      await getAllReviewCategories(req, res);
      expect(res.status).toHaveBeenCalledWith(404);
      expect(res.json).toHaveBeenCalledWith({
        review_category_fetch_rs: { category_list: [], message: 'No review categories found' },
      });
    });

    it('should fetch categories for businessUser successfully', async () => {
      jest.spyOn(ReviewCategory, 'find').mockResolvedValue([
        { category_id: 'cat1', category_name: 'Category 1', category_description: 'Description', status: 'Active' },
      ]);

      await getAllReviewCategories(req, res);
      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith({
        review_category_fetch_rs: {
          category_list: [
            {
              category_id: 'cat1',
              category_name: 'Category 1',
              category_desc: 'Description',
              modified_by: 'N/A',
              last_modified: 'N/A',
              Status: 'Active',
            },
          ],
          message: 'Review categories retrieved successfully',
        },
      });
    });

    it('should handle server errors gracefully', async () => {
      jest.spyOn(ReviewCategory, 'find').mockRejectedValue(new Error('Database error'));
      await getAllReviewCategories(req, res);
      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith({
        review_category_fetch_rs: { category_list: [], message: 'Server error', error: 'Database error' },
      });
    });
  });

  // Tests for editReviewCategory
  describe('editReviewCategory', () => {
    beforeEach(() => {
      req.body = {
        review_category_crud_rq: {
          header: { request_type: 'EDIT_REVIEW_CATEGORY' },
          category_id: 'cat1',
          category_name: 'Updated Category',
          category_desc: 'Updated Description',
          Status: 'Active',
        },
      };
    });

    it('should return 400 if request format is invalid', async () => {
      req.body = {};
      await editReviewCategory(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({ message: 'Invalid request format' });
    });

    it('should return 404 if category is not found', async () => {
      jest.spyOn(ReviewCategory, 'findOneAndUpdate').mockResolvedValue(null);
      await editReviewCategory(req, res);
      expect(res.status).toHaveBeenCalledWith(404);
      expect(res.json).toHaveBeenCalledWith({ message: 'Review category not found' });
    });

    it('should update the review category successfully', async () => {
      jest.spyOn(ReviewCategory, 'findOneAndUpdate').mockResolvedValue({
        category_id: 'cat1',
        category_name: 'Updated Category',
        category_description: 'Updated Description',
        status: 'Active',
      });

      await editReviewCategory(req, res);
      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith({
        review_category_crud_rs: {
          status: 'success',
          data: {
            category_id: 'cat1',
            category_name: 'Updated Category',
            category_description: 'Updated Description',
            status: 'Active',
          },
        },
      });
    });

    it('should handle server errors gracefully', async () => {
      jest.spyOn(ReviewCategory, 'findOneAndUpdate').mockRejectedValue(new Error('Database error'));
      await editReviewCategory(req, res);
      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith({ message: 'Server error', error: 'Database error' });
    });
  });

  // Tests for deleteReviewCategory
  describe('deleteReviewCategory', () => {
    beforeEach(() => {
      req.body = {
        review_category_crud_rq: {
          header: { request_type: 'DELETE_REVIEW_CATEGORY' },
          category_list: [{ category_id: 'cat1' }],
        },
      };
    });

    it('should return 400 for invalid request format', async () => {
      req.body = {};
      await deleteReviewCategory(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({ message: 'Invalid request format' });
    });

    it('should return 400 if category list is empty', async () => {
      req.body.review_category_crud_rq.category_list = [];
      await deleteReviewCategory(req, res);
      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({
        message: 'Category list is required and must contain at least one category_id',
      });
    });

    it('should return 404 if no categories are deleted', async () => {
      jest.spyOn(ReviewCategory, 'deleteMany').mockResolvedValue({ deletedCount: 0 });
      await deleteReviewCategory(req, res);
      expect(res.status).toHaveBeenCalledWith(404);
      expect(res.json).toHaveBeenCalledWith({ message: 'No review categories found to delete' });
    });

    it('should delete categories successfully', async () => {
      jest.spyOn(ReviewCategory, 'deleteMany').mockResolvedValue({ deletedCount: 1 });
      await deleteReviewCategory(req, res);
      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith({
        delete_category_crud_rs: { status: 'success', deletedCount: 1 },
      });
    });

    it('should handle server errors gracefully', async () => {
      jest.spyOn(ReviewCategory, 'deleteMany').mockRejectedValue(new Error('Database error'));
      await deleteReviewCategory(req, res);
      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith({ message: 'Server error', error: 'Database error' });
    });
  });
});
