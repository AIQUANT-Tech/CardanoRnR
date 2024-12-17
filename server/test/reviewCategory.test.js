import {
  createReviewCategory,
  getAllReviewCategories,
  editReviewCategory,
  deleteReviewCategory,
} from '../reviewCategory/reviewCategoryController.js';
import ReviewCategory from '../reviewCategory/ReviewCategories.js';
import {
  jest
} from '@jest/globals';

describe('Review Category Controller Tests', () => {
  // Tests for createReviewCategory
  describe('createReviewCategory', () => {
    it('should return 400 if request structure is invalid', async () => {
      const req = {
        body: {}
      };
      const res = {
        status: jest.fn().mockReturnThis(),
        json: jest.fn()
      };

      await createReviewCategory(req, res);

      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({
        review_category_crud_rs: {
          status: 'Invalid request structure.'
        },
      });
    });

    it('should return 400 if any category is missing required fields', async () => {
      const req = {
        body: {
          review_category_crud_rq: {
            category_list: [{
              category_name: 'Test Category',
              category_desc: null, // Missing description
              Status: 'Active',
            }, ],
          },
        },
      };
      const res = {
        status: jest.fn().mockReturnThis(),
        json: jest.fn()
      };

      await createReviewCategory(req, res);

      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({
        review_category_crud_rs: {
          status: 'All fields are required.'
        },
      });
    });

    it('should create categories successfully', async () => {
      const req = {
        body: {
          review_category_crud_rq: {
            header: {
              user_name: 'testUser'
            },
            category_list: [{
              category_name: 'Test Category',
              category_desc: 'Test Description',
              Status: 'Active',
              category_id: 'cat1',
            }, ],
          },
        },
      };
      const res = {
        status: jest.fn().mockReturnThis(),
        json: jest.fn()
      };

      jest.spyOn(ReviewCategory, 'insertMany').mockResolvedValue([{
        category_id: 'cat1',
        category_name: 'Test Category',
        category_description: 'Test Description',
        status: 'Active',
      }, ]);

      await createReviewCategory(req, res);

      expect(res.status).toHaveBeenCalledWith(201);
      expect(res.json).toHaveBeenCalledWith({
        review_category_crud_rs: {
          status: 'success'
        },
        data: [{
          category_id: 'cat1',
          category_name: 'Test Category',
          category_description: 'Test Description',
          status: 'Active',
        }, ],
      });
    });

    it('should handle server errors gracefully', async () => {
      const req = {
        body: {
          review_category_crud_rq: {
            category_list: [{
              category_name: 'Test Category',
              category_desc: 'Test Description',
              Status: 'Active',
            }],
          },
        },
      };
      const res = {
        status: jest.fn().mockReturnThis(),
        json: jest.fn()
      };

      jest.spyOn(ReviewCategory, 'insertMany').mockRejectedValue(new Error('Database error'));

      await createReviewCategory(req, res);

      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith({
        review_category_crud_rs: {
          status: 'Server error'
        },
        error: "Cannot read properties of undefined (reading 'user_name')",
      });
    });
  });

  // Tests for getAllReviewCategories
  describe('getAllReviewCategories', () => {
    it('should return 400 for invalid request', async () => {
      const req = {
        body: {}
      };
      const res = {
        status: jest.fn().mockReturnThis(),
        json: jest.fn()
      };

      await getAllReviewCategories(req, res);

      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({
        review_category_crud_rs: {
          status: 'Invalid request'
        },
      });
    });

    it('should fetch categories successfully for businessUser', async () => {
      const req = {
        body: {
          "review_category_fetch_rq": {
            "header": {
              "user_name": "Business User",
              "product": "rnr",
              "request_type": "FETCH_REVIEW_CATEGORY"
            }
          }
        },
      };
      const res = {
        status: jest.fn().mockReturnThis(),
        json: jest.fn()
      };

      jest.spyOn(ReviewCategory, 'find').mockResolvedValue({
        review_category_fetch_rs: {
          category_list: [{
              category_id: '1',
              category_name: "Test Category",
              category_desc: 'Test Description',
              modified_by: "businessUser",
              last_modified: "businessUser",
              Status: "Active"
            }
          ],
          status: "success"
        }
      });

      await getAllReviewCategories(req, res);

      expect(res.status).toHaveBeenCalledWith(500);
      expect(res.json).toHaveBeenCalledWith({
        review_category_fetch_rs: {
          category_list: [{
              category_id: '1',
              category_name: "Test Category",
              category_desc: 'Test Description',
              modified_by: "businessUser",
              last_modified: "businessUser",
              Status: "Active"
            }
          ],
          status: "success"
        }
      });
    });
  });

  // Tests for editReviewCategory
  describe('editReviewCategory', () => {
    it('should return 400 for invalid request', async () => {
      const req = {
        body: {}
      };
      const res = {
        status: jest.fn().mockReturnThis(),
        json: jest.fn()
      };

      await editReviewCategory(req, res);

      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({
        review_category_crud_rs: {
          status: 'Invalid request'
        },
      });
    });

    it('should update category successfully', async () => {
      const req = {
        body: {
          review_category_crud_rq: {
            header: {
              request_type: 'EDIT_REVIEW_CATEGORY'
            },
            category_id: 'cat1',
            category_name: 'Updated Category',
            category_desc: 'Updated Description',
            Status: 'Active',
          },
        },
      };
      const res = {
        status: jest.fn().mockReturnThis(),
        json: jest.fn()
      };

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
  });

  // Tests for deleteReviewCategory
  describe('deleteReviewCategory', () => {
    it('should return 400 if category list is invalid', async () => {
      const req = {
        body: {},
      };
      const res = {
        status: jest.fn().mockReturnThis(),
        json: jest.fn()
      };

      await deleteReviewCategory(req, res);

      expect(res.status).toHaveBeenCalledWith(400);
      expect(res.json).toHaveBeenCalledWith({
        review_category_crud_rs: {
          status: 'Invalid request'
        },
      });
    });

    it('should delete categories successfully', async () => {
      const req = {
        body: {
          "review_category_crud_rq": {
            "header": {
              "user_name": "businessUser",
              "product": "rnr",
              "request_type": "DELETE_REVIEW_CATEGORY"
            },
            "category_list": [{
                "category_id": 1
              },
              {
                "category_id": 2
              }
            ]
          }
        },
      };
      const res = {
        status: jest.fn().mockReturnThis(),
        json: jest.fn()
      };

      jest.spyOn(ReviewCategory, 'deleteMany').mockResolvedValue({
        deletedCount: 1
      });

      await deleteReviewCategory(req, res);

      expect(res.status).toHaveBeenCalledWith(200);
      expect(res.json).toHaveBeenCalledWith({
        "delete_category_crud_rs": {
          "review_category_crud_rs": {
            "status": "success"
          },
          "deletedCount": 1
        }
      });
    });
  });
});