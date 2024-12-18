import { createReview } from '../review/reviewController.js';
import Review from '../review/Reviews.js';
import User from '../user/UserMast.js';
import ReviewCategory from '../reviewCategory/ReviewCategories.js';
import { jest } from '@jest/globals';

describe('createReview', () => {
  let req, res;

  beforeEach(() => {
    req = {
      body: {
        new_review_rating_create_rq: {
          header: {
            request_type: 'CREATE_NEW_REVIEW_RATING',
            user_name: 'testUser',
          },
          user_email_id: 'test@example.com',
          overall_rating: 4,
          overall_review: 'Great product',
          category_wise_review_rating: [
            {
              category_id: 'cat1',
              review: 'Good quality',
              rating: 5,
            },
          ],
        },
      },
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

  it('should return 400 if request format is invalid', async () => {
    req.body = {};
    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(400);
    expect(res.json).toHaveBeenCalledWith({ new_review_rating_create_rs: {status: 'Invalid request' }});
  });

  it('should return 400 if request_type is invalid', async () => {
    req.body.new_review_rating_create_rq.header.request_type = 'INVALID_TYPE';
    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(400);
    expect(res.json).toHaveBeenCalledWith({ new_review_rating_create_rs: {status: 'Invalid request' }});
  });

  it('should return 400 if required fields are missing', async () => {
    req.body.new_review_rating_create_rq.user_email_id = null;
    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(400);
    expect(res.json).toHaveBeenCalledWith({new_review_rating_create_rs: {status: 'All fields are required.' }});
  });

  it('should return 400 if category_wise_review_rating is empty', async () => {
    req.body.new_review_rating_create_rq.category_wise_review_rating = [];
    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(400);
    expect(res.json).toHaveBeenCalledWith({new_review_rating_create_rs: {status:  'All fields are required.' }});
  });

  it('should return 404 if user is not found or inactive', async () => {
    jest.spyOn(User, 'findOne').mockResolvedValue(null);
    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(404);
    expect(res.json).toHaveBeenCalledWith({ new_review_rating_create_rs: {status: "Not found or inactive." } });
  });

  it('should return 404 if user has already reviewed', async () => {
    jest.spyOn(User, 'findOne').mockResolvedValue({ _id: 'userId' });
    jest.spyOn(Review, 'findOne').mockResolvedValue({ _id: 'existingReview' });
    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(404);
    expect(res.json).toHaveBeenCalledWith({ message: 'User already Reviewed!! ' });
  });

  it('should return 404 if one or more categories are not found', async () => {
    jest.spyOn(User, 'findOne').mockResolvedValue({ _id: 'userId' });
    jest.spyOn(Review, 'findOne').mockResolvedValue(null);
    jest.spyOn(ReviewCategory, 'find').mockResolvedValue([]);
    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(404);
    expect(res.json).toHaveBeenCalledWith({ new_review_rating_create_rs: {status: "Not found or inactive." }});
  });

  it('should create and save reviews successfully', async () => {
    jest.spyOn(User, 'findOne').mockResolvedValue({ _id: 'userId' });
    jest.spyOn(Review, 'findOne').mockResolvedValue(null);
    jest.spyOn(ReviewCategory, 'find').mockResolvedValue([
      { category_id: 'cat1', _id: 'cat1', status: true },
    ]);
    jest.spyOn(Review, 'insertMany').mockResolvedValue([{ _id: 'reviewId' }]);

    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(500);
    expect(res.json).toHaveBeenCalledWith({
      message: 'success.',
      reviews: [{ _id: 'reviewId' }],
    });
  });

  it('should handle server errors gracefully', async () => {
    jest.spyOn(User, 'findOne').mockRejectedValue(new Error('Database error'));
    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(500);
    expect(res.json).toHaveBeenCalledWith({
      new_review_rating_create_rs : {status: 'Error creating review.'},
      error: 'Database error',
    });
  });
});
