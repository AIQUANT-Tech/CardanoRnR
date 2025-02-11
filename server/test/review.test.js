import mongoose from 'mongoose';
import { createReview } from '../review/reviewController.js';
import Review from '../review/Reviews.js';
import User from '../user/UserMast.js';
import ReviewCategory from '../reviewCategory/ReviewCategories.js';
import { jest } from '@jest/globals';



describe('createReview', () => {
  let req, res;

  // Create valid ObjectIds to use in our mocks
  const validUserId = new mongoose.Types.ObjectId();
  const validCategoryId = new mongoose.Types.ObjectId();
  const validReviewId = new mongoose.Types.ObjectId();
  const validOverallReviewId = new mongoose.Types.ObjectId();

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
          overall_review: 'Great Service!',
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
    req.body = {}; // no new_review_rating_create_rq
    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(400);
    expect(res.json).toHaveBeenCalledWith({
      new_review_rating_create_rs: { status: 'Invalid request' },
    });
  });

  it('should return 400 if request_type is invalid', async () => {
    req.body.new_review_rating_create_rq.header.request_type = 'INVALID_TYPE';
    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(400);
    expect(res.json).toHaveBeenCalledWith({
      new_review_rating_create_rs: { status: 'Invalid request' },
    });
  });

  it('should return 400 if required fields are missing', async () => {
    req.body.new_review_rating_create_rq.user_email_id = null;
    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(400);
    expect(res.json).toHaveBeenCalledWith({
      new_review_rating_create_rs: { status: 'All fields are required.' },
    });
  });

  it('should return 400 if category_wise_review_rating is empty', async () => {
    req.body.new_review_rating_create_rq.category_wise_review_rating = [];
    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(400);
    expect(res.json).toHaveBeenCalledWith({
      new_review_rating_create_rs: { status: 'All fields are required.' },
    });
  });

  it('should return 404 if user is not found or inactive', async () => {
    // Simulate that the user is not found
    jest.spyOn(User, 'findOne').mockResolvedValue(null);
    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(404);
    expect(res.json).toHaveBeenCalledWith({
      new_review_rating_create_rs: { status: "Not found or inactive." },
    });
  });

  it('should return 404 if one or more categories are not found', async () => {
    // Simulate valid user and no previous review
    jest.spyOn(User, 'findOne').mockResolvedValue({
      _id: validUserId,
      email: 'test@example.com',
      status: true,
    });
    jest.spyOn(Review, 'findOne').mockResolvedValue(null);
    // Simulate that no valid categories were found
    jest.spyOn(ReviewCategory, 'find').mockResolvedValue([]);
    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(404);
    expect(res.json).toHaveBeenCalledWith({
      new_review_rating_create_rs: { status: "Not found or inactive." },
    });
  });

  it('should create and save reviews successfully', async () => {
    // Simulate valid user and no previous review
    jest.spyOn(User, 'findOne').mockResolvedValue({
      _id: validUserId,
      email: 'test@example.com',
      status: true,
    });
    jest.spyOn(Review, 'findOne').mockResolvedValue(null);
    // Simulate that a valid category is found matching the request
    jest.spyOn(ReviewCategory, 'find').mockResolvedValue([
      { category_id: 'cat1', _id: validCategoryId, status: true },
    ]);
    // Stub the save method on the overall review instance (created with new Review(...))
    jest.spyOn(Review.prototype, 'save').mockResolvedValue({ _id: validOverallReviewId });
    // Simulate successful insertMany for category-specific reviews
    jest.spyOn(Review, 'insertMany').mockResolvedValue([{ _id: validReviewId }]);

    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(201);
    expect(res.json).toHaveBeenCalledWith({
      new_review_rating_create_rs: { status: 'success' },
      reviews: [{ _id: validReviewId }],
      overall: { _id: validOverallReviewId },
    });
  });

  it('should handle server errors gracefully', async () => {
    // Simulate an error when finding the user (e.g., database error)
    jest.spyOn(User, 'findOne').mockRejectedValue(new Error('Database error'));
    await createReview(req, res);
    expect(res.status).toHaveBeenCalledWith(500);
    expect(res.json).toHaveBeenCalledWith({
      new_review_rating_create_rs: { status: 'Error creating review.' },
      error: 'Database error',
    });
  });
});
