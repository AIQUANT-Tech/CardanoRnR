import {
    createReply,
    fetchReviewReplyThread,
    getReplies,
  } from "../replyTransactionData/replyTransDataController.js"; // adjust the path if necessary
  
  import ReplyTransData from "../replyTransactionData/ReplyTransData.js";
  import User from "../user/UserMast.js";
  import Review from "../review/Reviews.js";
    const mockRequest = (body = {}) => {
    return { body };
  };
  
  const mockResponse = () => {
    const res = {};
    res.status = jest.fn().mockReturnValue(res);
    res.json = jest.fn().mockReturnValue(res);
    return res;
  };
  
  describe("Reply Controller", () => {
    afterEach(() => {
      jest.restoreAllMocks();
      jest.clearAllMocks();
    });
  
    describe("createReply", () => {
      it("should return 400 if required fields are missing", async () => {
        const req = mockRequest({
          review_reply_thread_rq: {
            header: {}, 
            review_id: null,
            content: "",
          },
        });
        const res = mockResponse();
  
        await createReply(req, res);
  
        expect(res.status).toHaveBeenCalledWith(400);
        expect(res.json).toHaveBeenCalledWith({
          error: "Missing required fields: user_name, review_id, or content.",
        });
      });
  
      it("should return 404 if user is not found", async () => {
        const req = mockRequest({
          review_reply_thread_rq: {
            header: { user_name: "nonexistentUserId" },
            review_id: "someReviewId",
            content: "Test reply content",
          },
        });
        const res = mockResponse();
  
        jest.spyOn(User, "findOne").mockResolvedValue(null);
  
        await createReply(req, res);
  
        expect(User.findOne).toHaveBeenCalledWith({ _id: "nonexistentUserId" });
        expect(res.status).toHaveBeenCalledWith(404);
        expect(res.json).toHaveBeenCalledWith({
          error: "User not found.",
        });
      });
  
      it("should return 404 if review is not found for a Business User", async () => {
        const req = mockRequest({
          review_reply_thread_rq: {
            header: { user_name: "businessUserId" },
            review_id: "nonexistentReviewId",
            content: "Business reply",
          },
        });
        const res = mockResponse();
  
        jest.spyOn(User, "findOne").mockResolvedValue({
          _id: "businessUserId",
          role: "Business User",
        });
  
        jest.spyOn(Review, "findOne").mockResolvedValue(null);
  
        await createReply(req, res);
  
        expect(User.findOne).toHaveBeenCalledWith({ _id: "businessUserId" });
        expect(Review.findOne).toHaveBeenCalledWith({ _id: "nonexistentReviewId" });
        expect(res.status).toHaveBeenCalledWith(404);
        expect(res.json).toHaveBeenCalledWith({
          error: "Review not found.",
        });
      });
  
      it("should update review (if not responded) and create reply for a Business User", async () => {
        const req = mockRequest({
          review_reply_thread_rq: {
            header: { user_name: "businessUserId" },
            review_id: "review123",
            content: "Business reply content",
          },
        });
        const res = mockResponse();
  
        const fakeUser = { _id: "businessUserId", role: "Business User" };
        jest.spyOn(User, "findOne").mockResolvedValue(fakeUser);
  
        const reviewSaveMock = jest.fn().mockResolvedValue(true);
        const fakeReview = {
          _id: "review123",
          is_responded: false,
          save: reviewSaveMock,
        };
        jest.spyOn(Review, "findOne").mockResolvedValue(fakeReview);
  
        const fakeReply = {
          _id: "replyId123",
          review_id: "review123",
          user_id: "businessUserId",
          content: "Business reply content",
        };
  
        jest
          .spyOn(ReplyTransData.prototype, "save")
          .mockResolvedValue(fakeReply);
  
        await createReply(req, res);
  

        expect(fakeReview.is_responded).toBe(true);
        expect(reviewSaveMock).toHaveBeenCalled();
  
        expect(ReplyTransData.prototype.save).toHaveBeenCalled();
  
        expect(res.status).toHaveBeenCalledWith(201);
        expect(res.json).toHaveBeenCalledWith({
          review_reply_rq: {
            status: "success",
            reply: fakeReply,
          },
        });
      });
  
      it("should catch errors and return 500", async () => {
        const req = mockRequest({
          review_reply_thread_rq: {
            header: { user_name: "errorUser" },
            review_id: "reviewError",
            content: "Some reply",
          },
        });
        const res = mockResponse();
 
        jest.spyOn(User, "findOne").mockRejectedValue(new Error("Unexpected error"));
  
        await createReply(req, res);
  
        expect(res.status).toHaveBeenCalledWith(500);
        expect(res.json).toHaveBeenCalledWith({ error: "Internal server error." });
      });
    });
  
    describe("fetchReviewReplyThread", () => {
      it("should return 400 if required fields are missing", async () => {
        const req = mockRequest({
          review_reply_thread_rq: {
            header: {},
            review_id: null,
          },
        });
        const res = mockResponse();
  
        await fetchReviewReplyThread(req, res);
  
        expect(res.status).toHaveBeenCalledWith(400);
        expect(res.json).toHaveBeenCalledWith({
          error: "Missing required fields: user_name or review_id.",
        });
      });
  
      it("should return 404 if user is not found", async () => {
        const req = mockRequest({
          review_reply_thread_rq: {
            header: { user_name: "nonexistentUser" },
            review_id: "review789",
          },
        });
        const res = mockResponse();
  
        jest.spyOn(User, "findOne").mockResolvedValue(null);
  
        await fetchReviewReplyThread(req, res);
  
        expect(User.findOne).toHaveBeenCalledWith({ _id: "nonexistentUser" });
        expect(res.status).toHaveBeenCalledWith(404);
        expect(res.json).toHaveBeenCalledWith({ error: "User not found." });
      });
  
      it("should return 404 if review is not found", async () => {
        const req = mockRequest({
          review_reply_thread_rq: {
            header: { user_name: "someUser" },
            review_id: "nonexistentReview",
          },
        });
        const res = mockResponse();
  
        jest.spyOn(User, "findOne").mockResolvedValue({ _id: "someUser" });
       
        jest.spyOn(Review, "findById").mockResolvedValue(null);
  
        await fetchReviewReplyThread(req, res);
  
        expect(Review.findById).toHaveBeenCalledWith({ _id: "nonexistentReview" });
        expect(res.status).toHaveBeenCalledWith(404);
        expect(res.json).toHaveBeenCalledWith({ error: "Review not found." });
      });
  
      it("should fetch and return the reply thread", async () => {
        const req = mockRequest({
          review_reply_thread_rq: {
            header: { user_name: "someUser" },
            review_id: "review101",
          },
        });
        const res = mockResponse();

        jest.spyOn(User, "findOne").mockResolvedValue({ _id: "someUser" });
        jest.spyOn(Review, "findById").mockResolvedValue({ _id: "review101" });

        const fakeReplies = [
          {
            content: "First reply",
            user_id: { display_name: "User One" },
          },
          {
            content: "Second reply",
            user_id: { display_name: "User Two" },
          },
        ];
  
        jest.spyOn(ReplyTransData, "find").mockReturnValue({
          populate: jest.fn().mockReturnThis(),
          sort: jest.fn().mockResolvedValue(fakeReplies),
        });
  
        await fetchReviewReplyThread(req, res);
  
        const expectedResponse = {
          review_reply_rs: {
            review_id: "review101",
            review_reply_info: [
              { reply: "First reply", replied_by: "User One" },
              { reply: "Second reply", replied_by: "User Two" },
            ],
          },
        };
  
        expect(ReplyTransData.find).toHaveBeenCalledWith({ review_id: "review101" });
        expect(res.status).toHaveBeenCalledWith(200);
        expect(res.json).toHaveBeenCalledWith(expectedResponse);
      });
  
      it("should catch errors and return 500", async () => {
        const req = mockRequest({
          review_reply_thread_rq: {
            header: { user_name: "errorUser" },
            review_id: "reviewError",
          },
        });
        const res = mockResponse();
  
        jest.spyOn(User, "findOne").mockRejectedValue(new Error("Unexpected error"));
  
        await fetchReviewReplyThread(req, res);
  
        expect(res.status).toHaveBeenCalledWith(500);
        expect(res.json).toHaveBeenCalledWith({ error: "Internal server error." });
      });
    });
  
    describe("getReplies", () => {
      it("should return 404 if no replies are found", async () => {
        const req = mockRequest(); // no body needed
        const res = mockResponse();
  
        jest.spyOn(ReplyTransData, "find").mockResolvedValue([]);
  
        await getReplies(req, res);
  
        expect(ReplyTransData.find).toHaveBeenCalled();
        expect(res.status).toHaveBeenCalledWith(404);
        expect(res.json).toHaveBeenCalledWith({
          success: false,
          message: "No replies found for the given review_id.",
        });
      });
  
      it("should return replies if found", async () => {
        const req = mockRequest();
        const res = mockResponse();
  
        const fakeReplies = [
          { _id: "r1", content: "Reply one" },
          { _id: "r2", content: "Reply two" },
        ];
        jest.spyOn(ReplyTransData, "find").mockResolvedValue(fakeReplies);
  
        await getReplies(req, res);
  
        expect(ReplyTransData.find).toHaveBeenCalled();
        expect(res.status).toHaveBeenCalledWith(200);
        expect(res.json).toHaveBeenCalledWith({
          success: true,
          data: fakeReplies,
        });
      });
  
      it("should catch errors and return 500", async () => {
        const req = mockRequest();
        const res = mockResponse();
  
        // Simulate error in ReplyTransData.find
        jest.spyOn(ReplyTransData, "find").mockRejectedValue(new Error("Unexpected error"));
  
        await getReplies(req, res);
  
        expect(ReplyTransData.find).toHaveBeenCalled();
        expect(res.status).toHaveBeenCalledWith(500);
        expect(res.json).toHaveBeenCalledWith({
          success: false,
          message: "Internal server error.",
        });
      });
    });
  });
  