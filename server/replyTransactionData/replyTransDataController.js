import ReplyTransData from "./ReplyTransData.js";
import User from '../user/UserMast.js';
import Review from '../review/Reviews.js'; 

// Function to create a new reply
export const createReply = async (req, res) => {
    try {
        const { review_reply_thread_rq } = req.body;
        const { header, review_id, content } = review_reply_thread_rq;
        const { user_name } = header;

        if (!user_name || !review_id || !content) {
            return res.status(400).json({ error: 'Missing required fields: user_name, review_id, or content.' });
        }

        const user = await User.findOne({ user_id: user_name });
        if (!user) {
            return res.status(404).json({ error: 'User not found.' });
        }

        const newReply = new ReplyTransData({
            review_id: review_id, 
            user_id: user._id,
            content,
        });

        const savedReply = await newReply.save();

        return res.status(201).json({ review_reply_rq:{
            status: 'success',
            reply: savedReply,
        }});
    } catch (error) {
        console.error('Error creating reply:', error);
        return res.status(500).json({ error: 'Internal server error.' });
    }
};

//view reply as end user
export const fetchReviewReplyThread = async (req, res) => {
    try {
        const { review_reply_thread_rq } = req.body;
        const { header, review_id } = review_reply_thread_rq;
        const { user_name } = header;

        if (!user_name || !review_id) {
            return res.status(400).json({ error: 'Missing required fields: user_name or review_id.' });
        }

        const user = await User.findOne({ user_id: user_name });
        if (!user) {
            return res.status(404).json({ error: 'User not found.' });
        }

        // Find the review by review_id
        const review = await Review.findById({_id: review_id});
        if (!review) {
            return res.status(404).json({ error: 'Review not found.' });
        }

        // Fetch the reply thread for the given review_id
        const replies = await ReplyTransData.find({ review_id: review_id })
            .populate('user_id', 'display_name') 
            .sort({ created_at: 1 }); 

        const response = {
            review_reply_rs: {
                review_id: review_id,
                review_reply_info: replies.map(reply => ({
                    reply: reply.content,
                    replied_by: reply.user_id.display_name
                }))
            }
        };

        return res.status(200).json(response);
    } catch (error) {
        console.error('Error fetching review reply thread:', error);
        return res.status(500).json({ error: 'Internal server error.' });
    }
};