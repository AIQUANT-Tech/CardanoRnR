import ReplyTransData from "./ReplyTransData.js";
import User from '../user/UserMast.js';

// Function to create a new reply
export const createReply = async (req, res) => {
    try {
        const { review_reply_thread_rq } = req.body;
        const { header, review_id, content } = review_reply_thread_rq;
        const { user_name } = header;

        if (!user_name || !review_id || !content) {
            return res.status(400).json({ error: 'Missing required fields: user_name, review_id, or content.' });
        }

        const user = await User.findOne({ display_name: user_name });
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