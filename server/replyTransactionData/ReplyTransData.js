import mongoose from 'mongoose';

const ReviewTransaction = new mongoose.Schema({
    review_id: {
        type: mongoose.Schema.Types.ObjectId,
        ref: 'Review',
        required: true
    },
    user_id: {
        type: mongoose.Schema.Types.ObjectId,
        ref: 'User',
        required: true
    },
    content: {
        type: String,
        required: true,
        description :'Content of the message'
    },
    created_at: {
        type: Date,
        default: Date.now,
        description :'Reply submission timestamp'
    },
    Status: {
        type: String,
        enum: ['Active', 'Inactive'],
        required: true,
        description :'Status of the reply (Active or Inactive)'
    }
});

const reviewTrans = new mongoose.model('ReplyTransData', ReviewTransaction);
export default reviewTrans;
