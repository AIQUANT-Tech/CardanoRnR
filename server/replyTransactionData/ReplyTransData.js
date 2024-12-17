import mongoose from 'mongoose';

const ReplyTransaction = new mongoose.Schema({
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
        type: Boolean,
        default: true,
        description :'Status of the reply (Active or Inactive)'
    }
});

const replyTrans = new mongoose.model('ReplyTransData', ReplyTransaction);
export default replyTrans;
