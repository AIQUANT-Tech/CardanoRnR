import mongoose from 'mongoose';

const Reviews = new mongoose.Schema({
    user_id: {
        type: mongoose.Schema.Types.ObjectId,
        ref: 'User',
        required: true,
    },
    overall_content: {
        type: String,
        required: true,
        description : 'OverallReview content'
    },
    overall_rating: {
        type: Number,    
        required: true,
        min : 1,
        max : 5,
        description : 'Rating given by the user (1-5 scale) overall'
    },
    review_list: [{
        type: Array,    
        required: true,
        description : 'List of reviews',
        category_id: {
            type: mongoose.Schema.Types.ObjectId,
            ref: 'ReviewCategory',
            required: true,
        },
        content: {
            type: String,
            required: true,
            description : 'Review content'
        },
        rating: {
            type: Number,    
            required: true,
            min : 1,
            max : 5,
            description : 'Rating given by the user (1-5 scale)'
        },
    }],
    created_at: {
        type: Date,    
        default: Date.now,
        description : 'Review submission timestamp'
    },
    is_responded: {
        type: Boolean,
        default: false,
        description : 'Response given or pending'
    },
    blockchain_tx : {
        type : String,   
        default: '',
        required: true,
        description : 'Blockchain transaction ID'
    },
    status: {
        type: String,
        enum: ['Active', 'Inactive','Deleted'],
        default: 'Active',
    },
  
});



const review = new mongoose.model('Review', Reviews);
export default review;
