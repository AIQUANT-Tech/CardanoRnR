
import mongoose, { Schema} from 'mongoose';

const reviewCategorySchema = new Schema({
  category_name: {
    type: String,
    required: true,
    trim: true
  },
  category_description: {
    type: String,
    trim: true
  },
  created_at: {
    type: Date,
    default: Date.now
  },
  created_by: {
    type: String, 
    required: true
  },
  status: {
    type: String,
    enum: ['Active', 'Inactive','Deleted'],
    default: 'Active'
  },
  
});

// reviewCategorySchema.pre('save', function(next) {
//   this.updatedAt = Date.now();
//   next();
// });

const ReviewCategory = new mongoose.model('ReviewCategory', reviewCategorySchema);

export default ReviewCategory;
