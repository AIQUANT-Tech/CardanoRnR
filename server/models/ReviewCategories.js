
import mongoose, { Schema} from 'mongoose';

const reviewCategorySchema = new Schema({
  categoryName: {
    type: String,
    required: true,
    trim: true
  },
  description: {
    type: String,
    trim: true
  },
  createdAt: {
    type: Date,
    default: Date.now
  },
  status: {
    type: String,
    enum: ['Active', 'Inactive','Deleted'],
    default: 'Active'
  },
  createdBy: {
    type: String, 
    required: true
  },
});

// reviewCategorySchema.pre('save', function(next) {
//   this.updatedAt = Date.now();
//   next();
// });

const ReviewCategory = new mongoose.model('ReviewCategory', reviewCategorySchema);

export default ReviewCategory;
