import mongoose, { Schema } from "mongoose";
import { v4 as uuidv4 } from "uuid";

const reviewCategorySchema = new Schema({
  category_id: {
    type: String,
    required: true,
    unique: true,
    default: uuidv4,
  },
  category_name: {
    type: String,
    required: true,
    trim: true,
  },
  category_description: {
    type: String,
    trim: true,
  },
  created_at: {
    type: Date,
    default: Date.now,
  },
  modified_at: {
    type: Date,
    default: Date.now,
  },
  created_by: {
    type: String,
    required: true,
  },
  modified_by: {
    type: String,
    required: true,
  },
  status: {
    type: String,
    enum: ["Active", "Inactive", "Deleted"],
    default: "Active",
  },
});

// reviewCategorySchema.pre('save', function(next) {
//   this.updatedAt = Date.now();
//   next();
// });

const ReviewCategory = new mongoose.model(
  "ReviewCategory",
  reviewCategorySchema
);

export default ReviewCategory;
