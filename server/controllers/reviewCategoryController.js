import ReviewCategory from "../models/ReviewCategories.js";

// Function to create a new review category
export const createReviewCategory = async (req, res) => {
  try {
    const { categoryName, description, isActive, createdBy } = req.body;

    // Check if categoryName is provided
    if (!categoryName) {
      return res.status(400).json({ message: 'Category name is required' });
    }

    // Create a new instance of the ReviewCategory model
    const newReviewCategory = new ReviewCategory({
      categoryName,
      description,
      isActive,
      createdBy 
    });

    // Save the new review category to the database
    await newReviewCategory.save();

    // Return a success response
    return res.status(201).json({ message: 'Review category created successfully', data: newReviewCategory });
  } catch (error) {
    console.error(error);
    return res.status(500).json({ message: 'Server error' });
  }
};

// Function to get all review categories
export const getAllReviewCategories = async (req, res) => {
  try {
    const reviewCategories = await ReviewCategory.find({ status: "Active" }); // You can adjust the query as needed

    if (reviewCategories.length === 0) {
      return res.status(404).json({ message: 'No active review categories found' });
    }

    return res.status(200).json({ message: 'Review categories retrieved successfully', data: reviewCategories });
  } catch (error) {
    console.error(error);
    return res.status(500).json({ message: 'Server error' });
  }
};

// You can add more functions to update or delete categories based on your needs
