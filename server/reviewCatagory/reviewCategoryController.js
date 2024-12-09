import ReviewCategory from "./ReviewCategories.js";

// Function to create a new review category
export const createReviewCategory = async (req, res) => {
  try {
    const { category_name, category_description ,  created_by} = req.body;

    // Check if categoryName is provided
    if (!category_name) {
      return res.status(400).json({ message: 'Category name is required' });
    }

    // Create a new instance of the ReviewCategory model
    const newReviewCategory = new ReviewCategory({
      category_name,
      category_description,
      created_by
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
// Function to update a review category
export const updateReviewCategory = async (req, res) => {
  try {
    const { id } = req.params;
    const { category_name, category_description, status } = req.body;

    // Find and update the review category
    const updatedCategory = await ReviewCategory.findByIdAndUpdate(
      {_id: id},
      { category_name, category_description, status },
      { new: true, runValidators: true } // Returns updated document and validates schema
    );

    if (!updatedCategory) {
      return res.status(404).json({ message: 'Review category not found' });
    }

    return res.status(200).json({
      message: 'Review category updated successfully',
      data: updatedCategory
    });
  } catch (error) {
    console.error(error);
    return res.status(500).json({ message: 'Server error' });
  }
};
// Function to delete a review category
export const deleteReviewCategory = async (req, res) => {
  try {
    const { id } = req.params;

    const deletedCategory = await ReviewCategory.findByIdAndDelete({_id: id});

    if (!deletedCategory) {
      return res.status(404).json({ message: 'Review category not found' });
    }

    return res.status(200).json({
      message: 'Review category deleted successfully',
      data: deletedCategory
    });
  } catch (error) {
    console.error(error);
    return res.status(500).json({ message: 'Server error' });
  }
};
// You can add more functions categories based on your needs
