import ReviewCategory from "./ReviewCategories.js";

// Function to create a new review category
export const createReviewCategory = async (req, res) => {
  try {
    const { review_category_crud_rq } = req.body;

    // Validate request format
    if (!review_category_crud_rq || !review_category_crud_rq.category_list) {
      return res.status(400).json({ message: "Invalid request structure." });
    }

    const { category_list } = review_category_crud_rq;

    // Validate categories in the list
    const categoriesToInsert = [];
    for (const category of category_list) {
      const { category_name, category_desc, Status, category_id } = category;

      if (!category_name || !category_desc || !Status) {
        return res
          .status(400)
          .json({ message: "All fields are required in each category." });
      }

      // Prepare category for insertion
      categoriesToInsert.push({
        category_id,
        category_name,
        category_description: category_desc,
        status: Status,
        created_by: review_category_crud_rq.header.user_name, // Assuming this comes from header
        modified_by: review_category_crud_rq.header.user_name, // Assuming this comes from header
      });
    }

    // Insert categories into the database
    const createdCategories = await ReviewCategory.insertMany(categoriesToInsert);

    // Return success response
    return res.status(201).json({
      message: "Review categories created successfully.",
      data: createdCategories,
    });
  } catch (error) {
    console.error(error);
    return res.status(500).json({ message: "Server error", error: error.message });
  }
};


// Function to get all review categories
// export const getAllReviewCategories = async (req, res) => {
//   try {
//     const reviewCategories = await ReviewCategory.find({ status: "Active" }); // You can adjust the query as needed

//     if (reviewCategories.length === 0) {
//       return res.status(404).json({ message: 'No active review categories found' });
//     }

//     return res.status(200).json({ message: 'Review categories retrieved successfully', data: reviewCategories });
//   } catch (error) {
//     console.error(error);
//     return res.status(500).json({ message: 'Server error' });
//   }
// };

// Function to update a review category
export const getAllReviewCategories = async (req, res) => {
  try {
    const { review_category_fetch_rq } = req.body;

    if (!review_category_fetch_rq) {
      return res.status(400).json({ message: "Invalid request format" });
    }

    const {
      header: { user_name, request_type },
    } = review_category_fetch_rq;

    if (request_type !== "FETCH_REVIEW_CATEGORY") {
      return res.status(400).json({ message: "Invalid request type" });
    }

    let query = {};
    let projection = {};

    if (user_name === "businessUser") {
      query = {}; 
      projection = { category_id: 1, category_name: 1, category_description: 1, status: 1, modified_by: 1, last_modified: 1 };
    } else if (user_name === "endUser") {
      query = { status: "Active" };
      projection = { category_id: 1, category_name: 1, category_description: 1 };
    } else {
      return res.status(400).json({ message: "Invalid user_name in request header" });
    }

    // Fetch review categories
    const reviewCategories = await ReviewCategory.find(query, projection);

    if (!reviewCategories || reviewCategories.length === 0) {
      return res.status(404).json({
        review_category_fetch_rs: {
          category_list: [],
          message: "No review categories found",
        },
      });
    }

    const categoryList = reviewCategories.map((category) => ({
      category_id: category.category_id,
      category_name: category.category_name,
      category_desc: category.category_description,
      ...(user_name === "businessUser" && {
        modified_by: category.modified_by || "N/A",
        last_modified: category.last_modified
          ? category.last_modified.toLocaleString()
          : "N/A",
        Status: category.status,
      }),
    }));

    return res.status(200).json({
      review_category_fetch_rs: {
        category_list: categoryList,
        message: "Review categories retrieved successfully",
      },
    });
  } catch (error) {
    console.error("Error fetching review categories:", error.message);
    return res.status(500).json({
      review_category_fetch_rs: {
        category_list: [],
        message: "Server error",
        error: error.message,
      },
    });
  }
};


export const editReviewCategory = async (req, res) => {
  try {
    const { review_category_crud_rq } = req.body;
    if (!review_category_crud_rq) {
      return res.status(400).json({ message: "Invalid request format" });
    }

    const {
      header: { request_type },
      category_id,
      category_name,
      category_desc,
      Status,
    } = review_category_crud_rq;

    if (request_type !== "EDIT_REVIEW_CATEGORY") {
      return res.status(400).json({ message: "Invalid request type" });
    }

    if (!category_id || !category_name || !category_desc || !Status) {
      return res
        .status(400)
        .json({ message: "Missing required fields: category_id, category_name, category_desc, Status" });
    }

    const updatedCategory = await ReviewCategory.findOneAndUpdate(
      { category_id: category_id },
      {
        category_name,
        category_description: category_desc,
        status: Status,
      },
      { new: true, runValidators: true }
    );

    if (!updatedCategory) {
      return res.status(404).json({ message: "Review category not found" });
    }

    return res.status(200).json({
      review_category_crud_rs: {
        status: 'success',
        data: updatedCategory
      }
    });
  } catch (error) {
    console.error("Error updating review category:", error.message);
    return res.status(500).json({ message: "Server error", error: error.message });
  }
};


// Function to delete a review category
export const deleteReviewCategory = async (req, res) => {
  try {
    const { review_category_crud_rq } = req.body;

    if (!review_category_crud_rq) {
      return res.status(400).json({ message: "Invalid request format" });
    }

    const {
      header: { request_type },
      category_list,
    } = review_category_crud_rq;

    if (request_type !== "DELETE_REVIEW_CATEGORY") {
      return res.status(400).json({ message: "Invalid request type" });
    }

    if (!Array.isArray(category_list) || category_list.length === 0) {
      return res.status(400).json({ message: "Category list is required and must contain at least one category_id" });
    }

    const categoryIds = category_list.map((category) => category.category_id);

    const deletedCategories = await ReviewCategory.deleteMany({ category_id: { $in: categoryIds } });

    if (deletedCategories.deletedCount === 0) {
      return res.status(404).json({ message: "No review categories found to delete" });
    }

    return res.status(200).json({
      delete_category_crud_rs: {
      status: 'success',
      deletedCount: deletedCategories.deletedCount,
      }
    });
  } catch (error) {
    console.error("Error deleting review categories:", error.message);
    return res.status(500).json({ message: "Server error", error: error.message });
  }
};

