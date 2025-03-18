import React, { useState, useEffect } from "react";
import {
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Button,
  Typography,
  TablePagination,
  Select,
  MenuItem,
  FormControl,
  InputLabel,
  CircularProgress,
  Dialog,
  DialogActions,
  DialogContent,
  DialogContentText,
  DialogTitle,
} from "@mui/material";
import EditIcon from "@mui/icons-material/Edit";
import DeleteIcon from "@mui/icons-material/Delete";
import AddIcon from "@mui/icons-material/Add";
import AddCategoriesModal from "./AddCategoriesModal";
import EditCategoryModal from "./EditCategoryModal";
import "./styles.css";
import API_BASE_URL from "../config.js";

const CategoriesTable = () => {
  const [categoriesData, setCategoriesData] = useState([]);
  const [isLoading, setIsLoading] = useState(true);
  const [page, setPage] = useState(0);
  const [rowsPerPage, setRowsPerPage] = useState(7);
  const [filterStatus, setFilterStatus] = useState("All");
  const [isAddModalOpen, setIsAddModalOpen] = useState(false);
  const [isEditModalOpen, setIsEditModalOpen] = useState(false);
  const [selectedCategory, setSelectedCategory] = useState(null);
  const [isDeleteDialogOpen, setIsDeleteDialogOpen] = useState(false);
  const [categoryToDelete, setCategoryToDelete] = useState(null);

  // Fetch data from backend
  useEffect(() => {
    const fetchCategories = async () => {
      setIsLoading(true);
      try {
        const requestPayload = {
          review_category_fetch_rq: {
            header: {
              user_name: "Business User",
              product: "rnr",
              request_type: "FETCH_REVIEW_CATEGORY",
            },
          },
        };

        const response = await fetch(
          `${API_BASE_URL}api/reviewcategory/getReviewCategoryInfo`,
          {
            method: "POST",
            headers: {
              "Content-Type": "application/json",
            },
            body: JSON.stringify(requestPayload),
          }
        );

        if (!response.ok) {
          throw new Error("Failed to fetch categories");
        }

        const data = await response.json();
        setCategoriesData(data.review_category_fetch_rs.category_list || []);
        setIsLoading(false);
      } catch (error) {
        console.error("Error fetching categories:", error);
        setIsLoading(false);
      }
    };

    fetchCategories();
  }, []);

  // Handle adding new categories
  const handleAddCategories = async (newCategories) => {
    const user = JSON.parse(localStorage.getItem("user"));

    const categoryList = newCategories.map((category) => ({
      category_name: category.category_name,
      category_desc: category.category_desc,
      Status: category.Status,
      created_by: user.display_name,
      modified_by: user.display_name,
    }));

    const payload = {
      review_category_crud_rq: {
        header: {
          user_name: user.display_name,
          product: "rnr",
          request_type: "CREATE_REVIEW_CATEGORY",
        },
        category_list: categoryList,
      },
    };

    try {
      const response = await fetch(
        `${API_BASE_URL}api/reviewcategory/createReviewCategory`,
        {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(payload),
        }
      );

      if (!response.ok) {
        throw new Error("Failed to add categories");
      }

      const result = await response.json();
      console.log("Categories added successfully:", result);

      // Refresh data after adding new categories
      setCategoriesData((prevData) => [...prevData, ...categoryList]);
    } catch (error) {
      console.error("Error adding categories:", error);
    }
  };

  // Handle deleting a category
  const handleDeleteCategory = async () => {
    if (!categoryToDelete) return;

    const payload = {
      review_category_crud_rq: {
        header: {
          user_name: "businessUser",
          product: "rnr",
          request_type: "DELETE_REVIEW_CATEGORY",
        },
        category_list: [{ category_id: categoryToDelete.category_id }],
      },
    };

    try {
      const response = await fetch(
        `${API_BASE_URL}api/reviewcategory/deleteReviewCategory`,
        {
          method: "DELETE",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(payload),
        }
      );

      if (!response.ok) {
        throw new Error("Failed to delete category");
      }

      console.log("Category deleted successfully");
      // Update local data
      setCategoriesData((prevData) =>
        prevData.filter(
          (category) => category.category_id !== categoryToDelete.category_id
        )
      );
      setCategoryToDelete(null);
      setIsDeleteDialogOpen(false);
    } catch (error) {
      console.error("Error deleting category:", error);
    }
  };

  const handleEditCategory = async (updatedCategory) => {
    const payload = {
      review_category_crud_rq: {
        header: {
          user_name: "businessUser",
          product: "rnr",
          request_type: "EDIT_REVIEW_CATEGORY",
        },
        category_id: updatedCategory.category_id,
        category_name: updatedCategory.category_name,
        category_desc: updatedCategory.category_desc,
        Status: updatedCategory.Status,
      },
    };

    try {
      const response = await fetch(
        `${API_BASE_URL}api/reviewcategory/editReviewCategory`,
        {
          method: "PUT",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify(payload),
        }
      );

      if (!response.ok) {
        throw new Error("Failed to edit category");
      }

      const result = await response.json();
      console.log("Category updated successfully:", result);

      // Update local data
      setCategoriesData((prevData) =>
        prevData.map((category) =>
          category.category_id === updatedCategory.category_id
            ? { ...category, ...updatedCategory }
            : category
        )
      );
    } catch (error) {
      console.error("Error editing category:", error);
    }
  };

  return (
    <div className="categories-table">
      <div
        style={{ display: "flex", alignItems: "center" }}
        className="table-header"
      >
        <Typography sx={{ marginLeft: "15px" }} variant="h6">
          All Categories
        </Typography>
        <div style={{ display: "flex", alignItems: "center", gap: "300px" }}>
          <Button
            variant="contained"
            startIcon={<AddIcon />}
            onClick={() => setIsAddModalOpen(true)}
            className="add-category-button"
          >
            Add Categories
          </Button>
          <FormControl size="small" sx={{ width: "150px" }}>
            <InputLabel
              sx={{
                padding: "10px",
                paddingLeft: "55px",
                color: "Black",
                "&.Mui-focused": {
                  color: "Black",
                },
              }}
              id="status-label"
            >
              Status
            </InputLabel>
            <Select
              labelId="status-label"
              value={filterStatus}
              onChange={(e) => {
                setFilterStatus(e.target.value);
                setPage(0); // Reset pagination to first page
              }}
              sx={{
                borderColor: "#DA9C9C",
                display: "flex",
                alignItems: "center",
                "& .MuiOutlinedInput-notchedOutline": {
                  borderColor: "#DA9C9C",
                },
                "&:hover .MuiOutlinedInput-notchedOutline": {
                  borderColor: "#DA9C9C",
                },
                "&.Mui-focused .MuiOutlinedInput-notchedOutline": {
                  borderColor: "#DA9C9C",
                },
              }}
            >
              <MenuItem value="All">All</MenuItem>
              <MenuItem value="Active">Active</MenuItem>
              <MenuItem value="Inactive">Inactive</MenuItem>
            </Select>
          </FormControl>
        </div>
      </div>
      {isLoading ? (
        <div
          style={{ display: "flex", justifyContent: "center", padding: "16px" }}
        >
          <CircularProgress />
        </div>
      ) : (
        <TableContainer>
          <Table>
            <TableHead>
              <TableRow>
                <TableCell>Categories Name</TableCell>
                <TableCell>Created By</TableCell>
                <TableCell>Status</TableCell>
                <TableCell>Action</TableCell>
              </TableRow>
            </TableHead>
            <TableBody>
              {categoriesData
                .filter(
                  (row) => filterStatus === "All" || row.Status === filterStatus
                )
                .slice(page * rowsPerPage, page * rowsPerPage + rowsPerPage)
                .map((row) => (
                  <TableRow key={row.category_id}>
                    <TableCell>{row.category_name}</TableCell>
                    <TableCell>{row.modified_by}</TableCell>
                    <TableCell>{row.Status}</TableCell>
                    <TableCell>
                      <div
                        style={{
                          display: "flex",
                          alignItems: "center",
                          gap: "8px",
                        }}
                      >
                        <div
                          className="action-icon edit-icon"
                          onClick={() => {
                            setSelectedCategory(row);
                            setIsEditModalOpen(true);
                          }}
                        >
                          <svg
                            xmlns="http://www.w3.org/2000/svg"
                            width="24"
                            height="24"
                            viewBox="0 0 24 24"
                            fill="none"
                            stroke="currentColor"
                            strokeWidth="2"
                            strokeLinecap="round"
                            strokeLinejoin="round"
                            className="lucide lucide-pencil"
                          >
                            <path d="M21.174 6.812a1 1 0 0 0-3.986-3.987L3.842 16.174a2 2 0 0 0-.5.83l-1.321 4.352a.5.5 0 0 0 .623.622l4.353-1.32a2 2 0 0 0 .83-.497z" />
                            <path d="m15 5 4 4" />
                          </svg>
                        </div>
                        <div
                          className="action-icon delete-icon"
                          onClick={() => {
                            setCategoryToDelete(row);
                            setIsDeleteDialogOpen(true);
                          }}
                        >
                          <svg
                            xmlns="http://www.w3.org/2000/svg"
                            width="24"
                            height="24"
                            viewBox="0 0 24 24"
                            fill="none"
                            stroke="currentColor"
                            strokeWidth="2"
                            strokeLinecap="round"
                            strokeLinejoin="round"
                            className="lucide lucide-trash-2"
                          >
                            <path d="M3 6h18" />
                            <path d="M19 6v14c0 1-1 2-2 2H7c-1 0-2-1-2-2V6" />
                            <path d="M8 6V4c0-1 1-2 2-2h4c1 0 2 1 2 2v2" />
                            <line x1="10" x2="10" y1="11" y2="17" />
                            <line x1="14" x2="14" y1="11" y2="17" />
                          </svg>
                        </div>
                      </div>
                    </TableCell>
                  </TableRow>
                ))}
            </TableBody>
          </Table>
        </TableContainer>
      )}
      <TablePagination
        component="div"
        count={categoriesData.length}
        page={page}
        onPageChange={(e, newPage) => setPage(newPage)}
        rowsPerPage={rowsPerPage}
        onRowsPerPageChange={(e) =>
          setRowsPerPage(parseInt(e.target.value, 10))
        }
        rowsPerPageOptions={[]}
      />

      {/* Conditionally render AddCategoriesModal to force unmount on close */}
      {isAddModalOpen && (
        <AddCategoriesModal
          open={isAddModalOpen}
          onClose={() => setIsAddModalOpen(false)}
          onSubmit={(categories) => {
            console.log("Received categories:", categories);
            handleAddCategories(
              categories.review_category_crud_rq.category_list
            );
            // Close the modal after submission so it unmounts and resets its state
            setIsAddModalOpen(false);
          }}
        />
      )}

      {selectedCategory && (
        <EditCategoryModal
          open={isEditModalOpen}
          onClose={() => setIsEditModalOpen(false)}
          category={selectedCategory}
          onSubmit={(updatedCategory) => {
            setIsEditModalOpen(false);
            handleEditCategory(updatedCategory);
          }}
        />
      )}

      <Dialog
        open={isDeleteDialogOpen}
        onClose={() => setIsDeleteDialogOpen(false)}
      >
        <DialogTitle>Confirm Delete</DialogTitle>
        <DialogContent>
          <DialogContentText>
            Are you sure you want to delete this category?
          </DialogContentText>
        </DialogContent>
        <DialogActions>
          <Button onClick={() => setIsDeleteDialogOpen(false)}>Cancel</Button>
          <Button
            onClick={handleDeleteCategory}
            variant="contained"
            color="error"
          >
            Delete
          </Button>
        </DialogActions>
      </Dialog>
    </div>
  );
};

export default CategoriesTable;
