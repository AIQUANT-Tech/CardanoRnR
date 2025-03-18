import React, { useState, useEffect } from "react";
import {
  Dialog,
  DialogTitle,
  DialogContent,
  TextField,
  DialogActions,
  Button,
  Select,
  MenuItem,
  FormControl,
  InputLabel,
} from "@mui/material";

const EditCategoryModal = ({ open, onClose, category, onSubmit }) => {
  const [formData, setFormData] = useState({
    ...category,
  });
  useEffect(() => {
    setFormData(category);
  }, [category]);

  const handleChange = (e) => {
    const { name, value } = e.target;
    setFormData((prev) => ({ ...prev, [name]: value }));
  };

  const handleSubmit = () => {
    onSubmit(formData);
    onClose();
  };

  return (
    <Dialog open={open} onClose={onClose}>
      <DialogTitle>Edit Category</DialogTitle>
      <DialogContent>
        <TextField
          label="Category Name"
          name="category_name"
          value={formData.category_name}
          onChange={handleChange}
          fullWidth
          margin="normal"
        />
        <TextField
          label="Description"
          name="category_desc"
          value={formData.category_desc}
          onChange={handleChange}
          fullWidth
          margin="normal"
        />
        <FormControl fullWidth margin="n">
          <InputLabel
            sx={{
              paddingTop: "8px",
              fontSize: "16px",
              "&.Mui-focused": {
                color: "Black",
              },
            }}
            id="status-label"
          >
            Status
          </InputLabel>
          <Select
            name="Status"
            value={formData.Status}
            onChange={handleChange}
            fullWidth
            margin="normal"
          >
            <MenuItem value="Active">Active</MenuItem>
            <MenuItem value="Inactive">Inactive</MenuItem>
          </Select>
        </FormControl>
      </DialogContent>
      <DialogActions>
        <Button
          onClick={onClose}
          sx={{ color: "black", borderColor: "#DA9C9C" }}
        >
          Cancel
        </Button>
        <Button
          onClick={handleSubmit}
          variant="contained"
          color="primary"
          sx={{
            backgroundColor: "#DA9C9C",
            color: "black",
            "&:hover": { backgroundColor: "#D68C8C" },
          }}
        >
          Save
        </Button>
      </DialogActions>
    </Dialog>
  );
};

export default EditCategoryModal;
