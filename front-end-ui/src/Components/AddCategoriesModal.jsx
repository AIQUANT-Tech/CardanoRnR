import React, { useState } from 'react';
import {
  Modal,
  Box,
  Typography,
  TextField,
  Button,
  Switch,
  FormControlLabel,
} from '@mui/material';
import AddIcon from '@mui/icons-material/Add';

const AddCategoriesModal = ({ open, onClose, onSubmit }) => {
  const [categories, setCategories] = useState([
    { name: "", status: "Active", desc: "" },
  ]);


  const handleAddCategory = () => {
    setCategories([...categories, { name: "", status: "Active", desc: "" }]);
  };


  const handleCategoryChange = (index, field, value) => {
    const updatedCategories = [...categories];
    updatedCategories[index][field] = value;
    setCategories(updatedCategories);
  };

  const handleSubmit = () => {
    const requestPayload = {
      review_category_crud_rq: {
        header: {
          user_name: 'businessUser',
          product: 'rnr',
          request_type: 'CREATE_REVIEW_CATEGORY',
        },
        category_list: categories.map((category, index) => ({
          category_id: index + 1,
          category_name: category.name,
          category_desc: category.desc,
          Status: category.status,
        })),
      },
    };
    onSubmit(requestPayload);
    onClose();
  };

  return (
    <Modal open={open} onClose={onClose}>
      <Box
        sx={{
          position: 'absolute',
          top: '50%',
          left: '50%',
          transform: 'translate(-50%, -50%)',
          width: '40%',
          bgcolor: 'white',
          boxShadow: 24,
          p: 4,
          borderRadius: 2,
        }}
      >
        <Typography variant="h6" sx={{ mb: 2 }}>
          Add Categories
        </Typography>
        {categories.map((category, index) => (
          <Box key={index} sx={{ display: 'flex', alignItems: 'center', mb: 2 }}>
            <TextField
              label={`Category Name`}
              variant="outlined"
              size="small"
              value={category.name}
              onChange={(e) =>
                handleCategoryChange(index, 'name', e.target.value)
              }
              sx={{ flex: 1, mr: 2 }}
            />
            <TextField
              label={`Category Description`}
              variant="outlined"
              size="small"
              value={category.desc}
              onChange={(e) =>
                handleCategoryChange(index, 'desc', e.target.value)
              }
              sx={{ flex: 1, mr: 2 }}
            />
            <FormControlLabel
              control={
                <Switch
                  checked={category.status === 'Active'}
                  onChange={(e) =>
                    handleCategoryChange(
                      index,
                      'status',
                      e.target.checked ? 'Active' : 'Inactive'
                    )
                  }
                />
              }
              label={category.status}
            />
          </Box>
        ))}
        <Button
          startIcon={<AddIcon />}
          onClick={handleAddCategory}
          variant="outlined"
          sx={{ mb: 3 }}
        >
          Add More Categories
        </Button>
        <Box sx={{ display: 'flex', justifyContent: 'flex-end', gap: 2 }}>
          <Button onClick={onClose} variant="outlined" color="error">
            Cancel
          </Button>
          <Button onClick={handleSubmit} variant="contained" color="primary">
            Add
          </Button>
        </Box>
      </Box>
    </Modal>
  );
};

export default AddCategoriesModal;
