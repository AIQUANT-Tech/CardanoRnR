import React, { useState } from "react";
import {
  Box,
  Typography,
  Button,
  TextField,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
} from "@mui/material";

const WriteReviewModal = () => {
  const [open, setOpen] = useState(false); // State to manage modal open/close
  const [email, setEmail] = useState(""); // State to manage email input

  // Open the modal
  const handleOpen = () => {
    setOpen(true);
  };

  // Close the modal
  const handleClose = () => {
    setOpen(false);
  };

  // Handle sign-in button click
  const handleSignIn = () => {
    console.log("Email entered:", email);
    // Add your logic for sign-in or email validation here
    setOpen(false); // Close the modal after processing
  };

  return (
    <div>
      {/* Button to trigger modal */}
      <Button
        variant="contained"
        color="primary"
        onClick={handleOpen}
        sx={{
          borderRadius: 20,
          paddingX: 3,
          paddingY: 1,
          textTransform: "none",
        }}
      >
        Write a Review
      </Button>

      {/* Modal Dialog */}
      <Dialog open={open} onClose={handleClose}>
        <DialogTitle>
          <Typography variant="h6" fontWeight="bold">
            Want to write a review?
          </Typography>
        </DialogTitle>
        <DialogContent>
          <Typography color="textSecondary" gutterBottom>
            If you stayed or not, you can write a review.
          </Typography>
          <TextField
            fullWidth
            label="Email ID"
            type="email"
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            placeholder="Enter your email"
            InputProps={{
              style: {
                backgroundColor: "#f9f9f9",
                borderRadius: 10,
              },
            }}
            sx={{ marginTop: 2 }}
          />
        </DialogContent>
        <DialogActions>
          <Button
            variant="contained"
            color="primary"
            onClick={handleSignIn}
            sx={{
              textTransform: "none",
              borderRadius: 20,
            }}
          >
            Sign In
          </Button>
          <Button
            variant="outlined"
            color="secondary"
            onClick={handleClose}
            sx={{
              textTransform: "none",
              borderRadius: 20,
            }}
          >
            Cancel
          </Button>
        </DialogActions>
      </Dialog>
    </div>
  );
};

export default WriteReviewModal;
