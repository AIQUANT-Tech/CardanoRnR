import React, { useState } from "react";
import {
  Typography,
  Button,
  TextField,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
} from "@mui/material";
import { EditIcon } from "lucide-react";
import ReviewModal from "./ReviewModal";
import API_BASE_URL from "../config.js";


const WriteReviewModal = () => {
  const [openReviewModal, setOpenReviewModal] = useState(false);
  const [openModal, setOpenModal] = useState(false);
  const [email, setEmail] = useState("");
  const [error, setError] = useState("");

  const handleOpen = () => {
    setOpenReviewModal(true);
  };


  const handleClose = () => {
    setOpenReviewModal(false);
  };


  const handleSignIn = async () => {
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(email)) {
        setError("Please enter a valid email address.");
        return;
    }
    if (!email) {
        setError("Email is required");
        return;
    }

    try {
        const response = await fetch(`${API_BASE_URL}api/user/validate`, {
            method: "POST",
            headers: {
                "Content-Type": "application/json",
            },
            body: JSON.stringify({ email }),
        });

        const data = await response.json();

        if (data?.user_crud_rs?.status === "success") {
            console.log("Sign-in successful");
            setOpenModal(true); 
            setOpenReviewModal(false);
        } else {
            alert(data.user_crud_rs.status);
            setError("Invalid email or user not found.");
        }
    } catch (error) {
        console.error("Error during sign-in:", error);
        setError("Something went wrong. Please try again later.");
    }
    setOpenReviewModal(false);
    setError("");
};

  return (
    <div>
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
        startIcon={<EditIcon />}
      >
        Write a Review
      </Button>

      {/* Modal Dialog */}
      <Dialog open={openReviewModal}>
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
            error={!!error}
            helperText={error}
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
      <ReviewModal open={openModal} setOpen={setOpenModal} email={email} setEmail={setEmail}/>
    </div>
  );
};

export default WriteReviewModal;
