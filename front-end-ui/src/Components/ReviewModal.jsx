import React, { useState, useEffect } from "react";
import {
  Box,
  Typography,
  Button,
  TextField,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Grid,
  Snackbar,
  Alert,
} from "@mui/material";
import StarIcon from "@mui/icons-material/Star";
import AddIcon from "@mui/icons-material/Add";
import CheckCircleIcon from "@mui/icons-material/CheckCircle";

const ReviewModal = ({ open, setOpen, email, setEmail }) => {
  const [overallRating, setOverallRating] = useState(0);
  const [hover, setHover] = useState(-1);
  const [overallReview, setOverallReview] = useState("");
  const [categories, setCategories] = useState([]);
  const [selectedCategories, setSelectedCategories] = useState([]);
  const [openSnackbar, setOpenSnackbar] = useState(false);
  const [error, setError] = useState("");  // State for error message

  useEffect(() => {
    if (open) {
      fetchCategories();
    }
  }, [open]);

  const fetchCategories = async () => {
    try {
      const response = await fetch(
        "http://localhost:8080/api/reviewcategory/getReviewCategoryInfo",
        {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
          body: JSON.stringify({
            review_category_fetch_rq: {
              header: {
                user_name: "Business User",
                product: "rnr",
                request_type: "FETCH_REVIEW_CATEGORY",
              },
            },
          }),
        }
      );

      if (!response.ok) {
        throw new Error(`HTTP error! status: ${response.status}`);
      }

      const data = await response.json();
      const fetchedCategories = data.review_category_fetch_rs.category_list.map(
        (category) => ({
          id: category.category_id,
          name: category.category_name,
          description: category.category_desc,
          rating: 0,
          review: "",
        })
      );

      setCategories(fetchedCategories);
    } catch (error) {
      console.error("Error fetching categories:", error);
    }
  };

  const toggleCategorySelection = (category) => {
    if (selectedCategories.includes(category)) {
      setSelectedCategories(
        selectedCategories.filter((c) => c.id !== category.id)
      );
    } else {
      setSelectedCategories([...selectedCategories, category]);
    }
  };

  const handleCategoryRating = (index, rating) => {
    const updatedCategories = [...selectedCategories];
    updatedCategories[index].rating = rating;
    setSelectedCategories(updatedCategories);
  };

  const handleCategoryReview = (index, review) => {
    const updatedCategories = [...selectedCategories];
    updatedCategories[index].review = review;
    setSelectedCategories(updatedCategories);
  };

  const handleSubmit = async () => {
    if (selectedCategories.length === 0) {
      setError("Please select at least one category before submitting.");
      return; // Prevent submission if no category is selected
    }
    setError(""); // Reset error message when form is valid

    try {
      const reviewData = {
        new_review_rating_create_rq: {
          header: {
            user_name: "End User",
            product: "rnr",
            request_type: "CREATE_NEW_REVIEW_RATING",
          },
          user_email_id: email,
          overall_rating: overallRating.toString(),
          overall_review: overallReview,
          category_wise_review_rating: selectedCategories.map((category) => ({
            category_id: category.id.toString(),
            rating: category.rating.toString(),
            review: category.review,
          })),
        },
      };

      const response = await fetch("http://localhost:8080/api/review/CreateReview", {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(reviewData),
      });

      if (response.ok) {
        const data = await response.json();
        console.log("Review submitted successfully:", data);
        setOpen(false);
        setOpenSnackbar(true);
      } else {
        const errorData = await response.json();
        console.error("Error submitting review:", errorData);
      }
    } catch (error) {
      console.error("Error:", error);
    }
    setEmail("");
  };

  const handleClose = () => {
    setOpen(false);
  };

  const handleCloseSnackbar = () => {
    setOpenSnackbar(false);
  };

  return (
    <>
      <Dialog open={open} maxWidth="md" fullWidth>
        <DialogTitle>
          <Typography variant="h6" fontWeight="bold">
            Overall Rating & Reviews
          </Typography>
        </DialogTitle>
        <DialogContent>
          <Typography color="textSecondary" gutterBottom>
            Thank you in advance for your feedback.
          </Typography>

          {/* Overall Rating */}
          <Typography variant="body1" fontWeight="bold" mt={2}>
            Overall Rating
          </Typography>
          <Box display="flex" alignItems="center" mb={2}>
            {[1, 2, 3, 4, 5].map((star) => (
              <StarIcon
                key={star}
                onClick={() => setOverallRating(star)}
                onMouseEnter={() => setHover(star)}
                onMouseLeave={() => setHover(-1)}
                sx={{
                  cursor: "pointer",
                  color: hover >= star || overallRating >= star ? "#fdd835" : "#e0e0e0",
                  fontSize: 32,
                }}
              />
            ))}
            <Typography sx={{ marginLeft: 2 }}>
              {hover !== -1 ? hover : overallRating} / 5
            </Typography>
          </Box>

          {/* Overall Review */}
          <TextField
            fullWidth
            multiline
            rows={3}
            value={overallReview}
            onChange={(e) => setOverallReview(e.target.value)}
            label="Overall Review"
            placeholder="Write your overall experience..."
            InputProps={{
              style: {
                backgroundColor: "#f9f9f9",
                borderRadius: 10,
              },
            }}
          />

          {/* Select Categories to Review */}
          {overallRating > 0 && overallReview.trim() && (
            <>
              <Typography variant="body1" fontWeight="bold" mt={3}>
                Select Categories to Review
              </Typography>
              <Box display="flex" flexWrap="wrap" gap={1} mt={1}>
                {categories.map((category) => (
                  <Grid item xs={12} key={category.id}>
                    <Button
                      variant="outlined"
                      onClick={() => toggleCategorySelection(category)}
                      sx={{
                        textTransform: "none",
                        borderRadius: 10,
                        backgroundColor: selectedCategories.includes(category)
                          ? "#d1e7dd"
                          : "#fff",
                      }}
                    >
                      {category.name}
                      {selectedCategories.includes(category) ? (
                        <CheckCircleIcon color="success" />
                      ) : (
                        <AddIcon color="primary" />
                      )}
                    </Button>
                  </Grid>
                ))}
              </Box>
            </>
          )}

          {/* Selected Category Ratings & Reviews */}
          {selectedCategories.length > 0 && (
            <>
              <Typography variant="body1" fontWeight="bold" mt={3}>
                Categorized Rating & Reviews
              </Typography>
              {selectedCategories.map((category, index) => (
                <Box key={category.id} mt={2} p={2}>
                  <Typography variant="body2" fontWeight="bold">
                    {category.name}
                  </Typography>
                  <Box display="flex" alignItems="center">
                    {[1, 2, 3, 4, 5].map((star) => (
                      <StarIcon
                        key={star}
                        onClick={() => handleCategoryRating(index, star)}
                        sx={{
                          cursor: "pointer",
                          color: category.rating >= star ? "#fdd835" : "#e0e0e0",
                          fontSize: 32,
                        }}
                      />
                    ))}
                    <Typography sx={{ marginLeft: 2 }}>
                      {category.rating} / 5
                    </Typography>
                  </Box>
                  <TextField
                    fullWidth
                    multiline
                    rows={2}
                    value={category.review}
                    onChange={(e) => handleCategoryReview(index, e.target.value)}
                    label={`Review for ${category.name}`}
                    placeholder={`Write your experience about ${category.name}...`}
                    sx={{ marginTop: 1 }}
                  />
                </Box>
              ))}
            </>
          )}

          {/* Error Message (styled with Alert) */}
          {error && (
            <Alert severity="error" sx={{ marginTop: 2 }}>
              {error}
            </Alert>
          )}
        </DialogContent>
        <DialogActions>
          <Button
            variant="contained"
            color="primary"
            onClick={handleSubmit}
            sx={{
              textTransform: "none",
              borderRadius: 20,
            }}
            disabled={!overallRating || !overallReview.trim()}
          >
            Submit
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

      {/* Snackbar for success message */}
      <Snackbar
        open={openSnackbar}
        autoHideDuration={6000}
        onClose={handleCloseSnackbar}
        anchorOrigin={{ vertical: "middle", horizontal: "center" }}
      >
        <Alert
          onClose={handleCloseSnackbar}
          severity="success"
          sx={{ width: "100%",
                fontSize: "40"
           }}
        >
          Review submitted successfully!
        </Alert>
      </Snackbar>
    </>
  );
};

export default ReviewModal;
