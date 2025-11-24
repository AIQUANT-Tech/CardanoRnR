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
  CircularProgress,
} from "@mui/material";
import AddIcon from "@mui/icons-material/Add";
import CheckCircleIcon from "@mui/icons-material/CheckCircle";
import Star from "../assets/Star.svg";
import API_BASE_URL from "../config.js";

const ReviewModal = ({ open, setOpen, email, setEmail }) => {
  // Form fields
  const [overallRating, setOverallRating] = useState(0);
  const [hover, setHover] = useState(-1);
  const [overallReview, setOverallReview] = useState("");
  const [categories, setCategories] = useState([]);
  const [selectedCategories, setSelectedCategories] = useState([]);

  // Error & Snackbar
  const [error, setError] = useState("");
  const [openSnackbar, setOpenSnackbar] = useState(false);
  const [submissionStatus, setSubmissionStatus] = useState("idle");
  const [reviewId, setReviewId] = useState(null);
  const [pollError, setPollError] = useState("");

  // Fetch categories when modal opens
  useEffect(() => {
    if (open) {
      fetchCategories();
      setSubmissionStatus("idle");
      setReviewId(null);
      setError("");
      setPollError("");
      setOverallRating(0);
      setOverallReview("");
      setSelectedCategories([]);
    }
  }, [open]);

  const fetchCategories = async () => {
    try {
      const response = await fetch(
        `${API_BASE_URL}api/reviewcategory/getReviewCategoryInfo`,
        {
          method: "POST",
          headers: { "Content-Type": "application/json" },
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
      console.log("Fetched Categories:", data);
      const fetchedCategories = data.review_category_fetch_rs.category_list.map(
        (category) => ({
          id: category.category_id,
          name: category.category_name,
          description: category.category_desc,
          status: category.Status,
          rating: 0,
          review: "",
        })
      );
      console.log("Fetched Categories 1:", fetchedCategories );
      setCategories(fetchedCategories);
    } catch (error) {
      console.error("Error fetching categories:", error);
    }
  };

  const toggleCategorySelection = (category) => {
    if (selectedCategories.find((c) => c.id === category.id)) {
      setSelectedCategories(
        selectedCategories.filter((c) => c.id !== category.id)
      );
    } else {
      setSelectedCategories([...selectedCategories, category]);
    }
  };

  const handleCategoryRating = (index, rating) => {
    const updated = [...selectedCategories];
    updated[index].rating = rating;
    setSelectedCategories(updated);
  };

  const handleCategoryReview = (index, review) => {
    const updated = [...selectedCategories];
    updated[index].review = review;
    setSelectedCategories(updated);
  };

  // Poll the backend for the overall review final redemption status
  // using the reviewId. This uses your GET /api/review/reviews/:id endpoint.
  useEffect(() => {
    let interval;
    if (submissionStatus === "submitted" && reviewId) {
      // Poll every 5 seconds
      interval = setInterval(async () => {
        try {
          const res = await fetch(
            `${API_BASE_URL}api/review/reviews/${reviewId}`,
            { method: "GET", headers: { "Content-Type": "application/json" } }
          );
          if (res.ok) {
            const data = await res.json();
            // If the final blockchain redemption status is confirmed (status true)
            if (data.status === true) {
              setSubmissionStatus("finalSuccess");
              clearInterval(interval);
            }
          } else {
            throw new Error(`Status check failed with ${res.status}`);
          }
        } catch (e) {
          console.error("Polling error:", e);
          setPollError("Error checking review status");
          clearInterval(interval);
          setSubmissionStatus("failed");
        }
      }, 5000);
    }
    return () => clearInterval(interval);
  }, [submissionStatus, reviewId]);

  const handleSubmit = async () => {
    if (selectedCategories.length === 0) {
      setError("Please select at least one category before submitting.");
      return;
    }
    setError("");
    // Immediately set loading to show spinner on submit click
    setSubmissionStatus("loading");

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

      const response = await fetch(`${API_BASE_URL}api/review/CreateReview`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(reviewData),
      });

      if (response.ok) {
        const data = await response.json();
        // Assume that the response returns the overall review document in data.overall
        // Immediately set state to "submitted" (lock tx hash received)
        setReviewId(data.overall._id);
        setSubmissionStatus("submitted");
      } else {
        const errorData = await response.json();
        console.error("Error submitting review:", errorData);
        setError("Error submitting review.");
        setSubmissionStatus("failed");
      }
    } catch (error) {
      console.error("Error:", error);
      setError("Error submitting review.");
      setSubmissionStatus("failed");
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
          {submissionStatus === "idle" && (
            <>
              <Typography color="textSecondary" gutterBottom>
                Thank you in advance for your feedback.
              </Typography>
              {/* Overall Rating */}
              <Typography variant="body1" fontWeight="bold" mt={2}>
                Overall Rating
              </Typography>
              <Box display="flex" alignItems="center" mb={2}>
                {[1, 2, 3, 4, 5].map((star) => (
                  <img
                    key={star}
                    src={Star}
                    alt="star"
                    onClick={() => setOverallRating(star)}
                    onMouseEnter={() => setHover(star)}
                    onMouseLeave={() => setHover(-1)}
                    style={{
                      cursor: "pointer",
                      width: 32,
                      height: 32,
                      filter:
                        hover >= star || overallRating >= star
                          ? "grayscale(0%)"
                          : "grayscale(100%)",
                      marginRight: 5,
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
                  style: { backgroundColor: "#f9f9f9", borderRadius: 10 },
                }}
              />

              {overallRating > 0 && overallReview.trim() && (
                <>
                  <Typography variant="body1" fontWeight="bold" mt={3}>
                    Select Categories to Review
                  </Typography>
                  <Box display="flex" flexWrap="wrap" gap={1} mt={1}>
                    {categories
                      .filter(
                        (category) => category.status.toLowerCase() === "active"
                      )
                      .map((category) => (
                        <Grid item xs={12} key={category.id}>
                          <Button
                            variant="outlined"
                            onClick={() => toggleCategorySelection(category)}
                            sx={{
                              textTransform: "none",
                              borderRadius: 10,
                              backgroundColor: selectedCategories.find(
                                (c) => c.id === category.id
                              )
                                ? "#d1e7dd"
                                : "#fff",
                            }}
                          >
                            {category.name}
                            {selectedCategories.find(
                              (c) => c.id === category.id
                            ) ? (
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
                          <img
                            key={star}
                            src={Star}
                            alt="star"
                            onClick={() => handleCategoryRating(index, star)}
                            onMouseEnter={() => setHover(star)}
                            onMouseLeave={() => setHover(-1)}
                            style={{
                              cursor: "pointer",
                              width: 32,
                              height: 32,
                              filter:
                                hover >= star || category.rating >= star
                                  ? "grayscale(0%)"
                                  : "grayscale(100%)",
                              marginRight: 5,
                            }}
                          />
                        ))}
                        <Typography sx={{ marginLeft: 2 }}>
                          {hover !== -1 ? hover : category.rating} / 5
                        </Typography>
                      </Box>
                      <TextField
                        fullWidth
                        multiline
                        rows={2}
                        value={category.review}
                        onChange={(e) =>
                          handleCategoryReview(index, e.target.value)
                        }
                        label={`Review for ${category.name}`}
                        placeholder={`${category.description}`}
                        sx={{ marginTop: 1 }}
                      />
                    </Box>
                  ))}
                </>
              )}
              {error && (
                <Alert severity="error" sx={{ marginTop: 2 }}>
                  {error}
                </Alert>
              )}
            </>
          )}

          {submissionStatus === "loading" && (
            <Box
              display="flex"
              flexDirection="column"
              alignItems="center"
              mt={4}
              mb={4}
            >
              <CircularProgress />
              <Typography variant="h6" mt={2}>
                Processing your review...
              </Typography>
            </Box>
          )}

          {(submissionStatus === "submitted" ||
            submissionStatus === "finalSuccess") && (
            <Box
              display="flex"
              flexDirection="column"
              alignItems="center"
              mt={4}
              mb={4}
            >
              <CheckCircleIcon color="success" sx={{ fontSize: 60 }} />
              <Typography variant="h6" mt={2}>
                Review submitted successfully!
              </Typography>
              {submissionStatus === "submitted" && (
                <Typography variant="body2" color="textSecondary">
                  Your review is submitted. We are still processing the
                  blockchain redemption.
                </Typography>
              )}
              {submissionStatus === "finalSuccess" && (
                <Typography variant="body2" color="textSecondary">
                  Blockchain redemption successful.
                </Typography>
              )}
            </Box>
          )}

          {submissionStatus === "failed" && (
            <Box
              display="flex"
              flexDirection="column"
              alignItems="center"
              mt={4}
              mb={4}
            >
              <Alert severity="error">
                {pollError ||
                  "Review submission failed. Please try again later."}
              </Alert>
            </Box>
          )}
        </DialogContent>
        <DialogActions>
          {submissionStatus === "idle" && (
            <>
              <Button
                variant="contained"
                color="primary"
                onClick={handleSubmit}
                sx={{ textTransform: "none", borderRadius: 20 }}
                disabled={!overallRating || !overallReview.trim()}
              >
                Submit
              </Button>
              <Button
                variant="outlined"
                color="secondary"
                onClick={handleClose}
                sx={{ textTransform: "none", borderRadius: 20 }}
              >
                Cancel
              </Button>
            </>
          )}
          {(submissionStatus === "loading" ||
            submissionStatus === "submitted" ||
            submissionStatus === "finalSuccess" ||
            submissionStatus === "failed") && (
            <Button
              variant="outlined"
              color="secondary"
              onClick={handleClose}
              sx={{ textTransform: "none", borderRadius: 20 }}
            >
              Close
            </Button>
          )}
        </DialogActions>
      </Dialog>

      <Snackbar
        open={openSnackbar}
        autoHideDuration={6000}
        onClose={handleCloseSnackbar}
        anchorOrigin={{ vertical: "middle", horizontal: "center" }}
      >
        <Alert
          onClose={handleCloseSnackbar}
          severity="success"
          sx={{ width: "100%" }}
        >
          Review submitted successfully!
          <br />
          Please wait for review approval.
        </Alert>
      </Snackbar>
    </>
  );
};

export default ReviewModal;
