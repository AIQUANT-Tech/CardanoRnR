import React, { useEffect, useState } from "react";
import {
  Box,
  Typography,
  Button,
  Card,
  CardContent,
  Avatar,
  Grid,
  TextField,
} from "@mui/material";
import EditIcon from "@mui/icons-material/Edit";
import RatingReviewModal from "../../Components/RatingReviewModal"; // Updated name of reusable modal

const GuestReviews = () => {
  const [reviews, setReviews] = useState([]);
  const [visibleCount, setVisibleCount] = useState(3);
  const [openReviewModal, setOpenReviewModal] = useState(false);
  const [openMoreReviewsModal, setOpenMoreReviewsModal] = useState(false);
  const [email, setEmail] = useState("");
  const [error, setError] = useState("");

  useEffect(() => {
    const fetchReviews = async () => {
      try {
        const response = await fetch(
          "http://localhost:8080/api/review/reviews/user/FetchReviews",
          {
            method: "POST",
            headers: {
              "Content-Type": "application/json",
            },
            body: JSON.stringify({
              review_rating_fetch_rq: {
                header: {
                  user_name: "endUser",
                  product: "rnr",
                  request_type: "FETCH_REVIEW_RATING",
                },
              },
            })
          }
        );

        const data = await response.json();
        const fetchedReviews =
          data.review_rating_fetch_rs?.review_rating_details_overall || [];
        setReviews(fetchedReviews);
      } catch (error) {
        console.error("Error fetching reviews:", error);
      }
    };

    fetchReviews();
  }, []);

  const handleShowMore = () => {
    setOpenMoreReviewsModal(true); // Open modal for all reviews
  };

  const handleSignIn = () => {
    const emailRegex = /^[^\s@]+@[^\s@]+\.[^\s@]+$/;
    if (!emailRegex.test(email)) {
      setError("Please enter a valid email address.");
      return;
    }

    console.log("Email entered:", email);
    setOpenReviewModal(false);
    setEmail("");
    setError("");
  };

  return (
    <Box sx={{ maxWidth: 1200, margin: "0 auto", padding: 4 }}>
      {/* Write a Review Button */}
      <Box textAlign="right" mb={3}>
        <Button
          variant="contained"
          sx={{
            backgroundColor: "#00bcd4",
            color: "#fff",
            borderRadius: 20,
            textTransform: "none",
            padding: "8px 16px",
            fontWeight: "bold",
          }}
          startIcon={<EditIcon />}
          onClick={() => setOpenReviewModal(true)}
        >
          Write A Review
        </Button>
      </Box>

      {/* Display Reviews */}
      {reviews.slice(0, visibleCount).map((review, index) => (
        <Card
          key={index}
          sx={{
            marginBottom: 2,
            boxShadow: 2,
            padding: 2,
            borderRadius: 3,
            backgroundColor: "#fff",
          }}
        >
          <CardContent>
            <Grid container spacing={2}>
              {/* Avatar */}
              <Grid item xs={12} md={2} textAlign="center">
                <Avatar
                  sx={{
                    width: 56,
                    height: 56,
                    margin: "0 auto",
                    backgroundColor: "#e6f7ff",
                  }}
                >
                  {review.review.charAt(0)}
                </Avatar>
                <Typography variant="body2" fontWeight="bold" mt={1}>
                  User {index + 1}
                </Typography>
              </Grid>

              {/* Review Content */}
              <Grid item xs={12} md={10}>
                <Typography variant="h6" fontWeight="bold">
                  {review.review}
                </Typography>
                <Typography variant="body2" paragraph>
                  Rating: {review.rating}/5
                </Typography>
              </Grid>
            </Grid>
          </CardContent>
        </Card>
      ))}

      {/* Show More Reviews Button */}
      {visibleCount < reviews.length && (
        <Box textAlign="center" mt={3}>
          <Button
            variant="outlined"
            onClick={handleShowMore}
            sx={{
              borderColor: "#00bcd4",
              color: "#00bcd4",
              borderRadius: 20,
              padding: "8px 16px",
              fontWeight: "bold",
              textTransform: "none",
            }}
          >
            + More Reviews
          </Button>
        </Box>
      )}

      {/* Modal for More Reviews */}
      <RatingReviewModal
        open={openMoreReviewsModal}
        onClose={() => setOpenMoreReviewsModal(false)}
        title="All Guest Reviews"
      >
        {/* Modal Header */}
        <Box>
          <Box display="flex" alignItems="center" justifyContent="space-between" mb={3}>
            <Typography variant="h5" fontWeight="bold">
              Guest Reviews
            </Typography>
            <Button
              variant="contained"
              sx={{
                backgroundColor: "#00bcd4",
                color: "#fff",
                borderRadius: 20,
                textTransform: "none",
                padding: "8px 16px",
                fontWeight: "bold",
              }}
              startIcon={<EditIcon />}
              onClick={() => setOpenReviewModal(true)}
            >
              Write A Review
            </Button>
          </Box>

          <Box display="flex" alignItems="center" gap={2} mb={2}>
            {/* Search Input */}
            <TextField
              variant="outlined"
              size="small"
              placeholder="Search Reviews"
              InputProps={{
                style: {
                  borderRadius: 20,
                  backgroundColor: "#f9f9f9",
                  paddingLeft: 8,
                },
              }}
              sx={{ flexGrow: 1 }}
            />
            {/* Sort Dropdown */}
            <TextField
              select
              size="small"
              defaultValue="Newest"
              SelectProps={{
                native: true,
              }}
              sx={{
                width: 150,
                backgroundColor: "#f9f9f9",
                borderRadius: 20,
              }}
            >
              <option value="Newest">Sort by: Newest</option>
              <option value="Oldest">Sort by: Oldest</option>
              <option value="Highest Rating">Sort by: Highest Rating</option>
              <option value="Lowest Rating">Sort by: Lowest Rating</option>
            </TextField>
          </Box>

          <Box display="flex" alignItems="center" justifyContent="space-between">
            {/* Filters */}
            <Box display="flex" gap={2}>
              {["Reviewers", "Ratings", "Languages", "Time"].map((filter, index) => (
                <TextField
                  key={index}
                  select
                  size="small"
                  defaultValue="All(2024)"
                  SelectProps={{
                    native: true,
                  }}
                  sx={{
                    minWidth: 150,
                    backgroundColor: "#f9f9f9",
                    borderRadius: 20,
                  }}
                >
                  <option value="All(2024)">All(2024)</option>
                  <option value="Custom">Custom</option>
                </TextField>
              ))}
            </Box>
          </Box>
        </Box>

        {/* Reviews List */}
        <Box mt={3}>
          {reviews.map((review, index) => (
            <Card
              key={index}
              sx={{
                marginBottom: 2,
                boxShadow: 2,
                padding: 2,
                borderRadius: 3,
                backgroundColor: "#fff",
              }}
            >
              <CardContent>
                <Grid container spacing={2}>
                  {/* Avatar */}
                  <Grid item xs={12} md={2} textAlign="center">
                    <Avatar
                      sx={{
                        width: 56,
                        height: 56,
                        margin: "0 auto",
                        backgroundColor: "#e6f7ff",
                      }}
                    >
                      {review.review.charAt(0)}
                    </Avatar>
                    <Typography variant="body2" fontWeight="bold" mt={1}>
                      User {index + 1}
                    </Typography>
                  </Grid>

                  {/* Review Content */}
                  <Grid item xs={12} md={10}>
                    <Typography variant="h6" fontWeight="bold">
                      {review.review}
                    </Typography>
                    <Typography variant="body2" paragraph>
                      Rating: {review.rating}/5
                    </Typography>
                  </Grid>
                </Grid>
              </CardContent>
            </Card>
          ))}
        </Box>
      </RatingReviewModal>

      {/* Modal for Writing Reviews */}
      <RatingReviewModal
        open={openReviewModal}
        onClose={() => setOpenReviewModal(false)}
        title="Want to write a review?"
      >
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
        <Box mt={2}>
          <Button
            variant="contained"
            color="primary"
            onClick={handleSignIn}
            sx={{
              textTransform: "none",
              borderRadius: 20,
              marginRight: 2,
            }}
          >
            Sign In
          </Button>
          <Button
            variant="outlined"
            color="secondary"
            onClick={() => setOpenReviewModal(false)}
            sx={{
              textTransform: "none",
              borderRadius: 20,
            }}
          >
            Cancel
          </Button>
        </Box>
      </RatingReviewModal>
    </Box>
  );
};

export default GuestReviews;
