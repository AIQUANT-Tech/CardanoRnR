import React, { useState, useEffect } from "react";
import {
  Grid,
  Paper,
  Typography,
  Button,
  CircularProgress,
  LinearProgress,
  Box,
  Card,
  CardContent,
  Rating,
} from "@mui/material";

const ReviewsPage = () => {
  const [reviewsData, setReviewsData] = useState(null);
  const [loading, setLoading] = useState(true);

  // Fetch data from backend
  useEffect(() => {
    const fetchReviews = async () => {
      setLoading(true);
      const payload = {
        review_rating_fetch_rq: {
          header: {
            user_name: "endUser",
            product: "rnr",
            request_type: "FETCH_REVIEW_RATING",
          },
        },
      };

      try {
        const response = await fetch(
          "http://localhost:8080/api/review/reviews/user/FetchReviews",
          {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify(payload),
          }
        );

        if (!response.ok) {
          throw new Error("Failed to fetch reviews");
        }

        const data = await response.json();
        setReviewsData(data.review_rating_fetch_rs);
      } catch (error) {
        console.error(error);
      } finally {
        setLoading(false);
      }
    };

    fetchReviews();
  }, []);

  if (loading) {
    return (
      <Box display="flex" justifyContent="center" alignItems="center" height="100vh">
        <CircularProgress />
      </Box>
    );
  }

  if (!reviewsData) {
    return <Typography>No reviews available.</Typography>;
  }

  const {
    overall_rating,
    reputation_score,
    review_rating_details_overall,
    category_wise_review_rating,
  } = reviewsData;

  return (
    <Box p={4}>
      {/* Top Section */}
      <Grid container spacing={3}>
        {/* Reputation Score */}
        <Grid item xs={12} sm={4}>
          <Paper elevation={3} style={{ padding: "16px", textAlign: "center" }}>
            <Typography variant="h6">Reputation Score</Typography>
            <Typography variant="h3">{reputation_score * 20}/100</Typography>
            <Typography variant="body2">{review_rating_details_overall.length} verified reviews</Typography>
          </Paper>
        </Grid>

        {/* Overall Rating */}
        <Grid item xs={12} sm={8}>
          <Paper elevation={3} style={{ padding: "16px" }}>
            <Typography variant="h6">Ratings & Reviews</Typography>
            <Typography variant="h4">
              {overall_rating} <Rating value={overall_rating} readOnly />
            </Typography>
            <Typography variant="body2">{review_rating_details_overall.length} Real Reviews</Typography>
          </Paper>
        </Grid>
      </Grid>

      {/* Category Ratings */}
      <Box mt={4}>
        <Typography variant="h6">Guest Categories Ratings</Typography>
        <Grid container spacing={2}>
          {category_wise_review_rating.map((category) => (
            <Grid item xs={12} sm={4} key={category.category_name}>
              <Card>
                <CardContent>
                  <Typography variant="subtitle1">{category.category_desc}</Typography>
                  <LinearProgress
                    variant="determinate"
                    value={
                      (category.review_rating_details_by_category.reduce((sum, r) => sum + r.rating, 0) /
                        category.review_rating_details_by_category.length) *
                      20
                    }
                  />
                  <Typography variant="body2" align="right">
                    {(
                      category.review_rating_details_by_category.reduce((sum, r) => sum + r.rating, 0) /
                      category.review_rating_details_by_category.length
                    ).toFixed(1)}
                  </Typography>
                </CardContent>
              </Card>
            </Grid>
          ))}
        </Grid>
      </Box>

      {/* Reviews Section */}
      <Box mt={4}>
        <Typography variant="h6">Guest Reviews</Typography>
        <Grid container spacing={2}>
          {review_rating_details_overall.map((review, index) => (
            <Grid item xs={12} key={index}>
              <Paper elevation={1} style={{ padding: "16px" }}>
                <Typography variant="body1">{review.review}</Typography>
                <Rating value={review.rating} readOnly />
              </Paper>
            </Grid>
          ))}
        </Grid>
      </Box>

      {/* Actions */}
      <Box mt={4} textAlign="center">
        <Button variant="contained" color="primary" style={{ margin: "8px" }}>
          Write A Review
        </Button>
        <Button variant="outlined" color="secondary">
          More Reviews
        </Button>
      </Box>
    </Box>
  );
};

export default ReviewsPage;
