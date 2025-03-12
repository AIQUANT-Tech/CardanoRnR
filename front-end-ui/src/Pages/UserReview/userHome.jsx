import React, { useEffect, useState } from "react";
import { Box, Typography, Button, LinearProgress, Grid } from "@mui/material";
import StarIcon from "@mui/icons-material/Star";
import GuestReviews from "./guestReviews";
import { useParams } from "react-router-dom";
import API_BASE_URL from "../../config.js";

const ReviewsPage = () => {
  const [data, setData] = useState(null);
  const [loading, setLoading] = useState(true);
  const [showAllCategories, setShowAllCategories] = useState(false);
  const { companyName } = useParams();

  useEffect(() => {
    const fetchData = async () => {
      setLoading(true);
      try {
        const response = await fetch(
          `${API_BASE_URL}api/review/reviews/user/FetchReviews`,
          {
            method: "POST",
            headers: { "Content-Type": "application/json" },
            body: JSON.stringify({
              review_rating_fetch_rq: {
                header: {
                  user_name: "endUser",
                  product: "rnr",
                  request_type: "FETCH_REVIEW_RATING",
                },
              },
            }),
          }
        );

        if (!response.ok) {
          throw new Error("Failed to fetch data.");
        }

        const result = await response.json();
        setData(result.review_rating_fetch_rs);
      } catch (error) {
        console.error(error);
      } finally {
        setLoading(false);
      }
    };

    fetchData();
  }, []);

  useEffect(() => {
    localStorage.setItem("companyName", companyName);
  }, [companyName]);

  if (loading) {
    return (
      <Box sx={{ textAlign: "center", marginTop: "20px" }}>
        <Typography variant="h6">Loading...</Typography>
      </Box>
    );
  }

  if (!data) {
    return (
      <Box sx={{ textAlign: "center", marginTop: "20px" }}>
        <Typography variant="h6">No data available</Typography>
      </Box>
    );
  }

  const {
    reputation_score,
    category_wise_review_rating,
    review_rating_details_overall,
  } = data;

  // Calculate Guest Overall Rating
  const guestOverallRating =
    review_rating_details_overall && review_rating_details_overall.length > 0
      ? (
          review_rating_details_overall.reduce(
            (sum, review) => sum + review.rating,
            0
          ) / review_rating_details_overall.length
        ).toFixed(1)
      : 0;

  // Calculate percentages for each star rating
  const starCounts = { 5: 0, 4: 0, 3: 0, 2: 0, 1: 0 };
  review_rating_details_overall?.forEach((review) => {
    const roundedRating = Math.round(review.rating);
    if (starCounts[roundedRating] !== undefined) {
      starCounts[roundedRating]++;
    }
  });

  const totalReviews = review_rating_details_overall?.length || 0;
  const starPercentages = Object.keys(starCounts).map((star) => {
    return totalReviews > 0 ? (starCounts[star] / totalReviews) * 100 : 0;
  });

  // Determine the categories to show based on "showAllCategories" state
  const visibleCategories = showAllCategories
    ? category_wise_review_rating
    : category_wise_review_rating.slice(0, 3);

  return (
    <>
      <Box sx={{ maxWidth: 1200, margin: "0 auto", padding: 4 }}>
        <Typography variant="h4" align="center" fontWeight="bold" gutterBottom>
          Ratings & Reviews
        </Typography>
        <Typography variant="h5" align="center" fontWeight="bold" gutterBottom>
          {guestOverallRating}{" "}
          <StarIcon sx={{ color: "#ffa500", height: 40, width: 40 }} />{" "}
          {guestOverallRating >= 4
            ? "Good"
            : guestOverallRating >= 3
            ? "Average"
            : "Bad"}
        </Typography>
        <Typography align="center" color="textSecondary">
          {totalReviews} Real Reviews
        </Typography>

        <Grid container spacing={4}>
          <Grid item xs={12} md={4}>
            <Box
              sx={{
                textAlign: "center",
                backgroundColor: "#65CBD94D",
                borderRadius: 50,
                paddingTop: 5,
                width: "250px",
                height: "250px",
              }}
            >
              <Typography variant="h6" color="primary" gutterBottom>
                Reputation Score
              </Typography>
              <Typography variant="h2" fontWeight="bold" color="#333">
                {Math.min(reputation_score, 100)}
                <br></br>/100
              </Typography>
            </Box>
            <Typography color="textSecondary" sx={{ padding: 5 }}>
              {totalReviews} verified reviews
            </Typography>
          </Grid>

          {/* Overall Rating */}
          <Grid item xs={12} md={8}>
            <Box
              sx={{
                textAlign: "center",
                backgroundColor: "#fff",
                borderRadius: 3,
                padding: 3,
                boxShadow: 1,
              }}
            >
              <Box mt={3}>
                <Typography variant="body1" fontWeight="bold" gutterBottom>
                  Guest Overall Ratings
                </Typography>

                {[5, 4, 3, 2, 1].map((star, index) => (
                  <Box key={index} display="flex" alignItems="center" mb={1}>
                    <Typography variant="body2" width={30}>
                      {star}
                    </Typography>
                    <LinearProgress
                      variant="determinate"
                      value={starPercentages[star - 1]}
                      sx={{
                        width: "100%",
                        marginX: 2,
                        height: 8,
                        backgroundColor: "#eaeaea",
                        "& .MuiLinearProgress-bar": {
                          backgroundColor: "#00bcd4",
                        },
                      }}
                    />
                    <Typography variant="body2" ml={2}>
                      {Math.round(starPercentages[star - 1])}%
                    </Typography>
                  </Box>
                ))}
              </Box>
            </Box>
          </Grid>
        </Grid>

        <Box mt={4} sx={{ marginLeft: "35%" }}>
          <Typography variant="h6" fontWeight="bold" gutterBottom>
            Guest Categories Ratings
          </Typography>

          {visibleCategories.map((category, index) => (
            <Box key={index} display="flex" alignItems="center" mb={2}>
              <Typography variant="body1" width={120}>
                {category.category_name}
              </Typography>
              <LinearProgress
                variant="determinate"
                value={
                  (category.review_rating_details_by_category.reduce(
                    (sum, r) => sum + r.rating,
                    0
                  ) /
                    category.review_rating_details_by_category.length) *
                  20
                }
                sx={{
                  flexGrow: 1,
                  height: 8,
                  marginX: 2,
                  backgroundColor: "#eaeaea",
                  "& .MuiLinearProgress-bar": {
                    backgroundColor: "#00bcd4",
                  },
                }}
              />
              <Typography variant="body2" fontWeight="bold">
                {(
                  category.review_rating_details_by_category.reduce(
                    (sum, r) => sum + r.rating,
                    0
                  ) / category.review_rating_details_by_category.length
                ).toFixed(1)}
              </Typography>
            </Box>
          ))}

          <Button
            variant="outlined"
            onClick={() => setShowAllCategories((prev) => !prev)}
            sx={{
              borderColor: "#00bcd4",
              color: "#00bcd4",
              borderRadius: 20,
              marginTop: 2,
            }}
          >
            {showAllCategories ? "Show Less" : "+ More Categories"}
          </Button>
        </Box>
      </Box>

      <GuestReviews reviews={review_rating_details_overall} />
    </>
  );
};

export default ReviewsPage;
