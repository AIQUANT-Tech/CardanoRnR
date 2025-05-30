import React, { useEffect, useState } from "react";
import {
  Box,
  Typography,
  Button,
  Card,
  CardContent,
  Avatar,
  Grid,
} from "@mui/material";
import EditIcon from "@mui/icons-material/Edit";
import RatingReviewModal from "../../Components/RatingReviewModal";
import WriteReviewModal from "../../Components/WriteReviewModal";
import FullReviewModal from "../../Components/FullReviewModal";
import { format } from "date-fns";
import Star from "../../assets/Star.svg";
import business from "../../assets/businessProfile.svg";
import user from "../../assets/userProfile.svg";
import API_BASE_URL from "../../config.js";

const GuestReviews = () => {
  const [reviews, setReviews] = useState([]);
  const [businessReplies, setBusinessReplies] = useState([]);
  const [openReviewModal, setOpenReviewModal] = useState(false);
  const [openMoreReviewsModal, setOpenMoreReviewsModal] = useState(false);
  const [selectedReview, setSelectedReview] = useState(null);
  const [companyName, setCompanyName] = useState("");

  // Fetch reviews
  useEffect(() => {
    const storedCompanyName = localStorage.getItem("companyName");

    if (storedCompanyName) {
      setCompanyName(storedCompanyName);
    }

    const fetchReviews = async () => {
      try {
        const response = await fetch(
          `${API_BASE_URL}api/review/reviews/user/FetchReviews`,
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
            }),
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

  // Fetch business replies
  useEffect(() => {
    const fetchBusinessReplies = async () => {
      try {
        const response = await fetch(`${API_BASE_URL}api/reply/`, {
          method: "POST",
          headers: {
            "Content-Type": "application/json",
          },
        });

        const data = await response.json();
        if (data.success) {
          setBusinessReplies(data.data || []);
        }
        // console.log(data);
      } catch (error) {
        console.error("Error fetching business replies:", error);
      }
    };

    fetchBusinessReplies();
  }, []);

  // Map business replies to reviews
  const reviewsWithReplies = reviews.map((review) => {
    const matchedReply = businessReplies.find(
      (reply) => reply.review_id === review.review_id
    );
    return { ...review, business_reply: matchedReply?.content };
  });

  const handleShowReview = (review) => {
    setSelectedReview(review);
  };

  const handleCloseModal = () => {
    setSelectedReview(null);
  };

  return (
    <Box sx={{ maxWidth: 1200, margin: "0 auto", padding: 4 }}>
      <Box textAlign="right" mb={3}>
        <WriteReviewModal
          open={openReviewModal}
          onClose={() => setOpenReviewModal(false)}
          title="Want to write a review?"
        >
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
        </WriteReviewModal>
      </Box>

      {reviewsWithReplies.slice(0, 4).map((review, index) => (
        <Card
          key={index}
          sx={{
            marginBottom: 2,
            boxShadow: 2,
            padding: 2,
            borderRadius: 3,
            backgroundColor: "#fff",
            cursor: "pointer",
          }}
          onClick={() => handleShowReview(review)}
        >
          <CardContent>
            <Grid container spacing={2}>
              <Grid item xs={12} md={2} textAlign="center">
                <Avatar
                  sx={{
                    width: 56,
                    height: 56,
                    margin: "0 auto",
                    backgroundColor: "#e6f7ff",
                  }}
                >
                  <img
                    src={user}
                    alt=""
                    style={{ width: "30px", height: "30px" }}
                  />
                  {/* {review.review.charAt(0)} */}
                </Avatar>
                <Typography variant="body2" fontWeight="bold" mt={1}>
                  {review.user_name}
                </Typography>
              </Grid>
              <Grid item xs={12} md={10}>
                <Box bottom={19} left={8} display="flex" alignItems="center">
                  {[1, 2, 3, 4, 5].map((star) =>
                    review.rating >= star ? (
                      // <StarIcon
                      //     key={star}
                      //     sx={{
                      //         color:
                      //             star <= review.rating
                      //                 ? "#fdd835"
                      //                 : "#e0e0e0",
                      //         fontSize: 35,
                      //         position: "relative",
                      //         left: "80%"

                      //     }}
                      // />
                      <img
                        key={star}
                        src={Star}
                        style={{
                          cursor: "pointer",
                          width: 35,
                          height: 35,
                          position: "relative",
                          left: "80%",
                          paddingBottom: "2px",
                          padding: 2,
                        }}
                      />
                    ) : (
                      <span key={star}></span>
                    )
                  )}
                </Box>
                <Typography fontSize={16} fontWeight="bold">
                  {review.review}
                </Typography>

                {review.created_at && (
                  <Typography variant="body2" mt={2} color="text.secondary">
                    Created At:{" "}
                    {format(
                      new Date(review.created_at),
                      "MMM dd, yyyy hh:mm a"
                    )}
                  </Typography>
                )}
                {review && (
                  <Box>
                    <Typography variant="body2" color="textSecondary">
                      Room Type:{" "}
                      {review.booking_details.room_type.toUpperCase()} {" | "}
                      Stay On:{" "}
                      {new Date(
                        review.booking_details.check_out_date
                      ).toLocaleDateString()}
                    </Typography>
                  </Box>
                )}

                <Typography
                  variant="body2"
                  mt={2}
                  color="primary"
                  sx={{
                    position: "relative",
                    left: "90%",
                  }}
                >
                  See more...
                </Typography>

                {review.business_reply && (
                  <Box
                    mt={2}
                    p={2}
                    sx={{ backgroundColor: "#f9f9f9", borderRadius: 2 }}
                  >
                    <Typography
                      variant="body2"
                      fontWeight="bold"
                      color="primary"
                      sx={{ display: "flex" }}
                    >
                      <Avatar>
                        <img
                          src={business}
                          alt=""
                          style={{ height: "30px", width: "30px" }}
                        />
                      </Avatar>
                      <Typography
                        variant="body2"
                        fontWeight="bold"
                        color="primary"
                        sx={{
                          fontSize: 18,
                          display: "flex",
                          alignContent: "center",
                          flexWrap: "wrap",
                          paddingLeft: 1,
                        }}
                      >
                        {companyName}
                      </Typography>
                    </Typography>
                    <Typography variant="body2">
                      {review.business_reply}
                    </Typography>
                  </Box>
                )}
              </Grid>
            </Grid>
          </CardContent>
        </Card>
      ))}

      <Box textAlign="center" mt={3}>
        <Button
          variant="outlined"
          onClick={() => setOpenMoreReviewsModal(true)}
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

      <RatingReviewModal
        open={openMoreReviewsModal}
        onClose={() => setOpenMoreReviewsModal(false)}
        title="All Guest Reviews"
        sx={{
          padding: "8px 16px",
          fontWeight: "bold",
          textTransform: "none",
        }}
      />

      <FullReviewModal
        open={!!selectedReview}
        onClose={handleCloseModal}
        review={selectedReview}
      />
    </Box>
  );
};

export default GuestReviews;
