import React, { useEffect, useState } from "react";
import {
  Box,
  Typography,
  Modal,
  CircularProgress,
  Divider,
  Grid,
  Button,
} from "@mui/material";
import { format } from "date-fns";
import { motion } from "framer-motion";
import Star from "../assets/Star.svg";
import API_BASE_URL from "../config.js";
import { differenceInCalendarDays } from "date-fns";


const FullReviewModal = ({ open, onClose, review }) => {
  const [fullReviewDetails, setFullReviewDetails] = useState(null);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState(null);
const [adminReplies, setAdminReplies] = useState([]);

const checkIn = review?.booking_details?.check_in_date
  ? new Date(review.booking_details.check_in_date)
  : null;

const checkOut = review?.booking_details?.check_out_date
  ? new Date(review.booking_details.check_out_date)
  : null;

let nights = null;
if (checkIn && checkOut) {
  nights = differenceInCalendarDays(checkOut, checkIn);

  // safety: if your DB has swapped dates (as in your sample), make it positive
  nights = Math.abs(nights);
}


useEffect(() => {
  if (!open || !review) return;

  const fetchAdminReplies = async () => {
    try {
      const response = await fetch(`${API_BASE_URL}api/reply/`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({
          user_id: review.user_id, // fetch all replies for this user
        }),
      });

      const data = await response.json();

      if (data?.success && Array.isArray(data.data)) {
        setAdminReplies(data.data);
      } else {
        setAdminReplies([]);
      }
    } catch (err) {
      console.error("Admin reply fetch failed", err);
      setAdminReplies([]);
    }
  };

  fetchAdminReplies();
}, [open, review]);



useEffect(() => {
  if (!open) {
    setAdminReplies([]);
  }
}, [open]);





const overallAdminReply = adminReplies.find(
  (r) => r.review_id === review?.overall_review_id
);




  useEffect(() => {
    if (open && review?.user_id) {
      const fetchReviewDetails = async () => {
        setLoading(true);
        setError(null);
        try {
          const response = await fetch(
            `${API_BASE_URL}api/review/reviews/user/selected`,
            {
              method: "POST",
              headers: {
                "Content-Type": "application/json",
              },
              body: JSON.stringify({ user_id: review.user_id }),
            }
          );

          if (!response.ok) {
            throw new Error("Failed to fetch review details");
          }

          const responseData = await response.json();
          setFullReviewDetails(responseData?.data || null);
        } catch (error) {
          setError(error.message);
        } finally {
          setLoading(false);
        }
      };

      fetchReviewDetails();
    }
  }, [open, review]);

  return (
    <Modal open={open} onClose={onClose} aria-labelledby="review-modal-title">
      <Box
        sx={{
          position: "absolute",
          top: "50%",
          left: "50%",
          transform: "translate(-50%, -50%)",
          width: "90%",
          maxWidth: 800,
          bgcolor: "#ddf4f7",
          borderRadius: 5,
          p: 4,
          maxHeight: "85vh",
          overflowY: "auto",
          animation: "fadeIn 0.3s ease-in-out",
          "&::-webkit-scrollbar": {
            width: "8px",
          },
          "&::-webkit-scrollbar-track": {
            backgroundColor: "#f1f1f1",
            borderRadius: "50px",
          },
          "&::-webkit-scrollbar-thumb": {
            backgroundColor: "#888",
            borderRadius: "50px",
          },
          "&::-webkit-scrollbar-thumb:hover": {
            backgroundColor: "#555",
          },
        }}
      >
        {loading ? (
          <Box
            display="flex"
            flexDirection="column"
            alignItems="center"
            justifyContent="center"
            py={4}
          >
            <CircularProgress />
            <Typography variant="body2" mt={2}>
              Loading review details...
            </Typography>
          </Box>
        ) : error ? (
          <Typography variant="body2" color="error" textAlign="center" py={4}>
            {error}
          </Typography>
        ) : fullReviewDetails ? (
          <>
            <Typography
              variant="h5"
              fontWeight="bold"
              gutterBottom
              id="review-modal-title"
              sx={{
                textAlign: "center",
                background: "#000",
                WebkitBackgroundClip: "text",
                WebkitTextFillColor: "transparent",
                mb: 2,
                fontSize: "2rem",
                textTransform: "uppercase",
                letterSpacing: "2px",
              }}
            >
              {review?.user_name}
            </Typography>
            <Divider sx={{ mb: 2 }} />
            <Box mb={3}>
              <Typography
                variant="subtitle1"
                fontWeight="bold"
                gutterBottom
                sx={{ color: "text.primary" }}
              >
                Overall Review:
              </Typography>
              <Typography variant="body1" sx={{ mb: 2 }}>
                {fullReviewDetails.overall_review}
              </Typography>
              <Grid container spacing={1} alignItems="center">
                <Grid item>
                  <Typography variant="body2" color="text.secondary">
                    Overall Rating:
                  </Typography>
                </Grid>
                <Grid item sx={{ display: "flex" }}>
                  {[
                    ...Array(
                      Math.max(
                        0,
                        Math.min(
                          5,
                          parseInt(fullReviewDetails?.overall_rating, 10) || 0,
                        ),
                      ),
                    ),
                  ].map((_, i) => (
                    <motion.div
                      key={i}
                      whileHover={{ scale: 1.3 }}
                      transition={{ duration: 0.2 }}
                    >
                      <img
                        src={Star}
                        alt="star"
                        style={{
                          cursor: "pointer",
                          width: 40,
                          height: 40,
                          padding: 3,
                        }}
                      />
                    </motion.div>
                  ))}
                </Grid>
              </Grid>
              {fullReviewDetails.created_at && (
                <Typography variant="body2" mt={2} color="text.secondary">
                  Created At:{" "}
                  {format(
                    new Date(fullReviewDetails.created_at),
                    "MMM dd, yyyy hh:mm a",
                  )}
                </Typography>
              )}
              {review?.booking_details && (
                <Box
                  mt={2}
                  p={2}
                  sx={{ bgcolor: "#fff", borderRadius: 2, boxShadow: 1 }}
                >
                  <Typography variant="subtitle2" fontWeight="bold">
                    Stay Details
                  </Typography>

                  <Typography variant="body2" mt={1}>
                    Room Type:{" "}
                    <b>
                      {review.booking_details?.room_type?.toUpperCase() ??
                        "N/A"}
                    </b>
                  </Typography>

                  <Typography variant="body2" mt={0.5}>
                    Check In:{" "}
                    <b>
                      {review.booking_details?.check_in_date
                        ? format(
                            new Date(review.booking_details.check_in_date),
                            "MMM dd, yyyy",
                          )
                        : "N/A"}
                    </b>
                  </Typography>

                  <Typography variant="body2" mt={0.5}>
                    Check Out:{" "}
                    <b>
                      {review.booking_details?.check_out_date
                        ? format(
                            new Date(review.booking_details.check_out_date),
                            "MMM dd, yyyy",
                          )
                        : "N/A"}
                    </b>
                  </Typography>

                  {/* ✅ Duration */}
                  {checkIn && checkOut && (
                    <Typography variant="body2" mt={0.5}>
                      Stay Duration:{" "}
                      <b>
                        {Math.abs(differenceInCalendarDays(checkOut, checkIn))}{" "}
                        night
                        {Math.abs(
                          differenceInCalendarDays(checkOut, checkIn),
                        ) === 1
                          ? ""
                          : "s"}
                      </b>
                    </Typography>
                  )}
                </Box>
              )}
            </Box>

            <Divider sx={{ mb: 3 }} />
            {/* <Typography
                            variant="h6"
                            fontWeight="bold"
                            gutterBottom
                            sx={{ color: "text.primary" }}
                        >
                             Reviews
                        </Typography> */}
            {fullReviewDetails.category_wise_reviews.map((category, index) => (
              <Box
                key={index}
                sx={{
                  p: 2,
                  mb: 2,
                  bgcolor:
                    index % 2 === 0 ? "background.default" : "background.paper",
                  borderRadius: 2,
                  boxShadow: 1,
                  transition: "all 0.3s ease",
                  "&:hover": {
                    boxShadow: 4,
                  },
                }}
              >
                <Typography
                  variant="subtitle1"
                  fontWeight="bold"
                  gutterBottom
                  display="flex"
                >
                  {category.category_name}
                  {category.reviews.map((catReview, idx) => (
                    <Typography variant="body2" key={idx}>
                      <Typography
                        component="span"
                        color="primary"
                        position="absolute"
                        left="65%"
                      >
                        {[1, 2, 3, 4, 5].map((star) =>
                          catReview.rating >= star ? (
                            <img
                              key={star}
                              src={Star}
                              alt="star"
                              style={{
                                cursor: "pointer",
                                width: 40,
                                height: 40,
                                padding: 2,
                              }}
                            />
                          ) : (
                            <span key={star}></span>
                          ),
                        )}
                      </Typography>
                    </Typography>
                  ))}
                </Typography>
                <Typography
                  variant="body2"
                  fontStyle="italic"
                  color="text.secondary"
                  sx={{ mb: 1 }}
                >
                  {category.category_desc}
                </Typography>
                {category.reviews.map((catReview) => {
                  const reply = adminReplies.find(
                    (r) => r.review_id === catReview.review_id,
                  );

                  return (
                    <Box key={catReview.review_id} sx={{ mt: 1 }}>
                      <Typography variant="body2">
                        - {catReview.review}
                      </Typography>

                      {reply && (
                        <Typography
                          variant="body2"
                          sx={{
                            mt: 0.5,
                            ml: 2,
                            fontStyle: "italic",
                            color: "text.secondary",
                          }}
                        >
                          Admin: {reply.content}
                        </Typography>
                      )}
                    </Box>
                  );
                })}
              </Box>
            ))}
            {overallAdminReply && (
              <Box sx={{ mt: 3, p: 2, borderRadius: 2, bgcolor: "#f9f2f2" }}>
                <Typography variant="subtitle1" fontWeight="bold">
                  Admin Reply (Overall):
                </Typography>

                <Typography variant="body1">
                  {overallAdminReply.content}
                </Typography>

                {overallAdminReply.created_at && (
                  <Typography variant="caption" color="text.secondary">
                    {format(
                      new Date(overallAdminReply.created_at),
                      "MMM dd, yyyy hh:mm a",
                    )}
                  </Typography>
                )}
              </Box>
            )}

            <Button
              variant="contained"
              color="primary"
              onClick={onClose}
              sx={{
                mt: 3,
                fontWeight: "bold",
                padding: "10px 20px",
                borderRadius: 2,
                textTransform: "uppercase",
                transition: "all 0.3s ease",
                "&:hover": {
                  backgroundColor: "blue",
                  boxShadow: "0 8px 15px rgba(0, 0, 0, 0.2)",
                },
              }}
            >
              Close
            </Button>
          </>
        ) : (
          <Typography
            variant="body2"
            textAlign="center"
            py={4}
            color="text.secondary"
          >
            No review details available.
          </Typography>
        )}
      </Box>
    </Modal>
  );
};

export default FullReviewModal;
