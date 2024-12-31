import React, { useEffect, useState } from "react";
import {
    Box,
    Typography,
    Modal,
    CircularProgress,
    Divider,
    Grid,
} from "@mui/material";
import { format } from "date-fns";
import StarIcon from "@mui/icons-material/Star";

const FullReviewModal = ({ open, onClose, review }) => {
    const [fullReviewDetails, setFullReviewDetails] = useState(null);
    const [loading, setLoading] = useState(false);
    const [error, setError] = useState(null);

    useEffect(() => {
        if (open && review?.user_id) {
            const fetchReviewDetails = async () => {
                setLoading(true);
                setError(null);
                try {
                    const response = await fetch(
                        "http://localhost:8080/api/review/reviews/user/selected",
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
                    maxWidth: 600,
                    bgcolor: "background.paper",
                    borderRadius: 3,
                    boxShadow: 24,
                    p: 4,
                    maxHeight: "85vh",
                    overflowY: "auto",
                    animation: "fadeIn 0.3s ease-in-out",
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
                    <Typography
                        variant="body2"
                        color="error"
                        textAlign="center"
                        py={4}
                    >
                        {error}
                    </Typography>
                ) : fullReviewDetails ? (
                    <>
                        {/* <Typography
                            variant="h5"
                            fontWeight="bold"
                            gutterBottom
                            id="review-modal-title"
                            sx={{
                                textAlign: "center",
                                background: "linear-gradient(90deg, #2196F3, #21CBF3)",
                                WebkitBackgroundClip: "text",
                                WebkitTextFillColor: "transparent",
                                mb: 2,
                            }}
                        >
                        </Typography> */}
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
                                <Grid item>
                                    {[...Array(fullReviewDetails.overall_rating)].map((_, i) => (
                                        <StarIcon
                                            key={i}
                                            sx={{
                                                color: "#FFD700",
                                                fontSize: 15,
                                            }}
                                        />
                                    ))}
                                </Grid>
                            </Grid>
                            {fullReviewDetails.created_at && (
                                <Typography
                                    variant="body2"
                                    mt={2}
                                    color="text.secondary"
                                >
                                    Created At:{" "}
                                    {format(
                                        new Date(fullReviewDetails.created_at),
                                        "MMM dd, yyyy hh:mm a"
                                    )}
                                </Typography>
                            )}
                        </Box>
                        <Divider sx={{ mb: 3 }} />
                        <Typography
                            variant="h6"
                            fontWeight="bold"
                            gutterBottom
                            sx={{ color: "text.primary" }}
                        >
                            Category-wise Reviews
                        </Typography>
                        {fullReviewDetails.category_wise_reviews.map((category, index) => (
                            <Box
                                key={index}
                                sx={{
                                    p: 2,
                                    mb: 2,
                                    bgcolor:
                                        index % 2 === 0
                                            ? "background.default"
                                            : "background.paper",
                                    borderRadius: 2,
                                    boxShadow: 1,
                                    transition: "all 0.3s ease",
                                    "&:hover": {
                                        boxShadow: 3,
                                    },
                                }}
                            >
                                <Typography
                                    variant="subtitle1"
                                    fontWeight="bold"
                                    gutterBottom
                                >
                                    {category.category_name}
                                </Typography>
                                <Typography
                                    variant="body2"
                                    fontStyle="italic"
                                    color="text.secondary"
                                    sx={{ mb: 1 }}
                                >
                                    {category.category_desc}
                                </Typography>
                                {category.reviews.map((catReview, idx) => (
                                    <Typography variant="body2" key={idx}>
                                        - {catReview.review}{" "}
                                        <Typography
                                            component="span"
                                            color="primary"
                                        >
                                            {[1, 2, 3, 4, 5].map((star) => (
                                                <StarIcon
                                                    key={star}
                                                    // onClick={() => handleCategoryRating(index, star)}
                                                    sx={{
                                                        cursor: "pointer",
                                                        color: catReview.rating >= star ? "#fdd835" : "#e0e0e0",
                                                        fontSize: 28,
                                                    }}
                                                />
                                            ))}
                                        </Typography>
                                    </Typography>
                                ))}
                            </Box>
                        ))}
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
