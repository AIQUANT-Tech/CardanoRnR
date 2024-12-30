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

const GuestReviews = () => {
    const [reviews, setReviews] = useState([]);
    const [visibleCount, setVisibleCount] = useState(3);
    const [openReviewModal, setOpenReviewModal] = useState(false);
    const [openMoreReviewsModal, setOpenMoreReviewsModal] = useState(false);
    const [selectedReview, setSelectedReview] = useState(null);

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
        setOpenMoreReviewsModal(true);
    };

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

            {reviews.slice(0, visibleCount).map((review, index) => (
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
                                    {review.review.charAt(0)}
                                </Avatar>
                                <Typography variant="body2" fontWeight="bold" mt={1}>
                                    {review.user_name}
                                </Typography>
                            </Grid>
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

            {visibleCount < reviews.length && (
                <Box textAlign="center" mt={3}>
                    <Button
                        variant="outlined"
                        onClick={() => setVisibleCount(visibleCount + 3)}
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
