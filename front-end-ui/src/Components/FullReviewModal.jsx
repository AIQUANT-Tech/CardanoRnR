import React, { useEffect, useState } from "react";
import { Box, Typography, Modal, CircularProgress } from "@mui/material";
import { format } from 'date-fns';

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
        <Modal open={open} onClose={onClose}>
            <Box
                sx={{
                    position: "absolute",
                    top: "50%",
                    left: "50%",
                    transform: "translate(-50%, -50%)",
                    width: 500,
                    bgcolor: "background.paper",
                    borderRadius: 2,
                    boxShadow: 24,
                    p: 4,
                    maxHeight: "80vh",
                    overflowY: "auto",
                }}
            >
                {loading ? (
                    <Box textAlign="center">
                        <CircularProgress />
                        <Typography variant="body2" mt={2}>
                            Loading review details...
                        </Typography>
                    </Box>
                ) : error ? (
                    <Typography variant="body2" color="error">
                        {error}
                    </Typography>
                ) : fullReviewDetails ? (
                    <>
                        <Typography variant="h6" fontWeight="bold" gutterBottom>
                            Overall Review
                        </Typography>
                        <Typography variant="body1" mt={2}>
                            {fullReviewDetails.overall_review}
                        </Typography>
                        <Typography variant="body2" mt={1}>
                            Overall Rating: {fullReviewDetails.overall_rating}/5
                        </Typography>
                        
                        {fullReviewDetails.created_at && (
                            <Typography variant="body2" mt={2} color="text.secondary">
                                Created At: {format(new Date(fullReviewDetails.created_at), 'MMM dd, yyyy hh:mm a')}
                            </Typography>
                        )}

                        <Typography variant="h6" fontWeight="bold" mt={3} gutterBottom>
                            Category-wise Reviews
                        </Typography>
                        {fullReviewDetails.category_wise_reviews.map((category, index) => (
                            <Box key={index} mt={2}>
                                <Typography variant="subtitle1" fontWeight="bold">
                                    {category.category_name}
                                </Typography>
                                <Typography variant="body2" fontStyle="italic" color="text.secondary">
                                    {category.category_desc}
                                </Typography>
                                {category.reviews.map((catReview, idx) => (
                                    <Typography variant="body2" key={idx} mt={1}>
                                        - {catReview.review} (Rating: {catReview.rating}/5)
                                    </Typography>
                                ))}
                            </Box>
                        ))}
                    </>
                ) : (
                    <Typography variant="body2">No review details available.</Typography>
                )}
            </Box>
        </Modal>
    );
};

export default FullReviewModal;
