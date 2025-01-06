import React, { useEffect, useState } from "react";
import {
    Dialog,
    Button,
    Box,
    Typography,
    Card,
    CardContent,
    Avatar,
    TextField,
    Grid,
} from "@mui/material";
import { EditIcon } from "lucide-react";
import WriteReviewModal from "./WriteReviewModal";
import FullReviewModal from "./FullReviewModal";
import { format } from "date-fns";
import Star from "../assets/Star.png";


const debounce = (func, delay) => {
    let timeout;
    return (...args) => {
        clearTimeout(timeout);
        timeout = setTimeout(() => func(...args), delay);
    };
};

const RatingReviewModal = ({
    open,
    onClose,
    header,
    children,
    closeButtonText,
}) => {
    const [openReviewModal, setOpenReviewModal] = useState(false);
    const [reviews, setReviews] = useState([]);
    const [filteredReviews, setFilteredReviews] = useState([]);
    const [searchQuery, setSearchQuery] = useState("");
    const [sortOption, setSortOption] = useState("Newest");
    const [selectedReview, setSelectedReview] = useState(null);
    const [visibleReviews, setVisibleReviews] = useState(4); // Track number of visible reviews

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
                        }),
                    }
                );

                const data = await response.json();
                const fetchedReviews =
                    data.review_rating_fetch_rs?.review_rating_details_overall || [];
                setReviews(fetchedReviews);
                setFilteredReviews(fetchedReviews);
            } catch (error) {
                console.error("Error fetching reviews:", error);
            }
        };

        fetchReviews();
    }, []);

    const handleSearch = debounce((query) => {
        if (query.trim() === "") {
            setFilteredReviews(reviews);
        } else {
            const filtered = reviews.filter((review) =>
                review.review.toLowerCase().includes(query.toLowerCase()) ||
                String(review.rating).includes(query) ||
                String(review.user_name).toLowerCase().includes(query)
            );
            
            setFilteredReviews(filtered);
        }
    }, 500);

    const handleSearchInputChange = (e) => {
        const query = e.target.value;
        setSearchQuery(query);
        handleSearch(query);
    };

    const handleSortChange = (event) => {
        const selectedOption = event.target.value;
        setSortOption(selectedOption);
        let sortedReviews;

        if (selectedOption === "Newest") {
            sortedReviews = [...filteredReviews].sort(
                (a, b) => new Date(b.created_at) - new Date(a.created_at)
            );
        } else if (selectedOption === "Oldest") {
            sortedReviews = [...filteredReviews].sort(
                (a, b) => new Date(a.created_at) - new Date(b.created_at)
            );
        } else if (selectedOption === "Highest Rating") {
            sortedReviews = [...filteredReviews].sort((a, b) => b.rating - a.rating);
        } else if (selectedOption === "Lowest Rating") {
            sortedReviews = [...filteredReviews].sort((a, b) => a.rating - b.rating);
        }

        setFilteredReviews(sortedReviews);
    };

    const handleShowReview = (review) => {
        setSelectedReview(review);
    };

    const handleCloseModal = () => {
        setSelectedReview(null);
    };

    const handleLoadMore = () => {
        setVisibleReviews((prevVisible) => prevVisible + 5); // Load 5 more reviews
    };

    return (
        <Dialog open={open} onClose={onClose} fullWidth maxWidth="md">
            <Box sx={{ padding: 2, borderBottom: "1px solid #ccc" }}>
                <Typography variant="h5" fontWeight="bold">
                    Guest Reviews
                </Typography>
            </Box>

            <Box sx={{ padding: 2 }}>
                <Box display="flex" alignItems="center" justifyContent="space-between" mb={2}>
                    <TextField
                        variant="outlined"
                        size="small"
                        placeholder="Search Reviews"
                        value={searchQuery}
                        onChange={handleSearchInputChange}
                        InputProps={{
                            style: {
                                borderRadius: 20,
                                backgroundColor: "#f9f9f9",
                                paddingLeft: 8,
                            },
                        }}
                        sx={{ flexGrow: 1, marginRight: 2 }}
                    />
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

                <Box display="flex" gap={2} mb={2}>
                    <TextField
                        select
                        size="small"
                        value={sortOption}
                        onChange={handleSortChange}
                        SelectProps={{ native: true }}
                        sx={{
                            width: 180,
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

                <Box>
                    {filteredReviews.slice(0, visibleReviews).map((review, index) => (
                        <Card
                            key={index}
                            sx={{
                                marginBottom: 2,
                                boxShadow: 2,
                                padding: 2,
                                borderRadius: 3,
                                backgroundColor: "#fff",
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

                                    <Grid item xs={12} md={10} position="relative">
                                        {/* Rating as Stars */}
                                        <Box
                                            bottom={19}
                                            display="flex"
                                            alignItems="center"
                                            position="relative"
                                            left="75%"
                                        >
                                            {[1, 2, 3, 4, 5].map((star) => (
                                                review.rating >= star ? (
                                                    <img
                                                        key={star}
                                                        src={Star}
                                                        alt="star"
                                                        style={{
                                                            cursor: "pointer",
                                                            width: 32,
                                                            height: 32,

                                                        }}
                                                    />
                                                ) : (<span key={star}></span>
                                                )
                                            ))}
                                        </Box>
                                        <Typography variant="h10" fontWeight="bold">
                                            {review.review}
                                        </Typography>
                                        {review.created_at && (
                                            <Typography
                                                variant="body2"
                                                mt={2}
                                                color="text.secondary"
                                            >
                                                Created At: {format(
                                                    new Date(review.created_at),
                                                    "MMM dd, yyyy hh:mm a"
                                                )}
                                            </Typography>
                                        )}
                                    </Grid>
                                </Grid>
                            </CardContent>
                        </Card>
                    ))}
                </Box>

                {filteredReviews.length > visibleReviews && (
                    <Button
                        variant="outlined"
                        color="primary"
                        onClick={handleLoadMore}
                        sx={{
                            marginLeft: "40%",
                            textTransform: "none",
                            borderRadius: 20,
                        }}
                    >
                        Show More Reviews
                    </Button>
                )}

                <Button
                    variant="outlined"
                    color="secondary"
                    onClick={onClose}
                    sx={{ textTransform: "none", borderRadius: 20 }}
                >
                    {closeButtonText || "Cancel"}
                </Button>
            </Box>

            <FullReviewModal
                open={!!selectedReview}
                onClose={handleCloseModal}
                review={selectedReview}
            />
        </Dialog>
    );
};

export default RatingReviewModal;
