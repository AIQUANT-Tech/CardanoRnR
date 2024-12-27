import React, { useEffect, useState } from "react";
import {
    Dialog,
    DialogContent,
    DialogActions,
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

// Debounce function to delay the search request.
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

    // Fetch the reviews when the modal is opened.
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
                setFilteredReviews(fetchedReviews); // Initialize filteredReviews
            } catch (error) {
                console.error("Error fetching reviews:", error);
            }
        };

        fetchReviews();
    }, []);

    // Debounced search function
    const handleSearch = debounce((query) => {
        if (query.trim() === "") {
            setFilteredReviews(reviews); // Show all if search is cleared
        } else {
            const filtered = reviews.filter((review) =>
                review.review.toLowerCase().includes(query.toLowerCase()) ||
                String(review.rating).includes(query) // Can add more fields here
            );
            setFilteredReviews(filtered);
        }
    }, 500); // Adjust debounce delay as needed (500ms)

    // Handle the input change and search
    const handleSearchInputChange = (e) => {
        const query = e.target.value;
        setSearchQuery(query);
        handleSearch(query);
    };

    // Sort function
    const handleSortChange = (event) => {
        const selectedOption = event.target.value;
        setSortOption(selectedOption);
        let sortedReviews;

        if (selectedOption === "Newest") {
            sortedReviews = [...filteredReviews].sort((a, b) => new Date(b.date) - new Date(a.date)); // Assuming `date` field exists
        } else if (selectedOption === "Oldest") {
            sortedReviews = [...filteredReviews].sort((a, b) => new Date(a.date) - new Date(b.date));
        } else if (selectedOption === "Highest Rating") {
            sortedReviews = [...filteredReviews].sort((a, b) => b.rating - a.rating);
        } else if (selectedOption === "Lowest Rating") {
            sortedReviews = [...filteredReviews].sort((a, b) => a.rating - b.rating);
        }

        setFilteredReviews(sortedReviews);
    };

    return (
        <Dialog open={open} onClose={onClose} fullWidth maxWidth="md">
            {header && <Box sx={{ padding: 2, borderBottom: "1px solid #ccc" }}>{header}</Box>}
            <DialogContent>{children}</DialogContent>
            <Box sx={{ padding: 2 }}>
                <Box display="flex" alignItems="center" justifyContent="space-between" mb={3}>
                    <Typography variant="h5" fontWeight="bold" padding={1}>
                        Guest Reviews
                    </Typography>
                    <WriteReviewModal>
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

                <Box display="flex" alignItems="center" gap={2} mb={2}>
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
                        sx={{ flexGrow: 1 }}
                    />
                    {/* Sort Dropdown */}
                    <TextField
                        select
                        size="small"
                        value={sortOption}
                        onChange={handleSortChange}
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
                                SelectProps={{
                                    native: true,
                                }}
                                sx={{
                                    minWidth: 150,
                                    backgroundColor: "#f9f9f9",
                                    borderRadius: 20,
                                }}
                            >
                                <option value="All">All</option>
                                <option value="Custom">Custom</option>
                            </TextField>
                        ))}
                    </Box>
                </Box>
            </Box>

            {/* Reviews List */}
            <Box mt={3}>
                {filteredReviews.map((review, index) => (
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
            <DialogActions>
                <Button
                    variant="outlined"
                    color="secondary"
                    onClick={onClose}
                    sx={{
                        textTransform: "none",
                        borderRadius: 20,
                    }}
                >
                    {closeButtonText || "Close"}
                </Button>
            </DialogActions>
        </Dialog>
    );
};

export default RatingReviewModal;
