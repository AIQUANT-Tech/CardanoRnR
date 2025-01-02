import React, { useState, useEffect } from "react";
import {
  Box,
  Typography,
  Table,
  TableBody,
  TableCell,
  TableContainer,
  TableHead,
  TableRow,
  Button,
  IconButton,
  Select,
  MenuItem,
  Paper,
  Snackbar,
  Alert,
  useMediaQuery,
  useTheme,
  TextField,
  InputAdornment
} from "@mui/material";
import { MessageCircle } from "lucide-react";
import Sidebar from "../../Components/Sidebar";
import Header from "../../Components/Header";
import Pagination from "../../Components/Custom-Pagination";
import axios from "axios";
import ChatPanel from "../../Components/Message";

const CustomerReviewManagement = () => {
  const [selectedReview, setSelectedReview] = useState(null);
  const [reviews, setReviews] = useState([]);
  const [replyThread, setReplyThread] = useState([]);
  const [replyContent, setReplyContent] = useState("");
  const [isLoading, setIsLoading] = useState(false);
  const [isThreadLoading, setIsThreadLoading] = useState(false);
  const [snackbar, setSnackbar] = useState({
    open: false,
    message: "",
    severity: "success",
  });
  const [currentPage, setCurrentPage] = useState(1);
  const [sortBy, setSortBy] = useState("Date Created");
  const [filterStatus, setFilterStatus] = useState("All");
  const [searchQuery, setSearchQuery] = useState(""); // State for search query
  const itemsPerPage = 4;
  const debounceTimeout = 200; // Time delay for debounce

  // Debounced search
  const [debouncedSearchQuery, setDebouncedSearchQuery] = useState(searchQuery);
  useEffect(() => {
    const timer = setTimeout(() => {
      setDebouncedSearchQuery(searchQuery);
    }, debounceTimeout);

    return () => {
      clearTimeout(timer); // Clean up on each change
    };
  }, [searchQuery]);

  // Fetch reviews
  const fetchReviews = async () => {
    try {
      setIsLoading(true);
      const response = await axios.post(
        "http://localhost:8080/api/review/reviews/business/FetchReviews",
        {
          review_rating_info_rq: {
            header: {
              user_name: "businessUser",
              product: "rnr",
              request_type: "REVIEW_RATING_INFO",
            },
          },
        }
      );

      let reviews =
        response.data.review_rating_info_rs.review_rating_info_by_user;

      // Filter reviews based on search query
      if (debouncedSearchQuery) {
        reviews = reviews.filter((review) =>
          review.user_display_name
            .toLowerCase()
            .includes(debouncedSearchQuery.toLowerCase())
        );
      }

      // Sort reviews
      if (sortBy === "Date Created") {
        reviews = reviews.sort(
          (a, b) => new Date(b.created_at) - new Date(a.created_at)
        );
      } else if (sortBy === "Rating Descending") {
        reviews = reviews.sort((a, b) => b.rating - a.rating);
      } else if (sortBy === "Rating Ascending") {
        reviews = reviews.sort((a, b) => a.rating - b.rating);
      }

      // Filter reviews by response status
      if (filterStatus === "Sent") {
        reviews = reviews.filter((review) => review.review_responded === true);
      } else if (filterStatus === "Un Sent") {
        reviews = reviews.filter((review) => review.review_responded === false);
      }

      setReviews(reviews);
    } catch (err) {
      handleSnackbar("Failed to fetch reviews", "error");
    } finally {
      setIsLoading(false);
    }
  };

  useEffect(() => {
    fetchReviews();
  }, [sortBy, filterStatus, debouncedSearchQuery]); // Depend on the debounced search query

  // Fetch reply thread
  const fetchReplyThread = async (reviewId) => {
    try {
      setIsThreadLoading(true);
      const response = await axios.post(
        "http://localhost:8080/api/reply/ReplyToReview",
        {
          review_reply_thread_rq: {
            header: {
              user_name: selectedReview.user_id,
              product: "rnr",
              request_type: "REVIEW_RATING_INFO",
            },
            review_id: selectedReview.review_id,
          },
        }
      );
      setReplyThread(response.data.review_reply_rs.review_reply_info);
    } catch (err) {
      handleSnackbar("Failed to fetch reply thread", "error");
    } finally {
      setIsThreadLoading(false);
    }
  };

  // Handle chat open
  const handleChatOpen = async (review) => {
    setSelectedReview(review);
    setReplyThread([]); // Clear previous replies

    // Fetch the reply thread after selecting the review to ensure the data is refreshed
    setIsThreadLoading(true); // Set the loading state to true to show a loading indicator
    try {
      const response = await axios.post(
        "http://localhost:8080/api/reply/ReplyToReview",
        {
          review_reply_thread_rq: {
            header: {
              user_name: review.user_id, // Make sure to use the correct user_id
              product: "rnr",
              request_type: "REVIEW_RATING_INFO",
            },
            review_id: review.review_id, // Use review_id to fetch the correct thread
          },
        }
      );
      
      setReplyThread(response.data.review_reply_rs.review_reply_info); // Set the new thread data
    } catch (err) {
      handleSnackbar("Failed to fetch reply thread", "error");
    } finally {
      setIsThreadLoading(false); // Set loading to false when the data fetch is complete
    }
  };

  // Handle reply submission
  const handleSubmitReply = async () => {
    if (!replyContent.trim()) return; // Don't submit if the reply is empty
    try {
      const user = JSON.parse(localStorage.getItem("user"));

      const response = await axios.post(
        "http://localhost:8080/api/reply/ReplyToReviews",
        {
          review_reply_thread_rq: {
            header: {
              user_name: user.user_id,
              product: "rnr",
              request_type: "REVIEW_RATING_INFO",
            },
            review_id: selectedReview.review_id,
            content: replyContent,
          },
        }
      );

      if (response.data.review_reply_rq.status === "success") {
        setReplyContent(""); // Clear the reply content after sending
        await fetchReplyThread(selectedReview.id); // Refresh the reply thread
        handleSnackbar("Reply sent successfully", "success");
      }
    } catch (err) {
      handleSnackbar("Failed to send reply", "error");
    }
  };

  const handlePageChange = (pageNum) => {
    setCurrentPage(pageNum);
  };

  const startIndex = (currentPage - 1) * itemsPerPage;
  const currentReviews = reviews.slice(startIndex, startIndex + itemsPerPage);

  const handleSnackbar = (message, severity) => {
    setSnackbar({
      open: true,
      message,
      severity,
    });
  };

  const theme = useTheme();
  const isMobile = useMediaQuery(theme.breakpoints.down("sm"));

  return (
    <Box display="flex" width="100vw" height="100vh" bgcolor="white">
      {/* Sidebar */}
      <Box height="100vh" position="fixed" width={isMobile ? "100%" : "20%"}>
        <Sidebar />
      </Box>

      <Box
        ml={isMobile ? 0 : "20%"}
        width={isMobile ? "100%" : "80%"}
        display="flex"
        flexDirection="column"
        height="100vh"
        overflow="hidden"
      >
        {/* Header */}
        <Box
          position="fixed"
          width="80%"
          zIndex={1000}
          bgcolor="white"
          sx={{ height: "64px" }}
        >
          <Header />
        </Box>

        {/* Main Content Area */}
        <Box
          mt="64px"
          display="flex"
          flex={1}
          flexDirection={isMobile ? "column" : "row"}
          overflow="auto"
        >
          <Box
            flex={selectedReview ? (isMobile ? 1 : 0.7) : 1}
            transition="all 0.3s ease"
            bgcolor="white"
            boxShadow={1}
            sx={{
              height: "calc(100vh - 64px)",
              overflow: "auto",
            }}
          >
            {/* Search Field */}

            <Box mb={6} sx={{ paddingTop: "16px", width: "50%" }}>
              <TextField
                fullWidth
                variant="outlined"
                placeholder="Search by Customer Name"
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.target.value)}
                size="small"
                sx={{
                  borderRadius: "50px", // Add borderRadius here
                  "& .MuiOutlinedInput-root": {
                    borderRadius: "50px",
                    border: "1px solid #DA9C9C",
                    "&:hover": {
                      border: "1px solid #DA9C9C",
                    },
                    "&:selected": {
                      border: "1px solid #DA9C9C",
                    },
                  },
                  "& .MuiOutlinedInput-notchedOutline": {
                    borderStyle: "none",
                    borderWidth: "none",
                  },
                }}
                InputProps={{
                  startAdornment: (
                    <InputAdornment position="start">
                      <svg
                        xmlns="http://www.w3.org/2000/svg"
                        width="30"
                        height="30"
                        viewBox="0 0 24 24"
                        fill="none"
                        stroke="#DA9C9C"
                        stroke-width="2"
                        stroke-linecap="round"
                        stroke-linejoin="round"
                        class="lucide lucide-search"
                      >
                        <circle cx="11" cy="11" r="8" />
                        <path d="m21 21-4.3-4.3" />
                      </svg>
                    </InputAdornment>
                  ),
                }}
              />
            </Box>

            <Box display="flex" justifyContent="space-between" mb={2}>
              <Typography variant="body2" color="textSecondary">
                Total: {reviews.length} Reviews
              </Typography>
              <Box display="flex" gap={2}>
                <Select
                  value={sortBy}
                  onChange={(e) => setSortBy(e.target.value)}
                  variant="outlined"
                  size="small"
                >
                  <MenuItem value="Date Created">
                    Sort by: Date Created
                  </MenuItem>
                  <MenuItem value="Rating Descending">
                    Sort by: Rating High to Low
                  </MenuItem>
                  <MenuItem value="Rating Ascending">
                    Sort by: Rating Low to High
                  </MenuItem>
                </Select>
                <Select
                  value={filterStatus}
                  onChange={(e) => setFilterStatus(e.target.value)}
                  variant="outlined"
                  size="small"
                >
                  <MenuItem value="All">Filter by: All</MenuItem>
                  <MenuItem value="Sent">Filter by: Sent</MenuItem>
                  <MenuItem value="Un Sent">Filter by: Un Sent</MenuItem>
                </Select>
              </Box>
            </Box>

            <TableContainer component={Paper}>
              <Table>
                <TableHead>
                  <TableRow>
                    <TableCell width={"142"} align="center">
                      Customer Name
                    </TableCell>
                    <TableCell align="center">Rating</TableCell>
                    <TableCell align="center">Review</TableCell>
                    <TableCell width={"120"} align="center">
                      Response Status
                    </TableCell>
                    <TableCell />
                  </TableRow>
                </TableHead>
                <TableBody>
                  {currentReviews.map((review) => (
                    <TableRow key={review.id}>
                      <TableCell>{review.user_display_name}</TableCell>
                      <TableCell align="center">{review.rating}</TableCell>
                      <TableCell>{review.review}</TableCell>
                      <TableCell align="center">
                        <Box
                          display="inline-block"
                          px={2}
                          py={0.5}
                          color={review.review_responded ? "success.main" : "error.main"}
                          width={"150"}
                        >
                          {review.review_responded ? "Sent" : "Un Sent"}
                        </Box>
                      </TableCell>
                      <TableCell>
                        <IconButton onClick={() => handleChatOpen(review)}>
                          <MessageCircle size={20} />
                        </IconButton>
                      </TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </TableContainer>
            <Pagination
              currentPage={currentPage}
              totalItems={reviews.length}
              itemsPerPage={itemsPerPage}
              onPageChange={handlePageChange}
            />
          </Box>
          {selectedReview && (
            <Box
              flex={0.3}
              sx={{
                width: isMobile ? "100%" : "30%",
                transition: "all 0.3s ease",
                height: "calc(100vh - 64px)",
                overflowY: "auto",
                boxShadow: 1,
              }}
            >
              {isThreadLoading ? (
                <Typography variant="body2" align="center" mt={2}>
                  Loading replies...
                </Typography>
              ) : (
                <ChatPanel
                  selectedReview={selectedReview}
                  replyThread={replyThread}
                  replyContent={replyContent}
                  setReplyContent={setReplyContent}
                  handleSubmitReply={handleSubmitReply}
                />
              )}
            </Box>
          )}
        </Box>
      </Box>

      {/* Snackbar for notifications */}
      <Snackbar
        open={snackbar.open}
        autoHideDuration={6000}
        onClose={() => setSnackbar({ ...snackbar, open: false })}
      >
        <Alert
          severity={snackbar.severity}
          onClose={() => setSnackbar({ ...snackbar, open: false })}
        >
          {snackbar.message}
        </Alert>
      </Snackbar>
    </Box>
  );
};

export default CustomerReviewManagement;
