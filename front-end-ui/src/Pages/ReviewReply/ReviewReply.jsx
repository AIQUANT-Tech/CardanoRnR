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
} from "@mui/material";
import { Filter, MessageCircle } from "lucide-react";
import Sidebar from "../../Components/Sidebar";
import Header from "../../Components/Header";
import Pagination from "../../Components/Custom-Pagination"; // Import Pagination
import axios from "axios";
import ChatPanel from "../../Components/Message"; // Import ChatPanel

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
  const itemsPerPage = 4;  // Set items per page to 4

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
      setReviews(response.data.review_rating_info_rs.review_rating_info_by_user);
    } catch (err) {
      handleSnackbar("Failed to fetch reviews", "error");
    } finally {
      setIsLoading(false);
    }
  };

  // Fetch reply thread
  const fetchReplyThread = async (reviewId) => {
    try {
      setIsThreadLoading(true);
      const response = await axios.post("http://localhost:8080/api/reply/ReplyToReview", {
        review_reply_thread_rq: {
          header: {
            user_name: selectedReview.user_id,
            product: "rnr",
            request_type: "REVIEW_RATING_INFO",
          },
          review_id: selectedReview.review_id,
        },
      });
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
    setReplyThread([]);
    await fetchReplyThread(review.id);
  };

  // Handle reply submission
  const handleSubmitReply = async () => {
    if (!replyContent.trim()) return;
    try {
      const user = JSON.parse(localStorage.getItem('user'));

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
        setReplyContent("");
        await fetchReplyThread(selectedReview.id);
        handleSnackbar("Reply sent successfully", "success");
      }
    } catch (err) {
      handleSnackbar("Failed to send reply", "error");
    }
  };

  // Handle snackbar
  const handleSnackbar = (message, severity) => {
    setSnackbar({
      open: true,
      message,
      severity,
    });
  };

  const handlePageChange = (pageNum) => {
    setCurrentPage(pageNum);
  };

  const startIndex = (currentPage - 1) * itemsPerPage;
  const currentReviews = reviews.slice(startIndex, startIndex + itemsPerPage);

  useEffect(() => {
    fetchReviews();
  });

  const theme = useTheme();
  const isMobile = useMediaQuery(theme.breakpoints.down("sm")); 

  return (
    <Box display="flex" width="100vw" height="100vh" bgcolor="grey.100">
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
          sx={{ height: "64px" }} // Fixed header height
        >
          <Header />
        </Box>

        {/* Main Content Area */}
        <Box
          mt="64px"  // Adjust for header height
          display="flex"
          flex={1}
          flexDirection={isMobile ? "column" : "row"} // Stack on mobile
          overflow="auto"
        >
          <Box
            flex={selectedReview ? (isMobile ? 1 : 0.7) : 1}
            transition="all 0.3s ease"
            bgcolor="white"
            boxShadow={1}
            sx={{
              height: "calc(100vh - 64px)", // Adjust height to take the remaining space
              overflow: "auto",
            }}
          >
            <Typography variant="h6" mb={2} padding={2}>
              Customer Review Management
            </Typography>

            <Box display="flex" justifyContent="space-between" mb={2}>
              <Typography variant="body2" color="textSecondary">
                Total: {reviews.length} Reviews
              </Typography>
              <Box display="flex" gap={2}>
                <Select defaultValue="Date Created" variant="outlined" size="small">
                  <MenuItem value="Date Created">Sort by: Date Created</MenuItem>
                </Select>
                <Button variant="outlined" startIcon={<Filter size={16} />}>
                  Filter
                </Button>
              </Box>
            </Box>

            <TableContainer component={Paper}>
              <Table>
                <TableHead>
                  <TableRow>
                    <TableCell>Customer Name</TableCell>
                    <TableCell>Rating</TableCell>
                    <TableCell>Review</TableCell>
                    <TableCell>Response Status</TableCell>
                    <TableCell />
                  </TableRow>
                </TableHead>
                <TableBody>
                  {currentReviews.map((review) => (
                    <TableRow key={review.id} hover>
                      <TableCell>{review.user_display_name}</TableCell>
                      <TableCell>{review.rating}</TableCell>
                      <TableCell>{review.review}</TableCell>
                      <TableCell>
                        <Box
                          display="inline-block"
                          px={2}
                          py={0.5}
                          borderRadius={16}
                          bgcolor={
                            review.responseStatus === "Sent"
                              ? "success.light"
                              : "error.light"
                          }
                          color={
                            review.responseStatus === "Sent"
                              ? "success.main"
                              : "error.main"
                          }
                          textAlign="center"
                        >
                          {review.responseStatus}
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

          {/* Chat Panel */}
          {selectedReview && (
            <Box
              flex={selectedReview ? 0.3 : 0}
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
