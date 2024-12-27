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
} from "@mui/material";
import { Filter, MessageCircle } from "lucide-react";
import Sidebar from "../../Components/Sidebar";
import Header from "../../Components/Header";
import CustomPagination from "../../Components/Custom-Pagination";
import axios from "axios";
import ChatPanel from "../../Components/Message"; // Import ChatPanel

const CustomerReviewManagement = () => {
  const [selectedReview, setSelectedReview] = useState(null);
  const [reviews, setReviews] = useState([]);
  const [replyThread, setReplyThread] = useState([]);
  const [replyContent, setReplyContent] = useState("");
  const [isLoading, setIsLoading] = useState(false);
  const [snackbar, setSnackbar] = useState({
    open: false,
    message: "",
    severity: "success",
  });

  // Function to fetch reviews
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

  // Function to fetch reply thread
  const fetchReplyThread = async (reviewId) => {
    try {
      const response = await axios.post("", {
        review_reply_thread_rq: {
          header: {
            user_name: localStorage.getItem("user_name"),
          },
          review_id: reviewId,
        },
      });
      setReplyThread(response.data.review_reply_rs.review_reply_info);
    } catch (err) {
      handleSnackbar("Failed to fetch reply thread", "error");
    }
  };

  // Function to handle chat open
  const handleChatOpen = async (review) => {
    setSelectedReview(review);
    await fetchReplyThread(review.id);
  };

  // Function to handle reply submission
  const handleSubmitReply = async () => {
    if (!replyContent.trim()) return;

    try {
      const response = await axios.post(
        "http://localhost:8080/api/review-reply",
        {
          review_reply_thread_rq: {
            header: {
              user_name: localStorage.getItem("user_name"),
            },
            review_id: selectedReview.id,
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

  // Function to handle snackbar
  const handleSnackbar = (message, severity) => {
    setSnackbar({
      open: true,
      message,
      severity,
    });
  };

  useEffect(() => {
    fetchReviews();
  }, []);

  return (
    <Box display="flex" width="100vw" height="100vh" bgcolor="grey.100">
      <Box height="100vh" position="fixed">
        <Sidebar />
      </Box>

      <Box ml="20%" width="85%" display="flex" flexDirection="column">
        <Box
          position="fixed"
          width="80%"
          zIndex={1000}
          bgcolor="white"
          boxShadow={1}
        >
          <Header />
        </Box>

        <Box mt={8} display="flex" flex={1}>
          <Box
            flex={selectedReview ? 0.7 : 1}
            transition="all 0.3s ease"
            p={2}
            bgcolor="white"
            boxShadow={1}
          >
            <Typography variant="h6" mb={2} padding={2}>
              Customer Review Management
            </Typography>
            <Box display="flex" justifyContent="space-between" mb={2}>
              <Typography variant="body2" color="textSecondary">
                Total: {reviews.length} Reviews
              </Typography>
              <Box display="flex" gap={2}>
                <Select
                  defaultValue="Date Created"
                  variant="outlined"
                  size="small"
                >
                  <MenuItem value="Date Created">
                    Sort by: Date Created
                  </MenuItem>
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
                  {reviews.map((review) => (
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
            <CustomPagination />
          </Box>

          {/* Chat Panel */}
          {selectedReview && (
            <ChatPanel
              selectedReview={selectedReview}
              replyThread={replyThread}
              replyContent={replyContent}
              setReplyContent={setReplyContent}
              handleSubmitReply={handleSubmitReply}
            />
          )}
        </Box>
      </Box>

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
