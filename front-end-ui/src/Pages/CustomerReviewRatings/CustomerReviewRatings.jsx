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
  Select,
  MenuItem,
  Paper,
  Snackbar,
  Alert,
  useMediaQuery,
  useTheme,
} from "@mui/material";
import { Filter } from "lucide-react";
import Sidebar from "../../Components/Sidebar";
import Header from "../../Components/Header";
import Pagination from "../../Components/Custom-Pagination"; // Import Pagination
import axios from "axios";

const CustomerReviewRatings = () => {
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
    <Box display="flex" width="90vw" height="100vh">
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
            boxShadow={1}
            sx={{
              height: "calc(100vh - 64px)", // Adjust height to take the remaining space
              overflow: "auto",
            }}
          >

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
                  </TableRow>
                </TableHead>
                <TableBody>
                  {currentReviews.map((review) => (
                    <TableRow key={review.id} hover>
                      <TableCell>{review.user_display_name}</TableCell>
                      <TableCell>{review.rating}</TableCell>
                      <TableCell>{review.review}</TableCell>
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

export default CustomerReviewRatings;
