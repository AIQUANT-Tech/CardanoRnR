import React, { useState, useEffect } from "react";
import {
  Box,
  Typography,
  Grid,
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
  InputAdornment,
  TextField,
} from "@mui/material";
import { Filter } from "lucide-react";
import Sidebar from "../../Components/Sidebar";
import Header from "../../Components/Header";
import Pagination from "../../Components/Custom-Pagination";
import axios from "axios";
import "./CustomerReviewRatings.css";

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
  const [currentPage, setCurrentPage] = useState(1);
  const [sortBy, setSortBy] = useState("Date Created");
  const [filterStatus, setFilterStatus] = useState("All");
  const [searchQuery, setSearchQuery] = useState("");
  const itemsPerPage = 4;
  const debounceTimeout = 200;

  const [debouncedSearchQuery, setDebouncedSearchQuery] = useState(searchQuery);
  useEffect(() => {
    const timer = setTimeout(() => {
      setDebouncedSearchQuery(searchQuery);
    }, debounceTimeout);

    return () => {
      clearTimeout(timer);
    };
  }, [searchQuery]);

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

      if (debouncedSearchQuery) {
        reviews = reviews.filter((review) =>
          review.user_display_name
            .toLowerCase()
            .includes(debouncedSearchQuery.toLowerCase())
        );
      }

      if (sortBy === "Date Created") {
        reviews = reviews.sort(
          (a, b) => new Date(b.created_at) - new Date(a.created_at)
        );
      } else if (sortBy === "Rating Descending") {
        reviews = reviews.sort((a, b) => b.rating - a.rating);
      } else if (sortBy === "Rating Ascending") {
        reviews = reviews.sort((a, b) => a.rating - b.rating);
      }

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
  }, [sortBy, filterStatus, debouncedSearchQuery]);

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
      <Box
        component="aside"
        width={isMobile ? "100%" : "240px"} // Fixed sidebar width
        height="100vh"
        position="fixed"
        bgcolor="lightgray"
        overflow="auto"
        zIndex={1200}
      >
        <Sidebar />
      </Box>

      <Box
        component="main"
        ml={isMobile ? 0 : "240px"}
        width={isMobile ? "100%" : "calc(100% - 240px)"}
        height="100vh"
        overflow="hidden"
        display="flex"
        flexDirection="column"
      >
        <Box
          component="header"
          height="64px" // Fixed header height
          width="100%"
          position="fixed"
          bgcolor="white"
          zIndex={1100}
          display="flex"
          alignItems="center"
          boxShadow={1}
        >
          <Header />
        </Box>

        <Box
          mt="64px" // Account for header height
          flex={1}
          overflow="auto"
          px={2}
          py={2}
        >
          <Box
            display="flex"
            justifyContent="space-between"
            alignItems="center"
            mb={2}
            flexWrap="wrap"
          >
            <TextField
              fullWidth
              variant="outlined"
              placeholder="Search by Customer Name"
              value={searchQuery}
              onChange={(e) => setSearchQuery(e.target.value)}
              size="small"
              InputProps={{
                startAdornment: (
                  <InputAdornment position="start">
                    <Filter />
                  </InputAdornment>
                ),
              }}
              sx={{ maxWidth: "300px", mb: 2 }}
            />
            <Box display="flex" gap={2}>
              <Select
                value={sortBy}
                onChange={(e) => setSortBy(e.target.value)}
                variant="outlined"
                size="small"
              >
                <MenuItem value="Date Created">Date Created</MenuItem>
                <MenuItem value="Rating Descending">Rating High to Low</MenuItem>
                <MenuItem value="Rating Ascending">Rating Low to High</MenuItem>
              </Select>
              <Select
                value={filterStatus}
                onChange={(e) => setFilterStatus(e.target.value)}
                variant="outlined"
                size="small"
              >
                <MenuItem value="All">All</MenuItem>
                <MenuItem value="Sent">Sent</MenuItem>
                <MenuItem value="Un Sent">Un Sent</MenuItem>
              </Select>
            </Box>
          </Box>

          <TableContainer component={Paper}>
            <Table>
              <TableHead>
                <TableRow>
                  <TableCell>Customer Name</TableCell>
                  <TableCell align="center">Rating</TableCell>
                  <TableCell align="center">Review</TableCell>
                  <TableCell align="center">Response Status</TableCell>
                </TableRow>
              </TableHead>
              <TableBody>
                {currentReviews.map((review) => (
                  <TableRow key={review.id}>
                    <TableCell>{review.user_display_name}</TableCell>
                    <TableCell align="center">{review.rating}</TableCell>
                    <TableCell>{review.review}</TableCell>
                    <TableCell align="center">
                      {review.review_responded ? "Sent" : "Un Sent"}
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
      </Box>
    </Box>
  );
};

export default CustomerReviewManagement;
