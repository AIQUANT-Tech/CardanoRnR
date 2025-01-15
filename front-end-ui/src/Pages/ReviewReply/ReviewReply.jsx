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
  InputAdornment,
  Autocomplete,
} from "@mui/material";
import { MessageCircle, MessageCircleReply } from "lucide-react";
import Sidebar from "../../Components/Sidebar";
import Header from "../../Components/Header";
import Pagination from "../../Components/Custom-Pagination";
import axios from "axios";
import ChatPanel from "../../Components/Message";
import "./ReviewReply.css";
import "../../Components/styles.css";

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
  const [chatPanelOpen, setChatPanelOpen] = useState(false); // Manage chat panel visibility
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
    setChatPanelOpen(true); // Open the chat panel

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

  // Handle reply submission and update response status
  const handleSubmitReply = async () => {
    if (!replyContent.trim()) return; // Don't submit if the reply is empty
    try {
      const user = JSON.parse(localStorage.getItem("user"));

      // Send the reply
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
        // Clear the reply content after sending
        setReplyContent("");

        // Fetch the updated reply thread
        await fetchReplyThread(selectedReview.id);

        // Update the review's response status to 'Sent'
        const updatedReviews = reviews.map((review) =>
          review.id === selectedReview.id
            ? { ...review, review_responded: true }
            : review
        );
        setReviews(updatedReviews);

        // Show success message
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

  // Close chat panel
  const closeChatPanel = () => {
    setChatPanelOpen(false);
    setSelectedReview(null); // Optional: reset selected review on close
  };

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
            flex={chatPanelOpen ? (isMobile ? 1 : 0.7) : 1}
            transition="all 0.3s ease"
            bgcolor="white"
            boxShadow={1}
            sx={{
              height: "calc(100vh - 64px)",
              overflow: "auto",
              paddingRight: "2px",
            }}
          >
            {/* Search Field */}
            <Box
              mb={6}
              sx={{
                margin: 0, // Set margin to 0 to ensure no margin
                paddingTop: "16px", // Padding at the top
                width: "40%", // Width remains as defined
              }}
            >
              <Autocomplete
                freeSolo
                options={reviews.map((review) => review.user_display_name)}
                value={searchQuery}
                onInputChange={(event, newInputValue) =>
                  setSearchQuery(newInputValue)
                }
                renderInput={(params) => (
                  <TextField
                    {...params}
                    fullWidth
                    variant="outlined"
                    placeholder="Search by Customer Name"
                    size="small"
                    sx={{
                      borderRadius: "50px",
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
                      ...params.InputProps,
                      startAdornment: (
                        <InputAdornment position="start">
                          <svg
                            xmlns="http://www.w3.org/2000/svg"
                            width="30"
                            height="30"
                            viewBox="0 0 24 24"
                            fill="none"
                            stroke="#DA9C9C"
                            strokeWidth="2"
                            strokeLinecap="round"
                            strokeLinejoin="round"
                            className="lucide lucide-search"
                          >
                            <circle cx="11" cy="11" r="8" />
                            <path d="m21 21-4.3-4.3" />
                          </svg>
                        </InputAdornment>
                      ),
                    }}
                  />
                )}
              />
            </Box>

            <Box
              display="flex"
              flexDirection={"row"}
              alignItems={"flex-end"}
              justifyContent="space-between"
              mb={2}
              sx={{ paddingTop: "5px", marginBottom: "5px" }}
            >
              <Typography variant="body2" color="textSecondary">
                Total: {reviews.length} Reviews
              </Typography>
              <Box display="flex" gap={4}>
                <Select
                  value={sortBy}
                  onChange={(e) => setSortBy(e.target.value)}
                  variant="outlined"
                  size="small"
                  sx={{
                    borderColor: "#DA9C9C",
                    display: "flex",
                    alignItems: "center",
                    "& .MuiOutlinedInput-notchedOutline": {
                      borderColor: "#DA9C9C",
                    },
                    "&:hover .MuiOutlinedInput-notchedOutline": {
                      borderColor: "#DA9C9C",
                    },
                    "&.Mui-focused .MuiOutlinedInput-notchedOutline": {
                      borderColor: "#DA9C9C",
                    },
                  }}
                  MenuProps={{
                    PaperProps: {
                      sx: {
                        "& .MuiMenuItem-root": {
                          "&:hover": {
                            backgroundColor: "#ECC0C0", // Background color on hover
                            color: "#fff", // Text color on hover
                          },
                          "&.Mui-selected": {
                            backgroundColor: "#DA9C9C", // Background color when selected
                            color: "#fff", // Text color when selected
                          },
                          "&.Mui-selected:hover": {
                            backgroundColor: "#DA9C9C", // Background color on hover when selected
                          },
                        },
                      },
                    },
                  }}
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
                  sx={{
                    borderColor: "#DA9C9C",
                    display: "flex",
                    alignItems: "center",
                    "& .MuiOutlinedInput-notchedOutline": {
                      borderColor: "#DA9C9C", // Default border color
                    },
                    "&:hover .MuiOutlinedInput-notchedOutline": {
                      borderColor: "#DA9C9C", // Hover border color
                    },
                    "&.Mui-focused .MuiOutlinedInput-notchedOutline": {
                      borderColor: "#DA9C9C", // Focused border color
                    },
                  }}
                  MenuProps={{
                    PaperProps: {
                      sx: {
                        "& .MuiMenuItem-root": {
                          "&:hover": {
                            backgroundColor: "#ECC0C0", // Background color on hover
                            color: "#fff", // Text color on hover
                          },
                          "&.Mui-selected": {
                            backgroundColor: "#DA9C9C", // Background color when selected
                            color: "#fff", // Text color when selected
                          },
                          "&.Mui-selected:hover": {
                            backgroundColor: "#DA9C9C", // Background color on hover when selected
                          },
                        },
                      },
                    },
                  }}
                >
                  <MenuItem value="All">Filter by: All</MenuItem>
                  <MenuItem value="Sent">Filter by: Sent</MenuItem>
                  <MenuItem value="Un Sent">Filter by: Un Sent</MenuItem>
                </Select>
              </Box>
            </Box>

            <TableContainer
              component={Paper}
              sx={{
                margin: 0, // No margin around the TableContainer
                paddingRight: "2px",
                width: "100%", // Full width of the container
                height: "auto", // Adjust height based on content
              }}
            >
              <Table
                sx={{
                  tableLayout: "fixed", // Ensures fixed column widths
                  width: "100%", // Full width\
                  height: "auto", // Adjust height based on content
                }}
              >
                <TableHead>
                  <TableRow>
                    <TableCell
                      align="center"
                      sx={{
                        width: "142px", // Fixed width
                        padding: "8px", // Minimal padding
                        whiteSpace: "nowrap", // Prevent text wrapping
                      }}
                    >
                      Customer Name
                    </TableCell>
                    <TableCell
                      align="center"
                      sx={{
                        width: "100px", // Adjust as needed
                        padding: "8px",
                        whiteSpace: "nowrap",
                      }}
                    >
                      Rating
                    </TableCell>
                    <TableCell
                      align="center"
                      sx={{
                        width: "auto", // Fills remaining space
                        padding: "8px",
                        textAlign: "center", // Aligns left if review text is long
                      }}
                    >
                      Review
                    </TableCell>
                    <TableCell
                      align="center"
                      sx={{
                        width: "120px", // Fixed width for status
                        padding: "8px",
                      }}
                    >
                      Response Status
                    </TableCell>
                    <TableCell
                      sx={{
                        width: "50px", // Adjust width for icons
                        padding: "8px",
                        textAlign: "center", // Center-align the icon
                      }}
                    />
                  </TableRow>
                </TableHead>
                <TableBody>
                  {currentReviews.map((review) => (
                    <TableRow key={review.id}>
                      <TableCell
                        sx={{
                          padding: "8px", // Minimal padding
                          textAlign: "center", // Center-align text
                        }}
                      >
                        {review.user_display_name}
                      </TableCell>
                      <TableCell
                        align="center"
                        sx={{
                          padding: "8px",
                        }}
                      >
                        {review.rating}
                      </TableCell>
                      <TableCell
                        sx={{
                          padding: "8px", // Minimal padding for compactness
                          textAlign: "left", // Align text to the left for readability
                          height: "75px", // Fixed height for the cell
                          overflow: "hidden", // Hide text that overflows the cell
                          textOverflow: "ellipsis", // Show ellipsis for long text
                          whiteSpace: "nowrap", // Prevent text wrapping
                        }}
                      >
                        {review.review}
                      </TableCell>
                      <TableCell
                        align="center"
                        sx={{
                          padding: "8px",
                        }}
                      >
                        <Box
                          display="inline-block"
                          px={2}
                          py={0.5}
                          sx={{
                            color: review.review_responded
                              ? "success.main"
                              : "error.main", // Dynamically set text color
                            borderRadius: "8px", // Rounded look
                            backgroundColor: review.review_responded
                              ? "#e8f5e9" // Light green for "Sent"
                              : "#ffebee", // Light red for "Un Sent"
                          }}
                        >
                          {review.review_responded ? "Sent" : "Un Sent"}
                        </Box>
                      </TableCell>
                      <TableCell
                        sx={{
                          padding: "8px",
                          textAlign: "center",
                          height: "20px", // Center-align the icon
                        }}
                      >
                        <IconButton onClick={() => handleChatOpen(review)}>
                          <MessageCircleReply size={24} color="black" />
                        </IconButton>
                      </TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </TableContainer>
            <Box
              sx={{
                display: "flex", // Use flexbox
                justifyContent: "flex-end", // Align pagination to the right
                marginRight: "5px", //
              }}
            >
              <Pagination
                currentPage={currentPage}
                totalItems={reviews.length}
                itemsPerPage={itemsPerPage}
                onPageChange={handlePageChange}
              />
            </Box>
          </Box>
          {/* Chat Panel */}
          {chatPanelOpen && (
            <ChatPanel
              selectedReview={selectedReview}
              replyThread={replyThread}
              isThreadLoading={isThreadLoading}
              replyContent={replyContent}
              setReplyContent={setReplyContent}
              handleSubmitReply={handleSubmitReply}
              closeChatPanel={closeChatPanel} // Pass close function to ChatPanel
            />
          )}
        </Box>
      </Box>
      <Snackbar
        open={snackbar.open}
        autoHideDuration={3000}
        onClose={() => setSnackbar({ open: false })}
      >
        <Alert
          severity={snackbar.severity}
          sx={{ width: "100%" }}
          onClose={() => setSnackbar({ open: false })}
        >
          {snackbar.message}
        </Alert>
      </Snackbar>
    </Box>
  );
};

export default CustomerReviewManagement;
