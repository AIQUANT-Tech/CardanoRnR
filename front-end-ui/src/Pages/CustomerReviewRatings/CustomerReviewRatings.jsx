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
import Sidebar from "../../Components/Sidebar";
import Header from "../../Components/Header";
import Pagination from "../../Components/Custom-Pagination";
import axios from "axios";
import "../../Components/styles.css";
import API_BASE_URL from "../../config.js";
import {
  Tooltip,
  IconButton,
  Dialog,
  DialogTitle,
  DialogContent,
} from "@mui/material";
import ExpandMoreIcon from "@mui/icons-material/ExpandMore";


const CustomerReviewManagement = () => {
  const [reviews, setReviews] = useState([]);
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

  const [openReviewDialog, setOpenReviewDialog] = useState(false);
  const [selectedReviewText, setSelectedReviewText] = useState("");

  // Debounced search
  const [debouncedSearchQuery, setDebouncedSearchQuery] = useState(searchQuery);
  useEffect(() => {
    const timer = setTimeout(() => {
      setDebouncedSearchQuery(searchQuery);
    }, debounceTimeout);
    return () => clearTimeout(timer);
  }, [searchQuery]);

  // Fetch reviews from API
  const fetchReviews = async () => {
    try {
      setIsLoading(true);
      const response = await axios.post(
        `${API_BASE_URL}api/review/reviews/business/FetchReviews`,
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

      let fetchedReviews =
        response.data.review_rating_info_rs.review_rating_info_by_user;

      // Filter reviews based on debounced search query
      if (debouncedSearchQuery) {
        fetchedReviews = fetchedReviews.filter((review) =>
          review.user_display_name
            .toLowerCase()
            .includes(debouncedSearchQuery.toLowerCase())
        );
      }

      // Sort reviews
      if (sortBy === "Date Created") {
        fetchedReviews = fetchedReviews.sort(
          (a, b) => new Date(b.created_at) - new Date(a.created_at)
        );
      } else if (sortBy === "Rating Descending") {
        fetchedReviews = fetchedReviews.sort((a, b) => b.rating - a.rating);
      } else if (sortBy === "Rating Ascending") {
        fetchedReviews = fetchedReviews.sort((a, b) => a.rating - b.rating);
      }

      // Filter reviews by response status
      if (filterStatus === "Sent") {
        fetchedReviews = fetchedReviews.filter(
          (review) => review.review_responded === true
        );
      } else if (filterStatus === "Un Sent") {
        fetchedReviews = fetchedReviews.filter(
          (review) => review.review_responded === false
        );
      }

      setReviews(fetchedReviews);
    } catch (err) {
      setSnackbar({
        open: true,
        message: "Failed to fetch reviews",
        severity: "error",
      });
    } finally {
      setIsLoading(false);
    }
  };

  // Whenever sortBy, filterStatus, or debouncedSearchQuery changes, reset current page and fetch reviews.
  useEffect(() => {
    setCurrentPage(1);
    fetchReviews();
  }, [sortBy, filterStatus, debouncedSearchQuery]);

  const handlePageChange = (pageNum) => {
    setCurrentPage(pageNum);
  };

  const startIndex = (currentPage - 1) * itemsPerPage;
  const currentReviews = reviews.slice(startIndex, startIndex + itemsPerPage);

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
          width={isMobile ? "100%" : "calc(100% - 20%)"}
          ml={isMobile ? 0 : "20%"}
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
            flex={1}
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
            <Box mb={6} sx={{ paddingTop: "16px", width: "40%" }}>
              <Autocomplete
                freeSolo
                options={reviews.map((review) => review.user_display_name)}
                value={searchQuery}
                onInputChange={(event, newInputValue) => {
                  setSearchQuery(newInputValue);
                  setCurrentPage(1); // Reset pagination to first page when search changes
                }}
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
              flexDirection="row"
              alignItems="flex-end"
              justifyContent="space-between"
              mb={2}
              sx={{ paddingTop: "5px", marginBottom: "5px" }}
            >
              <Typography variant="body2" color="textSecondary">
                Total: {reviews.length} Reviews
              </Typography>
              <Box display="flex" gap={4}>
                {/* Sort By Select */}
                <Select
                  value={sortBy}
                  onChange={(e) => {
                    setSortBy(e.target.value);
                    setCurrentPage(1); // Reset pagination to first page when sort changes
                  }}
                  variant="outlined"
                  size="small"
                  sx={{
                    borderColor: "#DA9C9C",
                    display: "flex",
                    alignItems: "center",
                    "& .MuiOutlinedInput-notchedOutline": {
                      borderColor: "#DA9C9C",
                    },
                  }}
                  MenuProps={{
                    PaperProps: {
                      sx: {
                        "& .MuiMenuItem-root": {
                          "&:hover": {
                            backgroundColor: "#ECC0C0",
                            color: "#fff",
                          },
                          "&.Mui-selected": {
                            backgroundColor: "#DA9C9C",
                            color: "#fff",
                          },
                          "&.Mui-selected:hover": {
                            backgroundColor: "#DA9C9C",
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

                {/* Filter Status Select */}
                <Select
                  value={filterStatus}
                  onChange={(e) => {
                    setFilterStatus(e.target.value);
                    setCurrentPage(1); // Reset pagination to first page when filter changes
                  }}
                  variant="outlined"
                  size="small"
                  sx={{
                    borderColor: "#DA9C9C",
                    display: "flex",
                    alignItems: "center",
                    "& .MuiOutlinedInput-notchedOutline": {
                      borderColor: "#DA9C9C",
                    },
                  }}
                  MenuProps={{
                    PaperProps: {
                      sx: {
                        "& .MuiMenuItem-root": {
                          "&:hover": {
                            backgroundColor: "#ECC0C0",
                            color: "#fff",
                          },
                          "&.Mui-selected": {
                            backgroundColor: "#DA9C9C",
                            color: "#fff",
                          },
                          "&.Mui-selected:hover": {
                            backgroundColor: "#DA9C9C",
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
                margin: 0,
                paddingRight: "2px",
                width: "100%",
                height: "auto",
              }}
            >
              <Table
                sx={{ tableLayout: "fixed", width: "100%", height: "auto" }}
              >
                <TableHead>
                  <TableRow>
                    <TableCell
                      align="center"
                      sx={{
                        width: "142px",
                        padding: "8px",
                        whiteSpace: "nowrap",
                      }}
                    >
                      Customer Name
                    </TableCell>

                    <TableCell
                      align="center"
                      sx={{
                        width: "100px",
                        padding: "8px",
                        whiteSpace: "nowrap",
                      }}
                    >
                      Rating
                    </TableCell>

                    <TableCell
                      align="center"
                      sx={{
                        width: "auto",
                        padding: "8px",
                        textAlign: "center",
                      }}
                    >
                      Review
                    </TableCell>

                    <TableCell
                      align="center"
                      sx={{
                        width: "auto",
                        padding: "8px",
                        textAlign: "center",
                      }}
                    >
                      Room Type
                    </TableCell>

                    <TableCell
                      align="center"
                      sx={{
                        width: "auto",
                        padding: "8px",
                        textAlign: "center",
                      }}
                    >
                      Category Name
                    </TableCell>

                    <TableCell
                      align="center"
                      sx={{
                        width: "auto",
                        padding: "8px",
                        textAlign: "center",
                      }}
                    >
                      Time of Stay
                    </TableCell>

                    <TableCell
                      align="center"
                      sx={{ width: "120px", padding: "8px" }}
                    >
                      Response Status
                    </TableCell>

                    {/* ---------------- NEW COLUMN ---------------- */}
                    <TableCell
                      align="center"
                      sx={{
                        width: "180px",
                        padding: "8px",
                        whiteSpace: "nowrap",
                      }}
                    >
                      Blockchain Tx
                    </TableCell>
                  </TableRow>
                </TableHead>

                <TableBody>
                  {currentReviews.map((review) => (
                    <TableRow key={review.id}>
                      <TableCell sx={{ padding: "8px", textAlign: "center" }}>
                        {review.user_display_name}
                      </TableCell>

                      <TableCell align="center" sx={{ padding: "8px" }}>
                        {review.rating}
                      </TableCell>

                      <TableCell
                        sx={{
                          padding: "8px",
                          textAlign: "left",
                          height: "75px",
                          position: "relative",
                          cursor: "pointer",
                          "&:hover": {
                            backgroundColor: "#FFF3F3",
                          },
                        }}
                      >
                        <Box
                          sx={{
                            display: "flex",
                            alignItems: "center",
                            gap: 2,
                            overflow: "hidden",
                          }}
                        >
                          <Tooltip
                            title={review.review}
                            placement="top-start"
                            arrow
                          >
                            <Typography
                              variant="body2"
                              sx={{
                                overflow: "hidden",
                                textOverflow: "ellipsis",
                                whiteSpace: "nowrap",
                                flexGrow: 1,
                              }}
                            >
                              {review.review}
                            </Typography>
                          </Tooltip>

                          <IconButton
                            size="small"
                            sx={{
                              flexShrink: 0,
                              bgcolor: "#F4D4D4",
                              "&:hover": {
                                bgcolor: "#E6B6B6",
                              },
                            }}
                            onClick={() => {
                              setSelectedReviewText(review.review);
                              setOpenReviewDialog(true);
                            }}
                          >
                            <ExpandMoreIcon fontSize="small" />
                          </IconButton>
                        </Box>
                      </TableCell>

                      <TableCell align="center" sx={{ padding: "8px" }}>
                        {review.booking_details?.room_type}
                      </TableCell>

                      <TableCell align="center" sx={{ padding: "8px" }}>
                        {review.category_name}
                      </TableCell>

                      <TableCell align="center" sx={{ padding: "8px" }}>
                        {review.time_of_stay}
                      </TableCell>

                      <TableCell align="center" sx={{ padding: "8px" }}>
                        <Box
                          display="inline-block"
                          px={2}
                          py={0.5}
                          sx={{
                            color: review.review_responded
                              ? "success.main"
                              : "error.main",
                            borderRadius: "8px",
                            backgroundColor: review.review_responded
                              ? "#e8f5e9"
                              : "#ffebee",
                          }}
                        >
                          {review.review_responded ? "Sent" : "Un Sent"}
                        </Box>
                      </TableCell>

                      {/* ---------------- NEW ROW CELL ---------------- */}
                      <TableCell align="center" sx={{ padding: "8px" }}>
                        <Tooltip
                          title={review.blockchain_tx || "No transaction"}
                        >
                          {review.blockchain_tx ? (
                            <a
                              href={`https://preprod.cardanoscan.io/transaction/${review.blockchain_tx}`}
                              target="_blank"
                              rel="noopener noreferrer"
                              style={{
                                textDecoration: "none",
                                color: "#1976d2",
                                overflow: "hidden",
                                textOverflow: "ellipsis",
                                whiteSpace: "nowrap",
                                display: "inline-block",
                                maxWidth: "160px",
                              }}
                            >
                              {review.blockchain_tx.slice(0, 10) + "..."}
                            </a>
                          ) : (
                            <Typography color="text.secondary">—</Typography>
                          )}
                        </Tooltip>
                      </TableCell>
                    </TableRow>
                  ))}
                </TableBody>
              </Table>
            </TableContainer>

            <Box
              sx={{
                display: "flex",
                justifyContent: "flex-end",
                marginRight: "5px",
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
      <Dialog
        open={openReviewDialog}
        onClose={() => setOpenReviewDialog(false)}
        fullWidth
        maxWidth="sm"
      >
        <DialogTitle>Full Review</DialogTitle>
        <DialogContent>
          <Typography variant="body1" sx={{ whiteSpace: "pre-wrap" }}>
            {selectedReviewText}
          </Typography>
        </DialogContent>
      </Dialog>
    </Box>
  );
};

export default CustomerReviewManagement;
