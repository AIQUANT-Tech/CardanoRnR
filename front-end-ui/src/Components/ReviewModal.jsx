import React, { useState } from "react";
import {
  Box,
  Typography,
  Button,
  TextField,
  Dialog,
  DialogActions,
  DialogContent,
  DialogTitle,
  Grid,
  Avatar,
} from "@mui/material";
import StarIcon from "@mui/icons-material/Star";

const ReviewModal = ({
    open,
    setOpen
}) => {
  const [overallRating, setOverallRating] = useState(0);
  const [hover, setHover] = useState(-1);
  const [overallReview, setOverallReview] = useState("");
  const [categories, setCategories] = useState([
    { name: "Pool", rating: 0, review: "" },
    { name: "Room", rating: 0, review: "" },
    { name: "Service", rating: 0, review: "" },
  ]);

  const handleCategoryRating = (index, rating) => {
    const updatedCategories = [...categories];
    updatedCategories[index].rating = rating;
    setCategories(updatedCategories);
  };

  const handleCategoryReview = (index, review) => {
    const updatedCategories = [...categories];
    updatedCategories[index].review = review;
    setCategories(updatedCategories);
  };

  const handleSubmit = () => {
    console.log("Overall Rating:", overallRating);
    console.log("Overall Review:", overallReview);
    console.log("Category Ratings & Reviews:", categories);
  };

  const handleClose = () => {
    setOpen(false);
  }

  return (
    <div>
      {/* Modal */}
      <Dialog open={open} maxWidth="md" fullWidth>
        <DialogTitle>
          <Typography variant="h6" fontWeight="bold">
            Overall Rating & Reviews
          </Typography>
        </DialogTitle>
        <DialogContent>
          <Typography color="textSecondary" gutterBottom>
            Thank you in advance for your feedback.
          </Typography>

          {/* Overall Rating */}
          <Typography variant="body1" fontWeight="bold" mt={2}>
            Overall Rating
          </Typography>
          <Box display="flex" alignItems="center" mb={2}>
            {[1, 2, 3, 4, 5].map((star) => (
              <StarIcon
                key={star}
                onClick={() => setOverallRating(star)}
                onMouseEnter={() => setHover(star)}
                onMouseLeave={() => setHover(-1)}
                sx={{
                  cursor: "pointer",
                  color:
                    (hover || overallRating) >= star
                      ? "#fdd835" 
                      : "#e0e0e0", 
                  fontSize: 32,
                }}
              />
            ))}
            <Typography sx={{ marginLeft: 2 }}>
              {hover !== -1 ? hover : overallRating} / 5
            </Typography>
          </Box>

          {/* Overall Review */}
          <TextField
            fullWidth
            multiline
            rows={3}
            value={overallReview}
            onChange={(e) => setOverallReview(e.target.value)}
            label="Overall Review"
            placeholder="Write your overall experience..."
            InputProps={{
              style: {
                backgroundColor: "#f9f9f9",
                borderRadius: 10,
              },
            }}
          />

          {/* Category Ratings & Reviews */}
          {overallRating > 0 && overallReview.trim() && (
            <>
              <Typography
                variant="body1"
                fontWeight="bold"
                mt={3}
                gutterBottom
              >
                Categorised Rating & Reviews
              </Typography>
              {categories.map((category, index) => (
                <Box key={index} mt={2}>
                  <Typography variant="body2" fontWeight="bold">
                    {category.name}
                  </Typography>
                  <Box display="flex" alignItems="center">
                    {[1, 2, 3, 4, 5].map((star) => (
                      <StarIcon
                        key={star}
                        onClick={() =>
                          handleCategoryRating(index, star)
                        }
                        onMouseEnter={() => setHover(star)}
                        onMouseLeave={() => setHover(-1)}
                        sx={{
                          cursor: "pointer",
                          color:
                            (hover || category.rating) >= star
                              ? "#fdd835"
                              : "#e0e0e0",
                          fontSize: 28,
                        }}
                      />
                    ))}
                    <Typography sx={{ marginLeft: 2 }}>
                      {category.rating} / 5
                    </Typography>
                  </Box>
                  <TextField
                    fullWidth
                    multiline
                    rows={2}
                    value={category.review}
                    onChange={(e) =>
                      handleCategoryReview(index, e.target.value)
                    }
                    label={`Review for ${category.name}`}
                    placeholder={`Write your experience about ${category.name}...`}
                    sx={{ marginTop: 1 }}
                  />
                </Box>
              ))}
            </>
          )}
        </DialogContent>

        <DialogActions>
          <Button
            variant="contained"
            color="primary"
            onClick={handleSubmit}
            sx={{
              textTransform: "none",
              borderRadius: 20,
            }}
            disabled={!overallRating || !overallReview.trim()}
          >
            Submit
          </Button>
          <Button
            variant="outlined"
            color="secondary"
            onClick={handleClose}
            sx={{
              textTransform: "none",
              borderRadius: 20,
            }}
          >
            Cancel
          </Button>
        </DialogActions>
      </Dialog>
    </div>
  );
};

export default ReviewModal;
