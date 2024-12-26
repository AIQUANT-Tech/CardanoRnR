import React from "react";
import {
  Dialog,
  DialogTitle,
  DialogContent,
  DialogActions,
  Button,
  Typography,
} from "@mui/material";

const RatingReviewModal = ({ open, onClose, title, children, closeButtonText }) => {
  return (
    <Dialog open={open} onClose={onClose} fullWidth maxWidth="md">
      {title && (
        <DialogTitle>
          <Typography variant="h6" fontWeight="bold">
            {title}
          </Typography>
        </DialogTitle>
      )}
      <DialogContent>{children}</DialogContent>
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
