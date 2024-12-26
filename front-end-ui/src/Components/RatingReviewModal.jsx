import React from "react";
import {
  Dialog,
  DialogContent,
  DialogActions,
  Button,
  Box,
} from "@mui/material";

const RatingReviewModal = ({
  open,
  onClose,
  header,
  children,
  closeButtonText,
}) => {
  return (
    <Dialog open={open} onClose={onClose} fullWidth maxWidth="md">
      {header && <Box sx={{ padding: 2, borderBottom: "1px solid #ccc" }}>{header}</Box>}
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
