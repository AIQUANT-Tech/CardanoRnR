import React, { useEffect, useState } from "react";
import {
  Box,
  Typography,
  Divider,
  TextField,
  IconButton,
  Avatar,
  Button,
  useMediaQuery,
  useTheme,
} from "@mui/material";
import { Paperclip, Send, X } from "lucide-react";

const ChatPanel = ({
  selectedReview,
  replyThread,
  replyContent,
  setReplyContent,
  handleSubmitReply,
  closeChatPanel, // Function to close the chat panel passed as a prop
}) => {
  const [isPanelVisible, setIsPanelVisible] = useState(true); // State to manage visibility
  const theme = useTheme();
  const isMobile = useMediaQuery(theme.breakpoints.down("sm")); // Detects mobile devices

  // Handles panel visibility on close
  const handleClosePanel = () => {
    setIsPanelVisible(false);
    closeChatPanel(); // Optional: Trigger parent component's logic
  };

  useEffect(() => {
    setReplyContent("");
  }, [selectedReview]);

  if (!isPanelVisible) return null; // Hide the panel when not visible

  return (
    <Box
      flex={isMobile ? 1 : 0.3} // Take full width on mobile
      bgcolor="white"
      boxShadow={1}
      display="flex"
      flexDirection="column"
      height="calc(100vh - 64px)"
      position="relative"
    >
      <Box
        p={2}
        borderBottom="1px solid #ddd"
        display="flex"
        justifyContent="space-between"
        alignItems="center"
      >
        <Typography variant="subtitle1" fontWeight="bold" padding={2}>
          Response
        </Typography>
        <IconButton onClick={handleClosePanel}>
          <X />
        </IconButton>
      </Box>
      <Box flex={1} p={2} overflow="auto">
        <Box display="flex" gap={2} mb={2}>
          <Avatar>{selectedReview?.avatar}</Avatar>
          <Box>
            <Typography variant="body1" fontWeight="bold">
              {selectedReview?.customerName}
            </Typography>
            <Typography variant="body2" color="textSecondary">
              {selectedReview?.date}
            </Typography>
            <Typography variant="body2" mt={1}>
              {selectedReview?.review}
            </Typography>
          </Box>
        </Box>
        {/* Reply Thread Without Pagination */}
        {replyThread.map((reply, index) => (
          <Box key={index} display="flex" gap={2} mb={2} mt={2}>
            <Avatar>{reply.replied_by[0]}</Avatar>
            <Box>
              <Typography variant="body1" fontWeight="bold">
                {reply.replied_by}
              </Typography>
              <Typography variant="body2" mt={1}>
                {reply.reply}
              </Typography>
            </Box>
          </Box>
        ))}
      </Box>
      <Divider />
      <Box p={2} display="flex" gap={1}>
        <TextField
          fullWidth
          variant="outlined"
          placeholder="Type here"
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
          value={replyContent}
          onChange={(e) => setReplyContent(e.target.value)}
          onKeyPress={(e) => {
            if (e.key === "Enter" && !e.shiftKey) {
              e.preventDefault();
              handleSubmitReply();
            }
          }}
        />
        <IconButton>
          <Paperclip />
        </IconButton>
        <IconButton
          onClick={handleSubmitReply}
          disabled={!replyContent.trim()}
        >
          <Send />
        </IconButton>
      </Box>
    </Box>
  );
};

export default ChatPanel;
