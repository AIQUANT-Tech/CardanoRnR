import React from "react";
import {
  Box,
  Typography,
  Divider,
  TextField,
  IconButton,
  Avatar,
} from "@mui/material";
import { Paperclip, Send } from "lucide-react";

const ChatPanel = ({
  selectedReview,
  replyThread,
  replyContent,
  setReplyContent,
  handleSubmitReply,
}) => {
  return (
    <Box
      flex={0.3}
      bgcolor="white"
      boxShadow={1}
      display="flex"
      flexDirection="column"
      height="calc(100vh - 64px)"
    >
      <Box p={2} borderBottom="1px solid #ddd">
        <Typography variant="subtitle1" fontWeight="bold" padding={2}>
          Response
        </Typography>
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
