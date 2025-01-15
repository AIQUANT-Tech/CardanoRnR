// import React, { useEffect, useState } from "react";
// import {
//   Box,
//   Typography,
//   Divider,
//   TextField,
//   IconButton,
//   Avatar,
//   Button,
//   useMediaQuery,
//   useTheme,
// } from "@mui/material";
// import { Paperclip, Send, X } from "lucide-react";

// const ChatPanel = ({
//   selectedReview,
//   replyThread,
//   replyContent,
//   setReplyContent,
//   handleSubmitReply,
//   closeChatPanel, // Function to close the chat panel passed as a prop
// }) => {
//   const [isPanelVisible, setIsPanelVisible] = useState(true); // State to manage visibility
//   const theme = useTheme();
//   const isMobile = useMediaQuery(theme.breakpoints.down("sm")); // Detects mobile devices

//   // Handles panel visibility on close
//   const handleClosePanel = () => {
//     setIsPanelVisible(false);
//     closeChatPanel(); // Optional: Trigger parent component's logic
//   };

//   useEffect(() => {
//     setReplyContent("");
//   }, [selectedReview]);

//   if (!isPanelVisible) return null; // Hide the panel when not visible

//   return (
//     <Box
//       flex={isMobile ? 1 : 0.3} // Take full width on mobile
//       bgcolor="white"
//       boxShadow={1}
//       display="flex"
//       flexDirection="column"
//       height="calc(100vh - 64px)"
//       position="relative"
//     >
//       <Box
//         p={2}
//         borderBottom="1px solid #ddd"
//         display="flex"
//         justifyContent="space-between"
//         alignItems="center"
//       >
//         <Typography variant="subtitle1" fontWeight="bold" padding={2}>
//           Response
//         </Typography>
//         <IconButton onClick={handleClosePanel}>
//           <X />
//         </IconButton>
//       </Box>
//       <Box flex={1} p={2} overflow="auto">
//         <Box display="flex" gap={2} mb={2}>
//           <Avatar>{selectedReview?.avatar}</Avatar>
//           <Box>
//             <Typography variant="body1" fontWeight="bold">
//               {selectedReview?.customerName}
//             </Typography>
//             <Typography variant="body2" color="textSecondary">
//               {selectedReview?.date}
//             </Typography>
//             <Typography variant="body2" mt={1}>
//               {selectedReview?.review}
//             </Typography>
//           </Box>
//         </Box>
//         {/* Reply Thread Without Pagination */}
//         {replyThread.map((reply, index) => (
//           <Box key={index} display="flex" gap={2} mb={2} mt={2}>
//             <Avatar>{reply.replied_by[0]}</Avatar>
//             <Box>
//               <Typography variant="body1" fontWeight="bold">
//                 {reply.replied_by}
//               </Typography>
//               <Typography variant="body2" mt={1}>
//                 {reply.reply}
//               </Typography>
//             </Box>
//           </Box>
//         ))}
//       </Box>
//       <Divider />
//       <Box p={2} display="flex" gap={1}>
//         <TextField
//           fullWidth
//           variant="outlined"
//           placeholder="Type here"
//           size="small"
//           sx={{
//             borderRadius: "50px", // Add borderRadius here
//             "& .MuiOutlinedInput-root": {
//               borderRadius: "50px",
//               border: "1px solid #DA9C9C",
//               "&:hover": {
//                 border: "1px solid #DA9C9C",
//               },
//               "&:selected": {
//                 border: "1px solid #DA9C9C",
//               },
//             },
//             "& .MuiOutlinedInput-notchedOutline": {
//               borderStyle: "none",
//               borderWidth: "none",
//             },
//           }}
//           value={replyContent}
//           onChange={(e) => setReplyContent(e.target.value)}
//           onKeyPress={(e) => {
//             if (e.key === "Enter" && !e.shiftKey) {
//               e.preventDefault();
//               handleSubmitReply();
//             }
//           }}
//         />
//         <IconButton>
//           <Paperclip />
//         </IconButton>
//         <IconButton
//           onClick={handleSubmitReply}
//           disabled={!replyContent.trim()}
//         >
//           <Send />
//         </IconButton>
//       </Box>
//     </Box>
//   );
// };

// export default ChatPanel;

import React, { useEffect, useState } from "react";
import {
  Box,
  Typography,
  Divider,
  TextField,
  IconButton,
  Avatar,
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
  closeChatPanel,
}) => {
  const [isPanelVisible, setIsPanelVisible] = useState(true);
  const theme = useTheme();
  const isMobile = useMediaQuery(theme.breakpoints.down("sm"));

  const handleClosePanel = () => {
    setIsPanelVisible(false);
    closeChatPanel();
  };

  useEffect(() => {
    setReplyContent("");
  }, [selectedReview]);

  if (!isPanelVisible) return null;

  return (
    <Box
      flex={isMobile ? 1 : 0.4}
      bgcolor="#ffffff"
      boxShadow={3}
      display="flex"
      flexDirection="column"
      height="calc(100vh - 64px)"
      borderRadius={isMobile ? 0 : "16px"}
      overflow="hidden"
    >
      {/* Header */}
      <Box
        display="flex"
        alignItems="center"
        justifyContent="space-between"
        p={2}
        bgcolor="#f9f0f0"
        borderBottom="1px solid #DA9C9C"
      >
        <Typography
          variant="h1"
          fontWeight="bold"
          sx={{ color: "#DA9C9C", fontSize: "24px" }}
        >
          Response
        </Typography>
        <IconButton onClick={handleClosePanel} sx={{ color: "#DA9C9C" }}>
          <X />
        </IconButton>
      </Box>

      {/* Content */}
      <Box
        flex={1}
        p={2}
        overflow="auto"
        bgcolor="#fafafa"
        display="flex"
        flexDirection="column"
        gap={3}
      >
        {/* Customer Review */}
        <Box
          display="flex"
          alignItems="center"
          justifyContent="flex-start"
          gap={2}
          p={2}
          bgcolor="#ffffff"
          borderRadius="12px"
          boxShadow="0 1px 3px rgba(0, 0, 0, 0.1)"
        >
          <Avatar sx={{ alignSelf: "center", bgcolor: "#DA9C9C" }}>
            {selectedReview?.user_display_name?.[0]}
          </Avatar>
          <Box>
            <Typography variant="body1" fontWeight="bold" color="black">
              {selectedReview?.user_display_name}
            </Typography>
            <Typography variant="body2" color="textSecondary">
              {selectedReview?.review_date}
            </Typography>
            <Typography variant="body2" mt={1}>
              {selectedReview?.review}
            </Typography>
            {selectedReview?.image && (
              <Box mt={1}>
                <img
                  src={selectedReview.image}
                  alt="Attached"
                  style={{
                    maxWidth: "100%",
                    borderRadius: "8px",
                    boxShadow: "0 1px 2px rgba(0, 0, 0, 0.1)",
                  }}
                />
              </Box>
            )}
          </Box>
        </Box>

        {/* Reply Thread */}
        {replyThread.map((reply, index) => (
          <Box
            key={index}
            display="flex"
            alignItems="center"
            justifyContent="flex-end" // Align all boxes to the right
            gap={2}
            p={2}
            bgcolor={reply.isAdmin ? "#fce4ec" : "#ffffff"} // Different background for admin replies
            borderRadius="12px"
            boxShadow="0 1px 3px rgba(0, 0, 0, 0.1)"
            maxWidth="80%" // Dynamic width for a clean look
            ml="auto" // Margin-left to push content to the right
          >
            {/* Content Container */}
            <Box
              display="flex"
              flexDirection="row-reverse" // Reverse the order to place the avatar on the right
              alignItems="center"
              gap={2}
              textAlign="right" // Align text to the right
            >
              {/* Avatar */}
              <Avatar
                sx={{
                  alignSelf: "center",
                  bgcolor: reply.isAdmin ? "#DA9C9C" : "#da9c9c",
                }}
              >
                {reply.replied_by[0]}
              </Avatar>

              {/* Text Content */}
              <Box>
                <Typography
                  variant="body1"
                  fontWeight="bold"
                  color="black" // Name text color set to black
                >
                  {reply.replied_by}
                </Typography>
                <Typography variant="body2" mt={1}>
                  {reply.reply}
                </Typography>
              </Box>
            </Box>
          </Box>
        ))}
      </Box>

      {/* Footer */}
      <Divider />
      <Box
        p={1}
        display="flex"
        alignItems="center"
        gap={0.5}
        bgcolor="#fce4ec"
        borderTop="1px solid #DA9C9C"
      >
        <TextField
          fullWidth
          variant="outlined"
          placeholder="Type your reply here..."
          size="small"
          sx={{
            "& .MuiOutlinedInput-root": {
              backgroundColor: "white",
              padding: "6px 10px", // Additional padding for better spacing
              fontSize: "16px", // Ensure readability
              color: "black", // Neutral text color
              borderRadius: "24px",
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
          <Paperclip color="black" />
        </IconButton>
        <IconButton
          onClick={handleSubmitReply}
          disabled={!replyContent.trim()}
          sx={{
            backgroundColor: replyContent.trim() ? "#DA9C9C" : "#f1f1f1",
            color: replyContent.trim() ? "#ffffff" : "#888",
            borderRadius: "50%",
            "&:hover": {
              backgroundColor: replyContent.trim() ? "#c76a6a" : "#f1f1f1",
            },
          }}
        >
          <Send />
        </IconButton>
      </Box>
    </Box>
  );
};

export default ChatPanel;
