import React, { useState } from "react";
import {
  Box,
  Typography,
  Button,
  Snackbar,
  Alert,
  useMediaQuery,
  useTheme,
} from "@mui/material";
import Sidebar from "../../Components/Sidebar";
import Header from "../../Components/Header";
import { motion } from "framer-motion"; // Using framer-motion for smooth animations

const Dashboard = () => {
  const [snackbar, setSnackbar] = useState({
    open: false,
    message: "Coming Soon!",
    severity: "info",
  });

  const theme = useTheme();
  const isMobile = useMediaQuery(theme.breakpoints.down("sm"));

  const handleSnackbarClose = () => {
    setSnackbar({ ...snackbar, open: false });
  };

  return (
    <Box
      display="flex"
      width="100vw"
      height="100vh"
      sx={{
        backgroundSize: "300% 300%",
        animation: "gradientBackground 5s ease infinite", // Smooth gradient animation
      }}
    >
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
          sx={{ height: "84px"}} // Shadow for header
        >
          <Header />
        </Box>

        {/* Main Content Area */}
        <Box
          mt="64px"  // Adjust for header height
          display="flex"
          flex={1}
          flexDirection={isMobile ? "column" : "row"} // Stack on mobile
          justifyContent="center"
          alignItems="center"
          overflow="auto"
        >
          <Box
            display="flex"
            flexDirection="column"
            alignItems="center"
            justifyContent="center"
            flex={1}
            boxShadow={4}
            sx={{
              height: "calc(100vh - 64px)", // Adjust height to take the remaining space
              overflow: "auto",
              textAlign: "center",
              borderRadius: 2,
              background: "rgba(255, 255, 255, 0.9)",
            }}
          >
            <motion.div
              initial={{ opacity: 0, scale: 0.5 }}
              animate={{ opacity: 1, scale: 1 }}
              transition={{ duration: 1, ease: "easeOut" }}
            >
              <Typography
                variant="h3"
                color="textPrimary"
                gutterBottom
                sx={{
                  fontWeight: "bold",
                  letterSpacing: "2px",
                  fontSize: "3rem",
                  textTransform: "uppercase",
                  color: "#DA9C9C", // Bold color for passion
                }}
              >
                Coming Soon!
              </Typography>
            </motion.div>

            <Typography
              variant="body1"
              color="textSecondary"
              paragraph
              sx={{
                fontSize: "1.2rem",
                fontStyle: "italic",
                color: "#000",
                marginBottom: "2rem",
              }}
            >
              We're cooking up something extraordinary just for you. Stay tuned for the magic to unfold.
            </Typography>
          </Box>
        </Box>
      </Box>

      {/* Snackbar for notifications */}
      <Snackbar
        open={snackbar.open}
        autoHideDuration={6000}
        onClose={handleSnackbarClose}
      >
        <Alert
          severity={snackbar.severity}
          onClose={handleSnackbarClose}
          sx={{ borderRadius: 2 }}
        >
          {snackbar.message}
        </Alert>
      </Snackbar>
    </Box>
  );
};

export default Dashboard;
