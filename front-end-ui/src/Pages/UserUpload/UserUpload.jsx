import React, { useState } from "react";
import { Button, Container, Typography, Box, Alert, Paper, IconButton } from "@mui/material";
import { styled } from "@mui/material/styles";
import CloudUploadIcon from "@mui/icons-material/CloudUpload";
import CheckCircleIcon from "@mui/icons-material/CheckCircle";
import ErrorIcon from "@mui/icons-material/Error";

const Input = styled("input")({
  display: "none",
});

const UserUpload = () => {
  const [file, setFile] = useState(null);
  const [error, setError] = useState(null);
  const [success, setSuccess] = useState(null);

  const handleFileChange = (event) => {
    setFile(event.target.files[0]);
    setError(null);
    setSuccess(null);
  };

  const handleUpload = async () => {
    if (!file) {
      setError("Please select a file before uploading.");
      return;
    }

    const formData = new FormData();
    formData.append("file", file);

    try {
      const response = await fetch("http://localhost:8080/api/user/uploadUser", {
        method: "POST",
        body: formData,
      });

      if (!response.ok) {
        throw new Error("Failed to upload file");
      }

      const data = await response.json();
      console.log("Upload successful:", data);
      setSuccess("File uploaded successfully!");
      setFile(null);
    } catch (error) {
      setError("Upload error: " + error.message);
    }
  };

  return (
    <Container maxWidth="sm">
      <Box textAlign="center" mt={5}>
        <Paper elevation={5} sx={{ p: 4, borderRadius: 3, backgroundColor: "#f9f9f9" }}>
          <Typography variant="h4" gutterBottom fontWeight="bold" color="primary">
            Upload JSON File
          </Typography>
          {error && (
            <Alert severity="error" sx={{ mb: 2, display: "flex", alignItems: "center" }}>
              {/* <ErrorIcon sx={{ mr: 1 }} /> {error} */}{error}
            </Alert>
          )}
          {success && (
            <Alert severity="success" sx={{ mb: 2, display: "flex", alignItems: "center" }}>
              <CheckCircleIcon sx={{ mr: 1 }} /> {success}
            </Alert>
          )}
          <Box mb={3}>
            <label htmlFor="upload-file">
              <Input accept=".json" id="upload-file" type="file" onChange={handleFileChange} />
              <Button
                variant="contained"
                component="span"
                startIcon={<CloudUploadIcon />}
                sx={{ px: 4, fontSize: "1rem" }}
              >
                Choose File
              </Button>
            </label>
          </Box>
          {file && <Typography mt={2} fontSize="1.1rem" color="text.secondary">{file.name}</Typography>}
          <Button
            variant="contained"
            color="secondary"
            onClick={handleUpload}
            sx={{ px: 5, fontSize: "1rem" }}
          >
            Upload
          </Button>
        </Paper>
      </Box>
    </Container>
  );
};

export default UserUpload;