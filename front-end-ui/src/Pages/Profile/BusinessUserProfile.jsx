import React, { useEffect, useState } from "react";
import {
    Container,
    Box,
    Typography,
    Paper,
    Grid,
    Avatar,
} from "@mui/material";
import Sidebar from "../../Components/Sidebar";
import Header from "../../Components/Header";
import business from "../../assets/businessProfile.svg";

const BusinessUserProfile = () => {
    const [user, setUser] = useState(null);

    // Load user data from localStorage
    useEffect(() => {
        const userData = localStorage.getItem("user");
        if (userData) {
            setUser(JSON.parse(userData));
        }
    }, []);

    return (
        <Grid container sx={{ height: "100vh" }}>
            {/* Sidebar */}
            <Grid
                item
                xs={2}
                sx={{
                    height: "100vh",
                    position: "fixed",  // Keeps the sidebar fixed on the left
                    zIndex: 100,        // Makes sure the sidebar stays on top
                }}
            >
                <Sidebar />
            </Grid>

            {/* Main Content Area */}
            <Grid
                item
                xs={10}
                sx={{
                    marginLeft: "16.6667%",  // Leave space for the sidebar (2/12)
                    padding: 2,
                    height: "100vh",         // Full height
                    display: "flex",
                    flexDirection: "column",
                }}
            >
                {/* Header */}
                <Box sx={{ height: "5vh", width: "100%" }}>
                    <Header />
                </Box>

                {/* Profile Section */}
                <Box
                    sx={{
                        flex: 1,
                        display: "flex",
                        justifyContent: "center",
                        alignItems: "center",
                        padding: 3,
                    }}
                >
                    <Container maxWidth="md">
                        <Paper elevation={5} sx={{ p: 3, width: "100%" }}>
                            <Avatar
                                alt={user ? user.display_name : "User"}
                                sx={{ width: 80, height: 80, margin: "0 auto", mb: 3, background: "#DA9C9C" }}
                            ><img src={business} alt="" />
                                {/* {user ? user.display_name.charAt(0) : "U"} */}
                            </Avatar>
                            {user ? (
                                <Box>
                                    <Typography variant="h5" sx={{ mb: 2, textAlign: "center" }}>
                                        <strong>{user.display_name}</strong>
                                    </Typography>
                                    <Typography variant="h6" sx={{ mb: 2 }}>
                                        <strong>Email:</strong> {user.email}
                                    </Typography>
                                    <Typography variant="h6" sx={{ mb: 2 }}>
                                        <strong>Role:</strong> {user.role}
                                    </Typography>
                                </Box>
                            ) : (
                                <Typography variant="body1" align="center">
                                    No user data found.
                                </Typography>
                            )}
                        </Paper>
                    </Container>
                </Box>
            </Grid>
        </Grid>
    );
};

export default BusinessUserProfile;
