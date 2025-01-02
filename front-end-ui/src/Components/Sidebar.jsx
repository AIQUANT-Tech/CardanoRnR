import React from "react";
import {
  List,
  ListItem,
  ListItemText,
  ListItemIcon,
  Typography,
} from "@mui/material";
import { Link, useLocation } from "react-router-dom";
import logo from "../assets/logo.png";
import "./styles.css";

const Sidebar = () => {
  const location = useLocation(); 

  const menuItems = [
    {
      text: "Profile",
      icon: (
        <svg
          xmlns="http://www.w3.org/2000/svg"
          width="36"
          height="36"
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          stroke-linecap="round"
          stroke-linejoin="round"
          class="lucide lucide-user"
        >
          <path d="M19 21v-2a4 4 0 0 0-4-4H9a4 4 0 0 0-4 4v2" />
          <circle cx="12" cy="7" r="4" />
        </svg>
      ),
      path: "/profile",
    },
    {
      text: "Dashboard",
      icon: (
        <svg
          xmlns="http://www.w3.org/2000/svg"
          width="36"
          height="36"
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          stroke-linecap="round"
          stroke-linejoin="round"
          class="lucide lucide-layout-dashboard"
        >
          <rect width="7" height="9" x="3" y="3" rx="1" />
          <rect width="7" height="5" x="14" y="3" rx="1" />
          <rect width="7" height="9" x="14" y="12" rx="1" />
          <rect width="7" height="5" x="3" y="16" rx="1" />
        </svg>
      ),
      path: "/dashboard",
    },
    {
      text: "Review Categories",
      icon: (
        <svg
          xmlns="http://www.w3.org/2000/svg"
          width="36"
          height="36"
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          stroke-linecap="round"
          stroke-linejoin="round"
          class="lucide lucide-layers-2"
        >
          <path d="m16.02 12 5.48 3.13a1 1 0 0 1 0 1.74L13 21.74a2 2 0 0 1-2 0l-8.5-4.87a1 1 0 0 1 0-1.74L7.98 12" />
          <path d="M13 13.74a2 2 0 0 1-2 0L2.5 8.87a1 1 0 0 1 0-1.74L11 2.26a2 2 0 0 1 2 0l8.5 4.87a1 1 0 0 1 0 1.74Z" />
        </svg>
      ),
      path: "/categories",
    },
    {
      text: "Customer Review & Ratings",
      icon: (
        <svg
          xmlns="http://www.w3.org/2000/svg"
          width="36"
          height="36"
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          stroke-linecap="round"
          stroke-linejoin="round"
          class="lucide lucide-list"
        >
          <path d="M3 12h.01" />
          <path d="M3 18h.01" />
          <path d="M3 6h.01" />
          <path d="M8 12h13" />
          <path d="M8 18h13" />
          <path d="M8 6h13" />
        </svg>
      ),
      path: "/reviews",
    },
    {
      text: "Chat & Reply",
      icon: (
        <svg
          xmlns="http://www.w3.org/2000/svg"
          width="36"
          height="36"
          viewBox="0 0 24 24"
          fill="none"
          stroke="currentColor"
          stroke-width="2"
          stroke-linecap="round"
          stroke-linejoin="round"
          class="lucide lucide-message-circle-more"
        >
          <path d="M7.9 20A9 9 0 1 0 4 16.1L2 22Z" />
          <path d="M8 12h.01" />
          <path d="M12 12h.01" />
          <path d="M16 12h.01" />
        </svg>
      ),
      path: "/chatreply",
    },
  ];

  return (
    <div className="sidebar">
      <Typography variant="h6" className="logo">
        <img src={logo} alt="logo" className="logo-img" />
        QuAnT
      </Typography>
      <List>
        {menuItems.map((item, index) => (
          <ListItem
            key={index}
            button
            component={Link}
            to={item.path}
            sx={{
              backgroundColor:
                location.pathname === item.path ? "#DA9C9C" : "transparent",
              borderRadius: "8px",
              "&:hover": {
                backgroundColor: 
                location.pathname === item.path ? "#DA9C9C" : "transparent",
              },        
            }}
          >
            <ListItemIcon>{item.icon}</ListItemIcon>
            <ListItemText primary={item.text} sx={{ color: "black" }} />
          </ListItem>
        ))}
      </List>
    </div>
  );
};

export default Sidebar;
