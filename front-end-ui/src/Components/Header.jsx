import React from 'react';
import { useLocation, useNavigate } from 'react-router-dom';
import { IconButton, Typography } from '@mui/material';
import './styles.css';

const Header = () => {
  const location = useLocation();

  const navigate = useNavigate();

  const handleClick = () => {
    navigate("/profile");
  };

  const handleLogout = () => {
    localStorage.removeItem("authToken");
    localStorage.removeItem("user");

    navigate("/");
  }

  // Function to determine header title based on the path
  const getHeaderTitle = (path) => {
    switch (path) {
      case '/categories':
        return 'Review Categories';
      case '/profile':
        return 'My Profile';
      case '/chatreply':
        return 'Customer Review Management';
      default:
        return 'Dashboard';
    }
  };

  return (
    <div className="header">
      <Typography variant="h5">{getHeaderTitle(location.pathname)}</Typography>
      <div className="header-icons">
        <IconButton>
          <svg xmlns="http://www.w3.org/2000/svg" width="36" height="36" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" className="lucide lucide-bell-dot">
            <path d="M10.268 21a2 2 0 0 0 3.464 0" />
            <path d="M13.916 2.314A6 6 0 0 0 6 8c0 4.499-1.411 5.956-2.74 7.327A1 1 0 0 0 4 17h16a1 1 0 0 0 .74-1.673 9 9 0 0 1-.585-.665" />
            <circle cx="18" cy="8" r="3" />
          </svg>
        </IconButton>
        <IconButton onClick={handleClick}>
          <svg xmlns="http://www.w3.org/2000/svg" width="36" height="36" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" className="lucide lucide-circle-user">
            <circle cx="12" cy="12" r="10" />
            <circle cx="12" cy="10" r="3" />
            <path d="M7 20.662V19a2 2 0 0 1 2-2h6a2 2 0 0 1 2 2v1.662" />
          </svg>
        </IconButton>
        <IconButton onClick={handleLogout}>
          <svg xmlns="http://www.w3.org/2000/svg" width="36" height="36" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" className="lucide lucide-log-out">
            <path d="M9 21H5a2 2 0 0 1-2-2V5a2 2 0 0 1 2-2h4" />
            <polyline points="16 17 21 12 16 7" />
            <line x1="21" x2="9" y1="12" y2="12" />
          </svg>
        </IconButton>
      </div>
    </div>
  );
};

export default Header;
