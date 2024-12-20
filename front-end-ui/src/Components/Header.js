import React from 'react';
import { IconButton, Typography } from '@mui/material';
import NotificationsIcon from '@mui/icons-material/Notifications';
import AccountCircleIcon from '@mui/icons-material/AccountCircle';
import ExitToAppIcon from '@mui/icons-material/ExitToApp';
import './styles.css';

const Header = () => {
  return (
    <div className="header">
      <Typography variant="h5">Reviews Categories</Typography>
      <div className="header-icons">
        <IconButton>
          <NotificationsIcon />
        </IconButton>
        <IconButton>
          <AccountCircleIcon />
        </IconButton>
        <IconButton>
          <ExitToAppIcon />
        </IconButton>
      </div>
    </div>
  );
};

export default Header;
