import React from 'react';
import { List, ListItem, ListItemText, ListItemIcon, Typography } from '@mui/material';
import HomeIcon from '@mui/icons-material/Home';
import PersonIcon from '@mui/icons-material/Person';
import CategoryIcon from '@mui/icons-material/Category';
import StarRateIcon from '@mui/icons-material/StarRate';
import ChatIcon from '@mui/icons-material/Chat';
import logo from '../assets/logo.png';
import './styles.css';

const Sidebar = () => {
  const menuItems = [
    { text: 'Profile', icon: <PersonIcon /> },
    { text: 'Dashboard', icon: <HomeIcon /> },
    { text: 'Review Categories', icon: <CategoryIcon /> },
    { text: 'Customer Review & Ratings', icon: <StarRateIcon /> },
    { text: 'Chat & Reply', icon: <ChatIcon /> },
  ];

  return (
    <div className="sidebar">
      <Typography variant="h6" className="logo">
        <img src={logo} alt="logo" className="logo-img" />QuAnT
      </Typography>
      <List>
        {menuItems.map((item, index) => (
          <ListItem key={index} button>
            <ListItemIcon>{item.icon}</ListItemIcon>
            <ListItemText primary={item.text} />
          </ListItem>
        ))}
      </List>
    </div>
  );
};

export default Sidebar;
