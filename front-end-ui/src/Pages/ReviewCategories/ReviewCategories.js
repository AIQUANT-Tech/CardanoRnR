import React from 'react';
import Sidebar from '../../Components/Sidebar';
import Header from '../../Components/Header';
import CategoriesTable from '../../Components/CategoriesTable';
import './ReviewCategories.css';

const reviewCategories = () => {
  return (
    <div className="reviewCategories">
      <Sidebar />
      <div className="main-content">
        <Header />
        <CategoriesTable />
      </div>
    </div>
  );
};

export default reviewCategories;
