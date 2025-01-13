import React from "react";
import "./styles.css";

const Pagination = ({
  currentPage = 1,
  totalItems = 136,
  itemsPerPage = 4,
  onPageChange = () => {},
}) => {
  const totalPages = Math.ceil(totalItems / itemsPerPage);
  const startItem = (currentPage - 1) * itemsPerPage + 1;
  const endItem = Math.min(currentPage * itemsPerPage, totalItems);

  const getPageNumbers = () => {
    const pages = [];
    const maxVisiblePages = 4;

    if (totalPages <= maxVisiblePages + 2) {
      for (let i = 1; i <= totalPages; i++) {
        pages.push(i);
      }
    } else {
      pages.push(1);

      if (currentPage <= 3) {
        pages.push(2, 3, 4);
      } else if (currentPage >= totalPages - 2) {
        pages.push(totalPages - 3, totalPages - 2, totalPages - 1);
      } else {
        pages.push(currentPage - 1, currentPage, currentPage + 1);
      }

      pages.push(totalPages);
    }

    return pages;
  };

  return (
    <div className="pagination-container">
      <div className="pagination-info">
        Showing data {startItem} to {endItem} of {totalItems} entries
      </div>
      <div className="pagination-buttons">
        {getPageNumbers().map((pageNum, index, array) => {
          if (index > 0 && pageNum - array[index - 1] > 1) {
            return (
              <React.Fragment key={`ellipsis-${index}`}>
                <span className="pagination-ellipsis">...</span>
                <button
                  className={`pagination-button ${
                    currentPage === pageNum ? "active" : ""
                  }`}
                  onClick={() => onPageChange(pageNum)}
                >
                  {pageNum}
                </button>
              </React.Fragment>
            );
          }

          return (
            <button
              key={pageNum}
              className={`pagination-button ${
                currentPage === pageNum ? "active" : ""
              }`}
              onClick={() => onPageChange(pageNum)}
            >
              {pageNum}
            </button>
          );
        })}
      </div>
    </div>
  );
};

export default Pagination;
