import React from 'react';

const Pagination = ({ 
  currentPage = 1,
  totalItems = 136,
  itemsPerPage = 8,
  onPageChange = () => {}
}) => {
  const totalPages = Math.ceil(totalItems / itemsPerPage);
  const startItem = ((currentPage - 1) * itemsPerPage) + 1;
  const endItem = Math.min(currentPage * itemsPerPage, totalItems);

  const getPageNumbers = () => {
    const pages = [];
    const maxVisiblePages = 4;
    
    if (totalPages <= maxVisiblePages + 2) {
      // Show all pages if total is small
      for (let i = 1; i <= totalPages; i++) {
        pages.push(i);
      }
    } else {
      // Always show first page
      pages.push(1);
      
      if (currentPage <= 3) {
        // Show first 4 pages
        pages.push(2, 3, 4);
      } else if (currentPage >= totalPages - 2) {
        // Show last 4 pages
        pages.push(totalPages - 3, totalPages - 2, totalPages - 1);
      } else {
        // Show pages around current page
        pages.push(currentPage - 1, currentPage, currentPage + 1);
      }
      
      // Always show last page
      pages.push(totalPages);
    }
    
    return pages;
  };

  return (
    <div className="p-4 flex justify-between items-center">
      <div className="text-sm text-gray-600">
        Showing data {startItem} to {endItem} of {totalItems} entries
      </div>
      <div className="flex gap-2">
        {getPageNumbers().map((pageNum, index, array) => {
          // Add ellipsis between non-consecutive pages
          if (index > 0 && pageNum - array[index - 1] > 1) {
            return (
              <React.Fragment key={`ellipsis-${index}`}>
                <span className="px-3 py-1">...</span>
                <button
                  className={`px-3 py-1 border rounded hover:bg-gray-50 ${
                    currentPage === pageNum ? 'bg-rose-100 text-rose-600 border-rose-200' : ''
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
              className={`px-3 py-1 border rounded hover:bg-gray-50 ${
                currentPage === pageNum ? 'bg-rose-100 text-rose-600 border-rose-200' : ''
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