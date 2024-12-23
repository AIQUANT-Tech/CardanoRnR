const CustomPagination = ({ totalPages, currentPage, onPageChange }) => {
    const handlePageClick = (page) => {
      if (page >= 1 && page <= totalPages) {
        onPageChange(page);
      }
    };
  
    return (
      <div className="custom-pagination">
        <button
          className={`pagination-button ${currentPage === 1 ? 'disabled' : ''}`}
          onClick={() => handlePageClick(currentPage - 1)}
        >
          &lt;
        </button>
        {Array.from({ length: totalPages }, (_, i) => i + 1).map((page) => (
          <button
            key={page}
            className={`pagination-button ${
              page === currentPage ? 'active' : ''
            }`}
            onClick={() => handlePageClick(page)}
          >
            {page}
          </button>
        ))}
        <button
          className={`pagination-button ${
            currentPage === totalPages ? 'disabled' : ''
          }`}
          onClick={() => handlePageClick(currentPage + 1)}
        >
          &gt;
        </button>
      </div>
    );
  };
  