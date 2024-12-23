import React, { useState, useEffect } from 'react';
import {
    Table,
    TableBody,
    TableCell,
    TableContainer,
    TableHead,
    TableRow,
    Button,
    Typography,
    TablePagination,
    Select,
    MenuItem,
    FormControl,
    InputLabel,
    CircularProgress,
} from '@mui/material';
import EditIcon from '@mui/icons-material/Edit';
import DeleteIcon from '@mui/icons-material/Delete';
import AddIcon from '@mui/icons-material/Add';
import './styles.css';

const CategoriesTable = () => {
    const [categoriesData, setCategoriesData] = useState([]);
    const [isLoading, setIsLoading] = useState(true);
    const [page, setPage] = useState(0);
    const [rowsPerPage, setRowsPerPage] = useState(7);
    const [filterStatus, setFilterStatus] = useState('All');

    // Fetch data from backend
    useEffect(() => {
        const fetchCategories = async () => {
            setIsLoading(true);
            try {
                const requestPayload = {
                    review_category_fetch_rq: {
                        header: {
                            user_name: 'Business User',
                            product: 'rnr',
                            request_type: 'FETCH_REVIEW_CATEGORY',
                        },
                    },
                };

                const response = await fetch('http://localhost:8080/api/reviewcategory/getReviewCategoryInfo', {
                    method: 'POST',
                    headers: {
                        'Content-Type': 'application/json',
                    },
                    body: JSON.stringify(requestPayload),
                });

                if (!response.ok) {
                    throw new Error('Failed to fetch categories');
                }

                const data = await response.json();

                setCategoriesData(data.review_category_fetch_rs.category_list);
                setIsLoading(false);
            } catch (error) {
                console.error('Error fetching categories:', error);
                setIsLoading(false);
            }
        };

        fetchCategories();
    }, []);

    // Handle page change
    const handleChangePage = (event, newPage) => {
        setPage(newPage);
    };

    // Handle rows per page change
    const handleChangeRowsPerPage = (event) => {
        setRowsPerPage(parseInt(event.target.value, 10));
        setPage(0);
    };

    // Handle status filter change
    const handleFilterChange = (event) => {
        setFilterStatus(event.target.value);
    };

    // Filtered data based on the status
    const filteredData =
        filterStatus === 'All'
            ? categoriesData
            : categoriesData.filter((row) => row.Status === filterStatus);

    // Paginated data
    const paginatedData = filteredData.slice(
        page * rowsPerPage,
        page * rowsPerPage + rowsPerPage
    );

    return (
        <div className="categories-table">
            <div className="table-header">
                <Typography variant="h6">All Categories</Typography>
                <div style={{ display: 'flex', alignItems: 'center', gap: '16px' }}>
                    {/* Add Categories Button */}
                    <Button
                        variant="contained"
                        startIcon={<AddIcon />}
                        className="add-category-button"
                    >
                        Add Categories
                    </Button>
                    {/* Filter Dropdown */}
                    <FormControl size="small">
                        <InputLabel>Status</InputLabel>
                        <Select
                            value={filterStatus}
                            onChange={handleFilterChange}
                            style={{ minWidth: '120px' }}
                        >
                            <MenuItem value="All">All</MenuItem>
                            <MenuItem value="Active">Active</MenuItem>
                            <MenuItem value="Inactive">Inactive</MenuItem>
                        </Select>
                    </FormControl>
                </div>
            </div>
            {isLoading ? (
                <div style={{ display: 'flex', justifyContent: 'center', padding: '16px' }}>
                    <CircularProgress />
                </div>
            ) : (
                <TableContainer>
                    <Table>
                        <TableHead>
                            <TableRow>
                                <TableCell>Categories Name</TableCell>
                                <TableCell>Created By</TableCell>
                                <TableCell>Status</TableCell>
                                <TableCell>Action</TableCell>
                            </TableRow>
                        </TableHead>
                        <TableBody>
                            {paginatedData.map((row, index) => (
                                <TableRow key={index}>
                                    <TableCell>{row.category_name}</TableCell>
                                    <TableCell>{row.modified_by}</TableCell>
                                    <TableCell>
                                        <Typography className="status">{row.Status}</Typography>
                                    </TableCell>
                                    <TableCell>
  <svg
    xmlns="http://www.w3.org/2000/svg"
    width="30"
    height="30"
    viewBox="0 0 24 24"
    fill="none"
    stroke="currentColor"
    strokeWidth="2"
    strokeLinecap="round"
    strokeLinejoin="round"
    className="action-icon"
  >
    <path d="M21.174 6.812a1 1 0 0 0-3.986-3.987L3.842 16.174a2 2 0 0 0-.5.83l-1.321 4.352a.5.5 0 0 0 .623.622l4.353-1.32a2 2 0 0 0 .83-.497z" />
    <path d="m15 5 4 4" />
  </svg>
  <svg
   xmlns="http://www.w3.org/2000/svg" 
   width="32" 
   height="32" 
   viewBox="0 0 24 24" 
   fill="none" 
   stroke="#ff0000" 
   stroke-width="2" 
   stroke-linecap="round" 
   stroke-linejoin="round" class="lucide lucide-trash-2"><path d="M3 6h18"/><path d="M19 6v14c0 1-1 2-2 2H7c-1 0-2-1-2-2V6"/><path d="M8 6V4c0-1 1-2 2-2h4c1 0 2 1 2 2v2"/><line x1="10" x2="10" y1="11" y2="17"/><line x1="14" x2="14" y1="11" y2="17"/></svg>
</TableCell>

                                </TableRow>
                            ))}
                        </TableBody>
                    </Table>
                </TableContainer>
            )}
            {/* Pagination */}
            {!isLoading && (
                <TablePagination
                    component="div"
                    count={filteredData.length}
                    page={page}
                    onPageChange={handleChangePage}
                    rowsPerPage={rowsPerPage}
                    onRowsPerPageChange={handleChangeRowsPerPage}
                    rowsPerPageOptions={[]}
                />
            )}
        </div>
    );
};

export default CategoriesTable;
