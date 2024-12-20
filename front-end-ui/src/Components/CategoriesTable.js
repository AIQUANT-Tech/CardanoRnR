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
                                        <EditIcon className="action-icon" />
                                        <DeleteIcon className="action-icon delete-icon" />
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
