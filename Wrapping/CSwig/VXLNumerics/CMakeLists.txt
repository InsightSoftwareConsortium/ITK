# create the VXLNumerics libraries

SET(WRAP_SOURCES
  wrap_vnl_matrix
  wrap_vnl_vector
  wrap_vnl_c_vector
  wrap_vnl_diag_matrix
  wrap_vnl_file_matrix
  wrap_vnl_file_vector
  wrap_vnl_fortran_copy
  wrap_vnl_matrix_fixed
  wrap_vnl_matrix_fixed_ref
  wrap_vnl_matrix_ref
  wrap_vnl_vector_ref
)


SET(MASTER_INDEX_FILES "${WrapITK_BINARY_DIR}/VXLNumerics/VXLNumerics.mdx")

ITK_WRAP_LIBRARY("${WRAP_SOURCES}" VXLNumerics VXLNumerics "" "" "${VXL_NUMERICS_LIBRARIES};itkvnl_inst")

