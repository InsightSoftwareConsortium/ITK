# vxl/vnl/algo/CMakeLists.txt

project(vnl_algo)

# most of vnl_algo is a wrapper around the in-tree netlib (itkv3p_netlib),
# whose PUBLIC include interface provides the netlib and linalg headers.

set( vnl_algo_sources
  vnl_algo_fwd.h
  vnl_netlib.h

  # matrix decompositions
  vnl_svd.hxx vnl_svd.h
  vnl_svd_economy.hxx vnl_svd_economy.h
  vnl_svd_fixed.hxx vnl_svd_fixed.h
  vnl_matrix_inverse.hxx vnl_matrix_inverse.h
  vnl_qr.hxx vnl_qr.h
  vnl_scatter_3x3.hxx vnl_scatter_3x3.h
  vnl_cholesky.cxx vnl_cholesky.h
  vnl_ldl_cholesky.cxx vnl_ldl_cholesky.h
  vnl_sparse_lu.cxx vnl_sparse_lu.h
  vnl_real_eigensystem.cxx vnl_real_eigensystem.h
  vnl_symmetric_eigensystem.hxx vnl_symmetric_eigensystem.h
  vnl_generalized_eigensystem.cxx vnl_generalized_eigensystem.h

  # optimisation
  vnl_discrete_diff.cxx vnl_discrete_diff.h
  vnl_levenberg_marquardt.cxx vnl_levenberg_marquardt.h
  vnl_conjugate_gradient.cxx vnl_conjugate_gradient.h
  vnl_lbfgs.cxx vnl_lbfgs.h
  vnl_lbfgsb.cxx vnl_lbfgsb.h
  vnl_amoeba.cxx vnl_amoeba.h
  vnl_powell.cxx vnl_powell.h
  vnl_brent.cxx vnl_brent.h
  vnl_lsqr.cxx vnl_lsqr.h
  vnl_solve_qp.cxx vnl_solve_qp.h
  vnl_bracket_minimum.cxx vnl_bracket_minimum.h
  vnl_brent_minimizer.cxx vnl_brent_minimizer.h

  # equation solvers
  vnl_fit_parabola.h


  # stuff
  vnl_determinant.cxx vnl_determinant.hxx vnl_determinant.h
  vnl_matrix_update.h

  # integral
  vnl_simpson_integral.cxx vnl_simpson_integral.h
  vnl_adaptsimpson_integral.cxx vnl_adaptsimpson_integral.h
)

aux_source_directory(Templates vnl_algo_sources)

# If VXL_INSTALL_INCLUDE_DIR is the default value
if("${VXL_INSTALL_INCLUDE_DIR}" STREQUAL "include/vxl")
  set(_algo_install_dir ${VXL_INSTALL_INCLUDE_DIR}/core/vnl/algo)
else()
  set(_algo_install_dir ${VXL_INSTALL_INCLUDE_DIR}/vnl/algo)
endif()
vxl_add_library(LIBRARY_NAME itkvnl_algo
  LIBRARY_SOURCES ${vnl_algo_sources}
  HEADER_INSTALL_DIR vnl/algo)
target_link_libraries( itkvnl_algo PUBLIC itkv3p_netlib itkvnl )
set(CURR_LIB_NAME vnl_algo)
set_vxl_library_properties(
   TARGET_NAME itk${CURR_LIB_NAME}
   BASE_NAME ${CURR_LIB_NAME}
   EXPORT_HEADER_FILE ${VXLCORE_BINARY_INCLUDE_DIR}/vnl/algo/${CURR_LIB_NAME}_export.h
   INSTALL_DIR   ${_algo_install_dir}
   USE_HIDDEN_VISIBILITY
)

if( BUILD_TESTING )
  add_subdirectory(tests)
endif()
