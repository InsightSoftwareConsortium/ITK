# vxl/core/vnl/CMakeLists.txt
project(vnl)

# SSE2 rounding in vnl_math is gated purely by the compiler's __SSE2__ macro
# (mandatory on x86-64, never on ARM), so no configure-time option or probe
# is needed.

# If VXL_INSTALL_INCLUDE_DIR is the default value
if("${VXL_INSTALL_INCLUDE_DIR}" STREQUAL "include/vxl")
  set(_config_install_dir ${VXL_INSTALL_INCLUDE_DIR}/core/vnl)
else()
  set(_config_install_dir ${VXL_INSTALL_INCLUDE_DIR}/vnl)
endif()
# vnl_config.h is an obsolete backward-compatibility shim (its options are now
# unconditional). Generate it only when future-legacy code is retained; under
# ITK_FUTURE_LEGACY_REMOVE it is omitted so stale includes fail fast.
if(NOT ITK_FUTURE_LEGACY_REMOVE)
  vxl_configure_file(${CMAKE_CURRENT_LIST_DIR}/vnl_config.h.in
                     ${PROJECT_BINARY_DIR}/vnl_config.h
                     ${_config_install_dir})
endif()

set( vnl_sources
  dll.h

  vnl_fwd.h

  # vector and matrix
  vnl_c_vector.hxx             vnl_c_vector.h
  vnl_vector.hxx               vnl_vector.h
                               vnl_vector_ref.h
  vnl_vector_fixed.hxx         vnl_vector_fixed.h
  vnl_vector_fixed_ref.hxx     vnl_vector_fixed_ref.h
  vnl_matrix.hxx               vnl_matrix.h
                               vnl_matrix_ref.h
  vnl_matrix_fixed.hxx         vnl_matrix_fixed.h
  vnl_matrix_fixed_ref.hxx     vnl_matrix_fixed_ref.h
  vnl_diag_matrix.hxx          vnl_diag_matrix.h
  vnl_diag_matrix_fixed.hxx    vnl_diag_matrix_fixed.h
  vnl_sparse_matrix.hxx        vnl_sparse_matrix.h
  vnl_sym_matrix.hxx           vnl_sym_matrix.h

  # matrix operators
  vnl_fortran_copy.hxx         vnl_fortran_copy.h
  vnl_fortran_copy_fixed.hxx   vnl_fortran_copy_fixed.h
  vnl_det.hxx                  vnl_det.h
                               vnl_transpose.h
                               vnl_inverse.h
                               vnl_power.h
                               vnl_trace.h
  vnl_rank.hxx                 vnl_rank.h

  #
  vnl_alloc.cxx                vnl_alloc.h
  vnl_block.cxx                vnl_block.h
  vnl_math.cxx                 vnl_math.h
  vnl_na.cxx                   vnl_na.h
  vnl_c_na_vector.hxx          vnl_c_na_vector.h
  vnl_copy.cxx                 vnl_copy.h
                               vnl_complex.h
  vnl_error.cxx                vnl_error.h
  vnl_matlab_print.hxx         vnl_matlab_print.h
  vnl_matlab_print_format.cxx  vnl_matlab_print_format.h
  vnl_matlab_print_scalar.cxx  vnl_matlab_print_scalar.h
                               vnl_matlab_print2.h
                               vnl_matlab_header.h
  vnl_matlab_write.cxx         vnl_matlab_write.h
  vnl_matlab_read.cxx          vnl_matlab_read.h
  vnl_matlab_filewrite.cxx     vnl_matlab_filewrite.h
  vnl_matops.cxx               vnl_matops.h
  # Polyniomial representations
  vnl_polynomial.hxx           vnl_polynomial.h
  vnl_real_polynomial.cxx      vnl_real_polynomial.h

  # alternative number representations

  # Quaternions
  vnl_quaternion.hxx           vnl_quaternion.h
  # Finite field and finite ring numbers and arithmetic

  # ops
  vnl_fastops.cxx              vnl_fastops.h
  vnl_operators.h
  vnl_linear_operators_3.h
  vnl_complex_ops.hxx          vnl_complexify.h vnl_real.h vnl_imag.h

  # traits
  vnl_numeric_traits.cxx       vnl_numeric_traits.h
                               vnl_complex_traits.h

  # matrix and vector specialisations
  vnl_int_matrix.cxx           vnl_int_matrix.h
  vnl_T_n.h
  vnl_int_2.h
  vnl_int_3.h
  vnl_int_4.h
  vnl_float_2.h
  vnl_float_3.h
  vnl_float_4.h
  vnl_double_2.h
  vnl_double_3.h
  vnl_double_4.h

  vnl_int_1x1.h
  vnl_int_2x2.h
  vnl_float_1x1.h
  vnl_float_1x2.h
  vnl_float_2x1.h
  vnl_float_2x2.h
  vnl_float_1x3.h
  vnl_float_3x1.h
  vnl_float_3x3.h
  vnl_float_3x4.h
  vnl_float_4x3.h
  vnl_float_4x4.h
  vnl_double_1x1.h
  vnl_double_1x2.h
  vnl_double_2x1.h
  vnl_double_2x2.h
  vnl_double_1x3.h
  vnl_double_3x1.h
  vnl_double_2x3.h
  vnl_double_3x2.h
  vnl_double_3x3.h
  vnl_double_3x4.h
  vnl_double_4x3.h
  vnl_double_4x4.h

  # optimisation
  vnl_cost_function.cxx               vnl_cost_function.h
  vnl_least_squares_function.cxx      vnl_least_squares_function.h
  vnl_least_squares_cost_function.cxx vnl_least_squares_cost_function.h
  vnl_nonlinear_minimizer.cxx         vnl_nonlinear_minimizer.h


  # linear systems
  vnl_linear_system.cxx               vnl_linear_system.h
  vnl_sparse_matrix_linear_system.cxx vnl_sparse_matrix_linear_system.h

  # special matrices
  vnl_rotation_matrix.cxx      vnl_rotation_matrix.h
  vnl_cross_product_matrix.h
  vnl_identity_3x3.h

  # indexing of sparse structures
  vnl_crs_index.cxx            vnl_crs_index.h

  # Special functions
                               vnl_cross.h
  vnl_gamma.cxx                vnl_gamma.h
  vnl_erf.cxx                  vnl_erf.h
  vnl_drand48.cpp              vnl_drand48.h
  vnl_sample.cxx               vnl_sample.h
  vnl_unary_function.hxx       vnl_unary_function.h
  vnl_identity.h
  vnl_random.cxx               vnl_random.h
  vnl_index_sort.h

  # numerical integration
                               vnl_integrant_fnct.h
                               vnl_analytic_integrant.h
  vnl_definite_integral.cxx    vnl_definite_integral.h

  # hardware optimisation
                               vnl_sse.h
)

aux_source_directory(Templates vnl_sources)

vxl_add_library(LIBRARY_NAME itkvnl
  LIBRARY_SOURCES ${vnl_sources}
  HEADER_BUILD_DIR "${CMAKE_CURRENT_BINARY_DIR}"  # vnl_config.h
  HEADER_INSTALL_DIR vnl)
target_link_libraries(itkvnl PUBLIC vcl)
set(_curr_lib_name vnl)
# If VXL_INSTALL_INCLUDE_DIR is the default value
if("${VXL_INSTALL_INCLUDE_DIR}" STREQUAL "include/vxl")
  set(_install_dir ${VXL_INSTALL_INCLUDE_DIR}/core/${_curr_lib_name})
else()
  set(_install_dir ${VXL_INSTALL_INCLUDE_DIR}/${_curr_lib_name})
endif()
set_vxl_library_properties(
     TARGET_NAME itk${_curr_lib_name}
     BASE_NAME ${_curr_lib_name}
     EXPORT_HEADER_FILE ${VXLCORE_BINARY_INCLUDE_DIR}/${_curr_lib_name}/${_curr_lib_name}_export.h
     INSTALL_DIR ${_install_dir}
     USE_HIDDEN_VISIBILITY
)

if( BUILD_TESTING )
  add_subdirectory(tests)
endif()

add_subdirectory(algo)
