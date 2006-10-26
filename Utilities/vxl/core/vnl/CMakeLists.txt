# vxl/core/vnl/CMakeLists.txt

# Create vnl_config.h
OPTION(VNL_CONFIG_CHECK_BOUNDS
  "Whether vnl_vector/vnl_matrix accessor methods check index bounds." ON)
OPTION(VNL_CONFIG_LEGACY_METHODS
  "Whether backward-compatibility methods are provided by vnl." OFF)
OPTION(VNL_CONFIG_THREAD_SAFE
  "Whether thread-safe vnl implementations are used." ON)
MARK_AS_ADVANCED(
  VNL_CONFIG_CHECK_BOUNDS
  VNL_CONFIG_LEGACY_METHODS
  VNL_CONFIG_THREAD_SAFE
  )
# Need to enforce 1/0 values for configuration.
IF(VNL_CONFIG_CHECK_BOUNDS)
  SET(VNL_CONFIG_CHECK_BOUNDS 1)
ELSE(VNL_CONFIG_CHECK_BOUNDS)
  SET(VNL_CONFIG_CHECK_BOUNDS 0)
ENDIF(VNL_CONFIG_CHECK_BOUNDS)
IF(VNL_CONFIG_LEGACY_METHODS)
  SET(VNL_CONFIG_LEGACY_METHODS 1)
ELSE(VNL_CONFIG_LEGACY_METHODS)
  SET(VNL_CONFIG_LEGACY_METHODS 0)
ENDIF(VNL_CONFIG_LEGACY_METHODS)
IF(VNL_CONFIG_THREAD_SAFE)
  SET(VNL_CONFIG_THREAD_SAFE 1)
ELSE(VNL_CONFIG_THREAD_SAFE)
  SET(VNL_CONFIG_THREAD_SAFE 0)
ENDIF(VNL_CONFIG_THREAD_SAFE)
CONFIGURE_FILE(${vxl_SOURCE_DIR}/core/vnl/vnl_config.h.in
               ${vxl_BINARY_DIR}/core/vnl/vnl_config.h @ONLY IMMEDIATE)

SET( vnl_sources
  dll.h

  vnl_config.h.in
  vnl_fwd.h
  vnl_tag.h

  # vector and matrix
  vnl_c_vector.txx             vnl_c_vector.h
  vnl_vector.txx               vnl_vector.h
                               vnl_vector_ref.h
  vnl_vector_fixed.txx         vnl_vector_fixed.h
  vnl_vector_fixed_ref.txx     vnl_vector_fixed_ref.h
  vnl_file_vector.txx          vnl_file_vector.h
  vnl_matrix.txx               vnl_matrix.h
                               vnl_matrix_ref.h
  vnl_matrix_fixed.txx         vnl_matrix_fixed.h
  vnl_matrix_fixed_ref.txx     vnl_matrix_fixed_ref.h
  vnl_diag_matrix.txx          vnl_diag_matrix.h
  vnl_sparse_matrix.txx        vnl_sparse_matrix.h
  vnl_matrix_exp.txx           vnl_matrix_exp.h
  vnl_file_matrix.txx          vnl_file_matrix.h
  vnl_sym_matrix.txx           vnl_sym_matrix.h

  # matrix operators
  vnl_fortran_copy.txx         vnl_fortran_copy.h
  vnl_det.txx                  vnl_det.h
                               vnl_transpose.h
                               vnl_inverse.h
                               vnl_trace.h
  vnl_rank.txx                 vnl_rank.h
  vnl_scalar_join_iterator.txx vnl_scalar_join_iterator.h

  #
  vnl_alloc.cxx                vnl_alloc.h
  vnl_block.cxx                vnl_block.h
  vnl_math.cxx                 vnl_math.h
  vnl_copy.cxx                 vnl_copy.h
                               vnl_complex.h
  vnl_error.cxx                vnl_error.h
  vnl_matlab_print.txx         vnl_matlab_print.h
  vnl_matlab_print_format.cxx  vnl_matlab_print_format.h
  vnl_matlab_print_scalar.cxx  vnl_matlab_print_scalar.h
                               vnl_matlab_print2.h
                               vnl_matlab_header.h
  vnl_matlab_write.cxx         vnl_matlab_write.h
  vnl_matlab_read.cxx          vnl_matlab_read.h
  vnl_matlab_filewrite.cxx     vnl_matlab_filewrite.h
  vnl_matops.cxx               vnl_matops.h
  vnl_real_polynomial.cxx      vnl_real_polynomial.h
  vnl_real_npolynomial.cxx     vnl_real_npolynomial.h

  # alternative number representations

  # Quaternions
  vnl_quaternion.txx           vnl_quaternion.h
  # Rational number arithmetic; represented by two long integers
  vnl_rational.cxx             vnl_rational.h
  # Arbitrary precision integers
  vnl_bignum.cxx               vnl_bignum.h
  # Finite field and finite ring numbers and arithmetic
  vnl_finite.h

  # ops
  vnl_fastops.cxx              vnl_fastops.h
  vnl_operators.h
  vnl_linear_operators_3.h
  vnl_complex_ops.txx          vnl_complexify.h vnl_real.h vnl_imag.h

  # traits
  vnl_numeric_traits.cxx       vnl_numeric_traits.h
                               vnl_complex_traits.h
  vnl_bignum_traits.cxx        vnl_bignum_traits.h
  vnl_rational_traits.cxx      vnl_rational_traits.h

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

  vnl_hungarian_algorithm.cxx         vnl_hungarian_algorithm.h

  # linear systems
  vnl_linear_system.cxx               vnl_linear_system.h
  vnl_sparse_matrix_linear_system.cxx vnl_sparse_matrix_linear_system.h

  # special matrices
  vnl_rotation_matrix.cxx      vnl_rotation_matrix.h
  vnl_cross_product_matrix.h
  vnl_identity_3x3.h

  # Special functions
  vnl_bessel.cxx               vnl_bessel.h
                               vnl_cross.h
  vnl_gamma.cxx                vnl_gamma.h
  vnl_erf.cxx                  vnl_erf.h
  vnl_sample.cxx               vnl_sample.h
  vnl_unary_function.txx       vnl_unary_function.h
  vnl_identity.h
  vnl_random.cxx               vnl_random.h

  # numerical integration
                               vnl_integrant_fnct.h
                               vnl_analytic_integrant.h
  vnl_definite_integral.cxx    vnl_definite_integral.h
)

AUX_SOURCE_DIRECTORY(Templates vnl_sources)

IF(CMAKE_COMPILER_IS_GNUCXX)
  # with optimisation, the is_finite etc functions fail on Alpha for long double:
  SET_SOURCE_FILES_PROPERTIES(vnl_math.cxx PROPERTIES COMPILE_FLAGS -O0)
  SET_SOURCE_FILES_PROPERTIES(vnl_bignum.cxx PROPERTIES COMPILE_FLAGS -O0)
  # and these need too much memory with gcc 3.0 on Alpha:
  SET_SOURCE_FILES_PROPERTIES(Templates/vnl_matrix+long-.cxx PROPERTIES COMPILE_FLAGS -O0)
  SET_SOURCE_FILES_PROPERTIES(Templates/vnl_matrix+ulong-.cxx PROPERTIES COMPILE_FLAGS -O0)
  SET_SOURCE_FILES_PROPERTIES(Templates/vnl_matrix+vcl_complex+double--.cxx PROPERTIES COMPILE_FLAGS -O0)
  SET_SOURCE_FILES_PROPERTIES(Templates/vnl_matrix+vcl_complex+long_double--.cxx PROPERTIES COMPILE_FLAGS -O0)
  SET_SOURCE_FILES_PROPERTIES(Templates/vnl_matrix_fixed+vnl_bignum.3.3-.cxx PROPERTIES COMPILE_FLAGS -O0)
  SET_SOURCE_FILES_PROPERTIES(Templates/vnl_vector+vnl_rational-.cxx PROPERTIES COMPILE_FLAGS -O0)
  SET_SOURCE_FILES_PROPERTIES(Templates/vnl_vector_fixed+vnl_rational.3-.cxx PROPERTIES COMPILE_FLAGS -O0)
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

ADD_LIBRARY(itkvnl ${vnl_sources})
TARGET_LINK_LIBRARIES( itkvnl itkvcl )

IF(NOT VXL_INSTALL_NO_DEVELOPMENT)
  INSTALL_NOBASE_HEADER_FILES(${VXL_INSTALL_ROOT}/core/vnl ${vnl_sources})
ENDIF(NOT VXL_INSTALL_NO_DEVELOPMENT)

IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(itkvnl PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

IF(NOT VXL_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS itkvnl
    RUNTIME DESTINATION ${VXL_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${VXL_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${VXL_INSTALL_LIB_DIR_CM24} COMPONENT Development)
ENDIF(NOT VXL_INSTALL_NO_LIBRARIES)

SUBDIRS(algo)

IF( BUILD_TESTING )
  SUBDIRS(tests)
ENDIF( BUILD_TESTING )
