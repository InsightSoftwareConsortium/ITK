# vxl/vnl/CMakeLists.txt

SET( vnl_sources
  vnl_fwd.h
  vnl_config.h
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
  # vnl_numeric_limits can be removed after the release of VXL 1.1.0
  vnl_numeric_limits.cxx       vnl_numeric_limits.h
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

  vnl_int_2x2.h
  vnl_float_1x2.h
  vnl_float_2x1.h
  vnl_float_2x2.h
  vnl_float_1x3.h
  vnl_float_3x1.h
  vnl_float_3x3.h
  vnl_float_3x4.h
  vnl_float_4x3.h
  vnl_float_4x4.h
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

  # Special functions
                               vnl_cross.h
  vnl_gamma.cxx                vnl_gamma.h
  vnl_erf.cxx                  vnl_erf.h
  vnl_sample.cxx               vnl_sample.h
  vnl_unary_function.txx       vnl_unary_function.h
  vnl_identity.h
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

# VNL_ALLOC is a fast but currently thread-unsafe allocator
# used in vnl_vectors. The default is to use it but to
# use the slower, thread-safe allocation, in your
# CMakeListsLocal.txt file add the following lines
# SET(VNL_C_VECTOR_USE_VNL_ALLOC_DEFINED 1)
# ADD_DEFINITIONS(-DVNL_C_VECTOR_USE_VNL_ALLOC=0)

IF(NOT VNL_C_VECTOR_USE_VNL_ALLOC_DEFINED)
  ADD_DEFINITIONS(-DVNL_C_VECTOR_USE_VNL_ALLOC=1)
ENDIF(NOT VNL_C_VECTOR_USE_VNL_ALLOC_DEFINED)

ADD_LIBRARY(vnl ${vnl_sources})
TARGET_LINK_LIBRARIES( vnl vcl )

SUBDIRS(algo)
SUBDIRS(io)

IF( BUILD_EXAMPLES )
  SUBDIRS( examples )
ENDIF( BUILD_EXAMPLES )

IF( BUILD_TESTING )
  SUBDIRS(tests)
ENDIF( BUILD_TESTING )
