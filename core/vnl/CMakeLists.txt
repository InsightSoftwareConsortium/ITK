# vxl/core/vnl/CMakeLists.txt

project(vnl)

doxygen_add_library(core/vnl
  DEPENDS core/vsl
  PACKAGE core
  DESCRIPTION "Numerics Library"
  )

# Create vnl_config.h
option(VNL_CONFIG_CHECK_BOUNDS
  "Whether vnl_vector/vnl_matrix accessor methods check index bounds." ON)
option(VNL_CONFIG_LEGACY_METHODS
  "Whether backward-compatibility methods are provided by vnl." OFF)
option(VNL_CONFIG_THREAD_SAFE
  "Whether thread-safe vnl implementations are used." ON)


#if( VXL_HAS_EMMINTRIN_H AND VXL_HAS_SSE2_HARDWARE_SUPPORT )
#  option(VNL_CONFIG_ENABLE_SSE2
#    "Enable Streaming SIMD Extensions 2 optimisations (hardware dependant)." ON)
#else()
  option(VNL_CONFIG_ENABLE_SSE2
    "Enable Streaming SIMD Extensions 2 optimisations (hardware dependant). Currently broken. For use by VNL developers only." OFF)
#endif()

option(VNL_CONFIG_ENABLE_SSE2_ROUNDING
  "Enable Streaming SIMD Extensions 2 implementation of rounding (hardware dependant)."
  ${VXL_HAS_SSE2_HARDWARE_SUPPORT} )
if( VNL_CONFIG_ENABLE_SSE2_ROUNDING )
  if( NOT VXL_HAS_SSE2_HARDWARE_SUPPORT )
    if( VXL_SSE2_HARDWARE_SUPPORT_POSSIBLE )
      message( ${VXL_SSE2_HARDWARE_SUPPORT_POSSIBLE_HELP} )
    endif()
    message( SEND_ERROR "Cannot have VNL_CONFIG_ENABLE_SSE2_ROUNDING because"
                        " there is no SSE2 hardware support" )
    set(VNL_CONFIG_ENABLE_SSE2_ROUNDING 0)
  endif()
endif()


mark_as_advanced(
  VNL_CONFIG_CHECK_BOUNDS
  VNL_CONFIG_LEGACY_METHODS
  VNL_CONFIG_THREAD_SAFE
  VNL_CONFIG_ENABLE_SSE2_ROUNDING
  VNL_CONFIG_ENABLE_SSE2
  )
# Need to enforce 1/0 values for configuration.
if(VNL_CONFIG_CHECK_BOUNDS)
  set(VNL_CONFIG_CHECK_BOUNDS 1)
else()
  set(VNL_CONFIG_CHECK_BOUNDS 0)
endif()
if(VNL_CONFIG_LEGACY_METHODS)
  set(VNL_CONFIG_LEGACY_METHODS 1)
else()
  set(VNL_CONFIG_LEGACY_METHODS 0)
endif()
if(VNL_CONFIG_THREAD_SAFE)
  set(VNL_CONFIG_THREAD_SAFE 1)
else()
  set(VNL_CONFIG_THREAD_SAFE 0)
endif()
if(VNL_CONFIG_ENABLE_SSE2)
  set(VNL_CONFIG_ENABLE_SSE2 1)
else()
  set(VNL_CONFIG_ENABLE_SSE2 0)
endif()
if(VNL_CONFIG_ENABLE_SSE2_ROUNDING)
  set(VNL_CONFIG_ENABLE_SSE2_ROUNDING 1)
else()
  set(VNL_CONFIG_ENABLE_SSE2_ROUNDING 0)
endif()

# If VXL_INSTALL_INCLUDE_DIR is the default value
if("${VXL_INSTALL_INCLUDE_DIR}" STREQUAL "include/vxl")
  set(_config_install_dir ${VXL_INSTALL_INCLUDE_DIR}/core/vnl)
else()
  set(_config_install_dir ${VXL_INSTALL_INCLUDE_DIR}/vnl)
endif()
vxl_configure_file(${CMAKE_CURRENT_LIST_DIR}/vnl_config.h.in
                   ${PROJECT_BINARY_DIR}/vnl_config.h
                   ${_config_install_dir})

set( vnl_sources
  dll.h

  vnl_fwd.h
  vnl_tag.h

  # vector and matrix
  vnl_c_vector.hxx             vnl_c_vector.h
  vnl_vector.hxx               vnl_vector.h
                               vnl_vector_ref.h
  vnl_vector_fixed.hxx         vnl_vector_fixed.h
  vnl_vector_fixed_ref.hxx     vnl_vector_fixed_ref.h
  vnl_file_vector.hxx          vnl_file_vector.h
  vnl_matrix.hxx               vnl_matrix.h
                               vnl_matrix_ref.h
  vnl_matrix_fixed.hxx         vnl_matrix_fixed.h
  vnl_matrix_fixed_ref.hxx     vnl_matrix_fixed_ref.h
  vnl_diag_matrix.hxx          vnl_diag_matrix.h
  vnl_diag_matrix_fixed.hxx    vnl_diag_matrix_fixed.h
  vnl_sparse_matrix.hxx        vnl_sparse_matrix.h
  vnl_matrix_exp.hxx           vnl_matrix_exp.h
  vnl_file_matrix.hxx          vnl_file_matrix.h
  vnl_sym_matrix.hxx           vnl_sym_matrix.h

  # matrix operators
  vnl_fortran_copy.hxx         vnl_fortran_copy.h
  vnl_det.hxx                  vnl_det.h
                               vnl_transpose.h
                               vnl_inverse.h
                               vnl_power.h
                               vnl_trace.h
  vnl_rank.hxx                 vnl_rank.h
  vnl_scalar_join_iterator.hxx vnl_scalar_join_iterator.h

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
  vnl_real_npolynomial.cxx     vnl_real_npolynomial.h

  # alternative number representations

  # Quaternions
  vnl_quaternion.hxx           vnl_quaternion.h
  # Rational number arithmetic; represented by two long integers
  vnl_rational.cxx             vnl_rational.h
  # Arbitrary precision integers
  vnl_bignum.cxx               vnl_bignum.h
  # Arbitrary precision decimal numbers
  vnl_decnum.cxx               vnl_decnum.h
  # Finite field and finite ring numbers and arithmetic
  vnl_finite.h

  # ops
  vnl_fastops.cxx              vnl_fastops.h
  vnl_operators.h
  vnl_linear_operators_3.h
  vnl_complex_ops.hxx          vnl_complexify.h vnl_real.h vnl_imag.h

  # traits
  vnl_numeric_traits.cxx       vnl_numeric_traits.h
                               vnl_complex_traits.h
  vnl_bignum_traits.cxx        vnl_bignum_traits.h
  vnl_decnum_traits.cxx        vnl_decnum_traits.h
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
  vnl_sparse_lst_sqr_function.cxx     vnl_sparse_lst_sqr_function.h
  vnl_nonlinear_minimizer.cxx         vnl_nonlinear_minimizer.h

  vnl_hungarian_algorithm.hxx         vnl_hungarian_algorithm.h

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
  vnl_bessel.cxx               vnl_bessel.h
                               vnl_cross.h
  vnl_gamma.cxx                vnl_gamma.h
                               vnl_beta.h
  vnl_erf.cxx                  vnl_erf.h
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

if(CMAKE_COMPILER_IS_GNUCXX)
  # with optimisation, the is_finite etc functions fail on Alpha for long double:
  set_source_files_properties(vnl_math.cxx PROPERTIES COMPILE_FLAGS -O0)
  set_source_files_properties(vnl_bignum.cxx PROPERTIES COMPILE_FLAGS -O0)
  # and these need too much memory with gcc 3.0 on Alpha:
  set_source_files_properties(Templates/vnl_matrix+long-.cxx PROPERTIES COMPILE_FLAGS -O0)
  set_source_files_properties(Templates/vnl_matrix+ulong-.cxx PROPERTIES COMPILE_FLAGS -O0)
  set_source_files_properties(Templates/vnl_matrix+vcl_complex+double--.cxx PROPERTIES COMPILE_FLAGS -O0)
  set_source_files_properties(Templates/vnl_matrix+vcl_complex+long_double--.cxx PROPERTIES COMPILE_FLAGS -O0)
  set_source_files_properties(Templates/vnl_matrix_fixed+vnl_bignum.3.3-.cxx PROPERTIES COMPILE_FLAGS -O0)
  set_source_files_properties(Templates/vnl_vector+vnl_rational-.cxx PROPERTIES COMPILE_FLAGS -O0)
  set_source_files_properties(Templates/vnl_vector_fixed+vnl_rational.3-.cxx PROPERTIES COMPILE_FLAGS -O0)
  # with optimisation, the vnl_na etc functions fail on x86_64:
  set_source_files_properties(vnl_na.cxx PROPERTIES COMPILE_FLAGS -O1)
  # gcc must have -msse2 option to enable sse2 support
  if(VNL_CONFIG_ENABLE_SSE2)
    add_definitions( -msse2 )
  endif()
  if(VNL_CONFIG_ENABLE_SSE2_ROUNDING)
    add_definitions( -msse2 )
  endif()
endif()

vxl_add_library(LIBRARY_NAME ${VXL_LIB_PREFIX}vnl
  LIBRARY_SOURCES ${vnl_sources}
  HEADER_INSTALL_DIR vnl)
target_link_libraries( ${VXL_LIB_PREFIX}vnl ${VXL_LIB_PREFIX}vcl )
set(_curr_lib_name vnl)
# If VXL_INSTALL_INCLUDE_DIR is the default value
if("${VXL_INSTALL_INCLUDE_DIR}" STREQUAL "include/vxl")
  set(_install_dir ${VXL_INSTALL_INCLUDE_DIR}/core/${_curr_lib_name})
else()
  set(_install_dir ${VXL_INSTALL_INCLUDE_DIR}/${_curr_lib_name})
endif()
set_vxl_library_properties(
     TARGET_NAME ${VXL_LIB_PREFIX}${_curr_lib_name}
     BASE_NAME ${_curr_lib_name}
     EXPORT_HEADER_FILE ${VXLCORE_BINARY_INCLUDE_DIR}/${_curr_lib_name}/${_curr_lib_name}_export.h
     INSTALL_DIR ${_install_dir}
     USE_HIDDEN_VISIBILITY
)

if( BUILD_TESTING )
  add_subdirectory(tests)
endif()

add_subdirectory(algo)

if( BUILD_EXAMPLES )
  add_subdirectory(examples)
endif()
