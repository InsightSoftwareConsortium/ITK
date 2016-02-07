# This is core/vnl/tests/CMakeLists.txt

add_executable( vnl_test_all
  # Driver source and utilities
  test_driver.cxx

  # The tests
  test_bignum.cxx
  test_decnum.cxx
  test_complex.cxx
  test_complexify.cxx
  test_inverse.cxx
  test_diag_matrix.cxx
  test_diag_matrix_fixed.cxx
  test_file_matrix.cxx
  test_finite.cxx
  test_math.cxx
  test_na.cxx
  test_matrix.cxx
  test_matrix_exp.cxx
  test_matrix_fixed.cxx
  test_vector_fixed_ref.cxx
  test_matrix_fixed_ref.cxx
  test_numeric_traits.cxx
  test_power.cxx
  test_quaternion.cxx
  test_rational.cxx
  test_polynomial.cxx
  test_real_polynomial.cxx
  test_real_npolynomial.cxx
  test_resize.cxx
  test_rotation_matrix.cxx
  test_sym_matrix.cxx
  test_transpose.cxx
  test_fastops.cxx
  test_vector.cxx
  test_gamma.cxx
  test_random.cxx
  test_alignment.cxx
  test_arithmetic.cxx  test_arithmetic_body.h
  test_hungarian_algorithm.cxx
  test_integrant.cxx
  test_bessel.cxx
  test_crs_index.cxx
  test_sparse_lst_sqr_function.cxx
  test_sparse_matrix.cxx
  test_pow_log.cxx
  test_vnl_index_sort.cxx
)

if(CMAKE_COMPILER_IS_GNUCXX)
 set_source_files_properties(test_rational.cxx PROPERTIES COMPILE_FLAGS -O1)
 set_source_files_properties(test_bignum.cxx PROPERTIES COMPILE_FLAGS -O3)
 set_source_files_properties(test_finite.cxx PROPERTIES COMPILE_FLAGS -O0)
endif()

target_link_libraries(vnl_test_all ${VXL_LIB_PREFIX}vnl ${VXL_LIB_PREFIX}testlib ${CMAKE_THREAD_LIBS})

if(BUILD_CORE_UTILITIES)
  add_executable( vnl_test_with_core_utils
    test_driver_with_core_utils.cxx
    test_matlab.cxx
    test_sample.cxx
  )
  target_link_libraries(vnl_test_with_core_utils ${VXL_LIB_PREFIX}vnl ${VXL_LIB_PREFIX}vpl ${VXL_LIB_PREFIX}vul ${VXL_LIB_PREFIX}testlib ${CMAKE_THREAD_LIBS})
  add_test( NAME vnl_test_matlab COMMAND $<TARGET_FILE:vnl_test_with_core_utils> test_matlab                 )
  add_test( NAME vnl_test_sample COMMAND $<TARGET_FILE:vnl_test_with_core_utils> test_sample                 )
endif()

add_executable(vnl_basic_operation_timings basic_operation_timings.cxx)
target_link_libraries(vnl_basic_operation_timings ${VXL_LIB_PREFIX}vnl)
add_test( NAME vnl_basic_operation_timings COMMAND $<TARGET_FILE:vnl_basic_operation_timings>   )

add_test( NAME vnl_test_bignum COMMAND $<TARGET_FILE:vnl_test_all> test_bignum                 )
add_test( NAME vnl_test_decnum COMMAND $<TARGET_FILE:vnl_test_all> test_decnum                 )
add_test( NAME vnl_test_complex COMMAND $<TARGET_FILE:vnl_test_all> test_complex                )
add_test( NAME vnl_test_complexify COMMAND $<TARGET_FILE:vnl_test_all> test_complexify             )
add_test( NAME vnl_test_diag_matrix COMMAND $<TARGET_FILE:vnl_test_all> test_diag_matrix            )
add_test( NAME vnl_test_diag_matrix_fixed COMMAND $<TARGET_FILE:vnl_test_all> test_diag_matrix_fixed      )
add_test( NAME vnl_test_file_matrix COMMAND $<TARGET_FILE:vnl_test_all> test_file_matrix            )
add_test( NAME vnl_test_finite COMMAND $<TARGET_FILE:vnl_test_all> test_finite                 )
add_test( NAME vnl_test_inverse COMMAND $<TARGET_FILE:vnl_test_all> test_inverse                )
add_test( NAME vnl_test_math COMMAND $<TARGET_FILE:vnl_test_all> test_math                   )
add_test( NAME vnl_test_matrix COMMAND $<TARGET_FILE:vnl_test_all> test_matrix                 )
add_test( NAME vnl_test_matrix_exp COMMAND $<TARGET_FILE:vnl_test_all> test_matrix_exp             )
add_test( NAME vnl_test_matrix_fixed COMMAND $<TARGET_FILE:vnl_test_all> test_matrix_fixed           )
add_test( NAME vnl_test_vector_fixed_ref COMMAND $<TARGET_FILE:vnl_test_all> test_vector_fixed_ref       )
add_test( NAME vnl_test_matrix_fixed_ref COMMAND $<TARGET_FILE:vnl_test_all> test_matrix_fixed_ref       )
add_test( NAME vnl_test_numeric_traits COMMAND $<TARGET_FILE:vnl_test_all> test_numeric_traits         )
add_test( NAME vnl_test_na COMMAND $<TARGET_FILE:vnl_test_all> test_na                     )
add_test( NAME vnl_test_random COMMAND $<TARGET_FILE:vnl_test_all> test_random                 )
add_test( NAME vnl_test_rational COMMAND $<TARGET_FILE:vnl_test_all> test_rational               )
add_test( NAME vnl_test_polynomial COMMAND $<TARGET_FILE:vnl_test_all> test_polynomial             )
add_test( NAME vnl_test_real_polynomial COMMAND $<TARGET_FILE:vnl_test_all> test_real_polynomial        )
add_test( NAME vnl_test_real_npolynomial COMMAND $<TARGET_FILE:vnl_test_all> test_real_npolynomial       )
add_test( NAME vnl_test_resize COMMAND $<TARGET_FILE:vnl_test_all> test_resize                 )
add_test( NAME vnl_test_rotation_matrix COMMAND $<TARGET_FILE:vnl_test_all> test_rotation_matrix        )
add_test( NAME vnl_test_sym_matrix COMMAND $<TARGET_FILE:vnl_test_all> test_sym_matrix             )
add_test( NAME vnl_test_transpose COMMAND $<TARGET_FILE:vnl_test_all> test_transpose              )
add_test( NAME vnl_test_fastops COMMAND $<TARGET_FILE:vnl_test_all> test_fastops                )
add_test( NAME vnl_test_vector COMMAND $<TARGET_FILE:vnl_test_all> test_vector                 )
add_test( NAME vnl_test_gamma COMMAND $<TARGET_FILE:vnl_test_all> test_gamma                  )
add_test( NAME vnl_test_arithmetic COMMAND $<TARGET_FILE:vnl_test_all> test_arithmetic             )
add_test( NAME vnl_test_alignment COMMAND $<TARGET_FILE:vnl_test_all> test_alignment              )
add_test( NAME vnl_test_hungarian_algorithm COMMAND $<TARGET_FILE:vnl_test_all> test_hungarian_algorithm    )
add_test( NAME vnl_test_integrant COMMAND $<TARGET_FILE:vnl_test_all> test_integrant              )
add_test( NAME vnl_test_bessel COMMAND $<TARGET_FILE:vnl_test_all> test_bessel                 )
add_test( NAME vnl_test_quaternion COMMAND $<TARGET_FILE:vnl_test_all> test_quaternion             )
add_test( NAME vnl_test_crs_index COMMAND $<TARGET_FILE:vnl_test_all> test_crs_index              )
add_test( NAME vnl_test_sparse_lst_sqr_function COMMAND $<TARGET_FILE:vnl_test_all> test_sparse_lst_sqr_function)
add_test( NAME vnl_test_power COMMAND $<TARGET_FILE:vnl_test_all> test_power                  )
add_test( NAME vnl_test_sparse_matrix COMMAND $<TARGET_FILE:vnl_test_all> test_sparse_matrix          )
add_test( NAME test_pow_log COMMAND $<TARGET_FILE:vnl_test_all> test_pow_log                )
add_test( NAME test_vnl_index_sort COMMAND $<TARGET_FILE:vnl_test_all> test_vnl_index_sort         )

add_executable(vnl_test_include test_include.cxx)
target_link_libraries(vnl_test_include ${VXL_LIB_PREFIX}vnl)
add_executable(vnl_test_template_include test_template_include.cxx)
target_link_libraries(vnl_test_template_include ${VXL_LIB_PREFIX}vnl)
