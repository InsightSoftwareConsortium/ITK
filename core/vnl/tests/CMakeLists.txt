# This is core/vnl/tests/CMakeLists.txt

add_executable( vnl_test_all
  # Driver source and utilities
  test_driver.cxx

  # The tests
  test_complex.cxx
  test_complexify.cxx
  test_inverse.cxx
  test_diag_matrix.cxx
  test_diag_matrix_fixed.cxx
  test_math.cxx
  test_na.cxx
  test_matrix.cxx
  test_container_interface.cxx
  test_matrix_fixed.cxx
  test_vector_fixed_ref.cxx
  test_matrix_fixed_ref.cxx
  test_numeric_traits.cxx
  test_power.cxx
  test_quaternion.cxx
  test_polynomial.cxx
  test_real_polynomial.cxx
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
  test_integrant.cxx
  test_crs_index.cxx
  test_sparse_matrix.cxx
  test_pow_log.cxx
  test_vnl_index_sort.cxx
)

if(CMAKE_COMPILER_IS_GNUCXX)
endif()

target_link_libraries(vnl_test_all itkvnl itktestlib ${CMAKE_THREAD_LIBS})

add_executable(vnl_basic_operation_timings basic_operation_timings.cxx)
target_link_libraries(vnl_basic_operation_timings itkvnl)
add_test( NAME vnl_basic_operation_timings COMMAND vnl_basic_operation_timings   )

add_test( NAME vnl_test_complex COMMAND vnl_test_all test_complex                )
add_test( NAME vnl_test_complexify COMMAND vnl_test_all test_complexify             )
add_test( NAME vnl_test_diag_matrix COMMAND vnl_test_all test_diag_matrix            )
add_test( NAME vnl_test_diag_matrix_fixed COMMAND vnl_test_all test_diag_matrix_fixed      )
add_test( NAME vnl_test_inverse COMMAND vnl_test_all test_inverse                )
add_test( NAME vnl_test_math COMMAND vnl_test_all test_math                   )
add_test( NAME vnl_test_matrix COMMAND vnl_test_all test_matrix                 )
add_test( NAME vnl_test_container_interface COMMAND vnl_test_all test_container_interface )
add_test( NAME vnl_test_matrix_fixed COMMAND vnl_test_all test_matrix_fixed           )
add_test( NAME vnl_test_vector_fixed_ref COMMAND vnl_test_all test_vector_fixed_ref       )
add_test( NAME vnl_test_matrix_fixed_ref COMMAND vnl_test_all test_matrix_fixed_ref       )
add_test( NAME vnl_test_numeric_traits COMMAND vnl_test_all test_numeric_traits         )
add_test( NAME vnl_test_na COMMAND vnl_test_all test_na                     )
add_test( NAME vnl_test_random COMMAND vnl_test_all test_random                 )
add_test( NAME vnl_test_polynomial COMMAND vnl_test_all test_polynomial             )
add_test( NAME vnl_test_real_polynomial COMMAND vnl_test_all test_real_polynomial        )
add_test( NAME vnl_test_resize COMMAND vnl_test_all test_resize                 )
add_test( NAME vnl_test_rotation_matrix COMMAND vnl_test_all test_rotation_matrix        )
add_test( NAME vnl_test_sym_matrix COMMAND vnl_test_all test_sym_matrix             )
add_test( NAME vnl_test_transpose COMMAND vnl_test_all test_transpose              )
add_test( NAME vnl_test_fastops COMMAND vnl_test_all test_fastops                )
add_test( NAME vnl_test_vector COMMAND vnl_test_all test_vector                 )
add_test( NAME vnl_test_gamma COMMAND vnl_test_all test_gamma                  )
add_test( NAME vnl_test_arithmetic COMMAND vnl_test_all test_arithmetic             )
add_test( NAME vnl_test_alignment COMMAND vnl_test_all test_alignment              )
add_test( NAME vnl_test_integrant COMMAND vnl_test_all test_integrant              )
add_test( NAME vnl_test_quaternion COMMAND vnl_test_all test_quaternion             )
add_test( NAME vnl_test_crs_index COMMAND vnl_test_all test_crs_index              )
add_test( NAME vnl_test_power COMMAND vnl_test_all test_power                  )
add_test( NAME vnl_test_sparse_matrix COMMAND vnl_test_all test_sparse_matrix          )
add_test( NAME test_pow_log COMMAND vnl_test_all test_pow_log                )
add_test( NAME test_vnl_index_sort COMMAND vnl_test_all test_vnl_index_sort         )

add_executable(vnl_test_include test_include.cxx)
target_link_libraries(vnl_test_include itkvnl)
add_executable(vnl_test_template_include test_template_include.cxx)
target_link_libraries(vnl_test_template_include itkvnl)
