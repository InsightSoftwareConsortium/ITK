# This is core/vnl/io/tests/CMakeLists.txt

add_executable( vnl_io_test_all
  # Driver source
  test_driver.cxx

  # The tests
  golden_test_vnl_io.cxx
  test_bignum_io.cxx
  test_diag_matrix_io.cxx
  test_matrix_fixed_io.cxx
  test_matrix_io.cxx
  test_nonlinear_minimizer_io.cxx
  test_rational_io.cxx
  test_real_npolynomial_io.cxx
  test_real_polynomial_io.cxx
  test_sparse_matrix_io.cxx
  test_sym_matrix_io.cxx
  test_vector_fixed_io.cxx
  test_vector_io.cxx
)

target_link_libraries( vnl_io_test_all ${VXL_LIB_PREFIX}vnl_io ${VXL_LIB_PREFIX}vsl ${VXL_LIB_PREFIX}testlib ${VXL_LIB_PREFIX}vpl )

add_test( NAME vnl_io_test_bignum_io COMMAND $<TARGET_FILE:vnl_io_test_all> test_bignum_io               )
add_test( NAME vnl_io_test_diag_matrix_io COMMAND $<TARGET_FILE:vnl_io_test_all> test_diag_matrix_io          )
add_test( NAME vnl_io_test_matrix_fixed_io COMMAND $<TARGET_FILE:vnl_io_test_all> test_matrix_fixed_io         )
add_test( NAME vnl_io_test_matrix_io COMMAND $<TARGET_FILE:vnl_io_test_all> test_matrix_io               )
add_test( NAME vnl_io_test_nonlinear_minimizer_io COMMAND $<TARGET_FILE:vnl_io_test_all> test_nonlinear_minimizer_io  )
add_test( NAME vnl_io_test_rational_io COMMAND $<TARGET_FILE:vnl_io_test_all> test_rational_io             )
add_test( NAME vnl_io_test_real_npolynomial_io COMMAND $<TARGET_FILE:vnl_io_test_all> test_real_npolynomial_io     )
add_test( NAME vnl_io_test_real_polynomial_io COMMAND $<TARGET_FILE:vnl_io_test_all> test_real_polynomial_io      )
add_test( NAME vnl_io_test_sparse_matrix_io COMMAND $<TARGET_FILE:vnl_io_test_all> test_sparse_matrix_io        )
add_test( NAME vnl_io_test_sym_matrix_io COMMAND $<TARGET_FILE:vnl_io_test_all> test_sym_matrix_io           )
add_test( NAME vnl_io_test_vector_fixed_io COMMAND $<TARGET_FILE:vnl_io_test_all> test_vector_fixed_io         )
add_test( NAME vnl_io_test_vector_io COMMAND $<TARGET_FILE:vnl_io_test_all> test_vector_io               )
add_test( NAME vnl_io_golden_test_vnl_io COMMAND $<TARGET_FILE:vnl_io_test_all> golden_test_vnl_io           )

add_executable( vnl_io_test_include test_include.cxx )
target_link_libraries( vnl_io_test_include ${VXL_LIB_PREFIX}vnl_io )
add_executable( vnl_io_test_template_include test_template_include.cxx )
target_link_libraries( vnl_io_test_template_include ${VXL_LIB_PREFIX}vnl_io )
