# This is core/vnl/io/tests/CMakeLists.txt

ADD_EXECUTABLE( vnl_io_test_all
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

TARGET_LINK_LIBRARIES( vnl_io_test_all vnl_io vsl testlib vpl )

ADD_TEST( vnl_io_test_bignum_io                ${EXECUTABLE_OUTPUT_PATH}/vnl_io_test_all test_bignum_io               )
ADD_TEST( vnl_io_test_diag_matrix_io           ${EXECUTABLE_OUTPUT_PATH}/vnl_io_test_all test_diag_matrix_io          )
ADD_TEST( vnl_io_test_matrix_fixed_io          ${EXECUTABLE_OUTPUT_PATH}/vnl_io_test_all test_matrix_fixed_io         )
ADD_TEST( vnl_io_test_matrix_io                ${EXECUTABLE_OUTPUT_PATH}/vnl_io_test_all test_matrix_io               )
ADD_TEST( vnl_io_test_nonlinear_minimizer_io   ${EXECUTABLE_OUTPUT_PATH}/vnl_io_test_all test_nonlinear_minimizer_io  )
ADD_TEST( vnl_io_test_rational_io              ${EXECUTABLE_OUTPUT_PATH}/vnl_io_test_all test_rational_io             )
ADD_TEST( vnl_io_test_real_npolynomial_io      ${EXECUTABLE_OUTPUT_PATH}/vnl_io_test_all test_real_npolynomial_io     )
ADD_TEST( vnl_io_test_real_polynomial_io       ${EXECUTABLE_OUTPUT_PATH}/vnl_io_test_all test_real_polynomial_io      )
ADD_TEST( vnl_io_test_sparse_matrix_io         ${EXECUTABLE_OUTPUT_PATH}/vnl_io_test_all test_sparse_matrix_io        )
ADD_TEST( vnl_io_test_sym_matrix_io            ${EXECUTABLE_OUTPUT_PATH}/vnl_io_test_all test_sym_matrix_io           )
ADD_TEST( vnl_io_test_vector_fixed_io          ${EXECUTABLE_OUTPUT_PATH}/vnl_io_test_all test_vector_fixed_io         )
ADD_TEST( vnl_io_test_vector_io                ${EXECUTABLE_OUTPUT_PATH}/vnl_io_test_all test_vector_io               )
ADD_TEST( vnl_io_golden_test_vnl_io            ${EXECUTABLE_OUTPUT_PATH}/vnl_io_test_all golden_test_vnl_io           )

ADD_EXECUTABLE( vnl_io_test_include test_include.cxx )
TARGET_LINK_LIBRARIES( vnl_io_test_include vnl_io )
