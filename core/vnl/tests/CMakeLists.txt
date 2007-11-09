# This is core/vnl/tests/CMakeLists.txt

ADD_EXECUTABLE( vnl_test_all
  # Driver source and utilities
  test_driver.cxx

  # The tests
  test_bignum.cxx
  test_complex.cxx
  test_inverse.cxx
  test_diag_matrix.cxx
  test_file_matrix.cxx
  test_finite.cxx
  test_math.cxx
  test_matlab.cxx
  test_matrix.cxx
  test_matrix_exp.cxx
  test_matrix_fixed.cxx
  test_vector_fixed_ref.cxx
  test_matrix_fixed_ref.cxx
  test_numeric_traits.cxx
  test_quaternion.cxx
  test_rational.cxx
  test_real_polynomial.cxx
  test_real_npolynomial.cxx
  test_resize.cxx
  test_rotation_matrix.cxx
  test_sample.cxx
  test_sym_matrix.cxx
  test_transpose.cxx
  test_fastops.cxx
  test_vector.cxx
  test_gamma.cxx
  test_random.cxx
  test_arithmetic.cxx  test_arithmetic_body.h
  test_hungarian_algorithm.cxx
  test_integrant.cxx
  test_bessel.cxx
)

IF(CMAKE_COMPILER_IS_GNUCXX)
 SET_SOURCE_FILES_PROPERTIES(test_rational.cxx PROPERTIES COMPILE_FLAGS -O1)
 SET_SOURCE_FILES_PROPERTIES(test_bignum.cxx PROPERTIES COMPILE_FLAGS -O3)
 SET_SOURCE_FILES_PROPERTIES(test_finite.cxx PROPERTIES COMPILE_FLAGS -O0)
ENDIF(CMAKE_COMPILER_IS_GNUCXX)

TARGET_LINK_LIBRARIES(vnl_test_all vnl vpl vul testlib ${CMAKE_THREAD_LIBS})

ADD_EXECUTABLE(vnl_basic_operation_timings basic_operation_timings.cxx)
TARGET_LINK_LIBRARIES(vnl_basic_operation_timings vnl)

ADD_TEST( vnl_test_bignum               ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_bignum               )
ADD_TEST( vnl_test_complex              ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_complex              )
ADD_TEST( vnl_test_diag_matrix          ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_diag_matrix          )
ADD_TEST( vnl_test_file_matrix          ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_file_matrix          )
ADD_TEST( vnl_test_finite               ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_finite               )
ADD_TEST( vnl_test_inverse              ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_inverse              )
ADD_TEST( vnl_test_math                 ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_math                 )
ADD_TEST( vnl_test_matlab               ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_matlab               )
ADD_TEST( vnl_test_matrix               ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_matrix               )
ADD_TEST( vnl_test_matrix_exp           ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_matrix_exp           )
ADD_TEST( vnl_test_matrix_fixed         ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_matrix_fixed         )
ADD_TEST( vnl_test_vector_fixed_ref     ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_vector_fixed_ref     )
ADD_TEST( vnl_test_matrix_fixed_ref     ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_matrix_fixed_ref     )
ADD_TEST( vnl_test_numeric_traits       ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_numeric_traits       )
ADD_TEST( vnl_test_random               ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_random               )
ADD_TEST( vnl_test_rational             ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_rational             )
ADD_TEST( vnl_test_real_polynomial      ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_real_polynomial      )
ADD_TEST( vnl_test_real_npolynomial     ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_real_npolynomial     )
ADD_TEST( vnl_test_resize               ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_resize               )
ADD_TEST( vnl_test_rotation_matrix      ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_rotation_matrix      )
ADD_TEST( vnl_test_sample               ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_sample               )
ADD_TEST( vnl_test_sym_matrix           ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_sym_matrix           )
ADD_TEST( vnl_test_transpose            ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_transpose            )
ADD_TEST( vnl_test_fastops              ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_fastops              )
ADD_TEST( vnl_test_vector               ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_vector               )
ADD_TEST( vnl_test_gamma                ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_gamma                )
ADD_TEST( vnl_test_arithmetic           ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_arithmetic           )
ADD_TEST( vnl_test_hungarian_algorithm  ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_hungarian_algorithm  )
ADD_TEST( vnl_test_integrant            ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_integrant            )
ADD_TEST( vnl_test_bessel               ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_bessel               )
ADD_TEST( vnl_test_quaternion           ${EXECUTABLE_OUTPUT_PATH}/vnl_test_all test_quaternion           )

ADD_EXECUTABLE(vnl_test_include test_include.cxx)
TARGET_LINK_LIBRARIES(vnl_test_include vnl)
