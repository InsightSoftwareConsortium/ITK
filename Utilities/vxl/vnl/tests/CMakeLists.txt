# This is core/vnl/tests/CMakeLists.txt

INCLUDE( ${MODULE_PATH}/FindNetlib.cmake )

# MESSAGE( NETLIB_FOUND " is " ${NETLIB_FOUND} )
IF(NETLIB_FOUND)

  INCLUDE_DIRECTORIES( ${NETLIB_INCLUDE_DIR} )

  ADD_EXECUTABLE( vnl_test_all
    # Driver source and utilities
    test_driver.cxx
    test_util.cxx       test_util.h

    # The tests
    test_amoeba.cxx
    test_bignum.cxx
    test_cholesky.cxx
    test_complex.cxx
    test_complex_eigensystem.cxx
    test_convolve.cxx
    test_cpoly_roots.cxx
    test_determinant.cxx
    test_inverse.cxx
    test_diag_matrix.cxx
    test_fft.cxx
    test_fft1d.cxx
    test_fft2d.cxx
    test_file_matrix.cxx
    test_finite.cxx
    test_functions.cxx
    test_generalized_eigensystem.cxx
    test_levenberg_marquardt.cxx
    test_math.cxx
    test_matlab.cxx
    test_matrix.cxx
    test_matrix_exp.cxx
    test_matrix_fixed.cxx
    test_minimizers.cxx
    test_numeric_limits.cxx
    test_numeric_traits.cxx
    test_qr.cxx
    test_qsvd.cxx
    test_rational.cxx
    test_real_eigensystem.cxx
    test_real_polynomial.cxx
    test_resize.cxx
    test_rnpoly_roots.cxx
    test_rpoly_roots.cxx
    test_sample.cxx
    test_sparse_matrix.cxx
    test_svd.cxx
    test_sym_matrix.cxx
    test_symmetric_eigensystem.cxx
    test_transpose.cxx
    test_vector.cxx
    test_gamma.cxx
    test_arithmetic.cxx  test_arithmetic_body.h
  )

  IF(CMAKE_COMPILER_IS_GNUCXX)
    SET_SOURCE_FILES_PROPERTIES(test_rational.cxx COMPILE_FLAGS -O1)
    SET_SOURCE_FILES_PROPERTIES(test_bignum.cxx COMPILE_FLAGS -O2)
    SET_SOURCE_FILES_PROPERTIES(test_finite.cxx COMPILE_FLAGS -O0)
  ENDIF(CMAKE_COMPILER_IS_GNUCXX)

  TARGET_LINK_LIBRARIES( vnl_test_all itkvnl_algo itkvnl itktestlib ${CMAKE_THREAD_LIBS} )


  ADD_EXECUTABLE( vnl_basic_operation_timings basic_operation_timings.cxx  )
  TARGET_LINK_LIBRARIES( vnl_basic_operation_timings itkvnl )
IF( CXX_TEST_PATH )
  SET(VNL_TEST_ALL ${CXX_TEST_PATH}/vnl_test_all)
  SET(VNL_TEST_INCLUDE ${CXX_TEST_PATH}/vnl_test_include)
ELSE( CXX_TEST_PATH )
  SET(VNL_TEST_ALL /vnl_test_all)
  SET(VNL_TEST_INCLUDE /vnl_test_include)
ENDIF( CXX_TEST_PATH )


  ADD_TEST( vnl_test_amoeba                   ${VNL_TEST_ALL} test_amoeba                  )
  ADD_TEST( vnl_test_bignum                   ${VNL_TEST_ALL} test_bignum                  )
  ADD_TEST( vnl_test_cholesky                 ${VNL_TEST_ALL} test_cholesky                )
  ADD_TEST( vnl_test_complex                  ${VNL_TEST_ALL} test_complex                 )
  ADD_TEST( vnl_test_complex_eigensystem      ${VNL_TEST_ALL} test_complex_eigensystem     )
  ADD_TEST( vnl_test_convolve                 ${VNL_TEST_ALL} test_convolve                )
  ADD_TEST( vnl_test_cpoly_roots              ${VNL_TEST_ALL} test_cpoly_roots             )
  ADD_TEST( vnl_test_determinant              ${VNL_TEST_ALL} test_determinant             )
  ADD_TEST( vnl_test_diag_matrix              ${VNL_TEST_ALL} test_diag_matrix             )
  ADD_TEST( vnl_test_fft                      ${VNL_TEST_ALL} test_fft                     )
  ADD_TEST( vnl_test_fft1d                    ${VNL_TEST_ALL} test_fft1d                   )
  ADD_TEST( vnl_test_fft2d                    ${VNL_TEST_ALL} test_fft2d                   )
  ADD_TEST( vnl_test_file_matrix              ${VNL_TEST_ALL} test_file_matrix             )
  ADD_TEST( vnl_test_finite                   ${VNL_TEST_ALL} test_finite                  )
  ADD_TEST( vnl_test_functions                ${VNL_TEST_ALL} test_functions               )
  ADD_TEST( vnl_test_generalized_eigensystem  ${VNL_TEST_ALL} test_generalized_eigensystem )
  ADD_TEST( vnl_test_inverse                  ${VNL_TEST_ALL} test_inverse                 )
  ADD_TEST( vnl_test_levenberg_marquardt      ${VNL_TEST_ALL} test_levenberg_marquardt     )
  ADD_TEST( vnl_test_math                     ${VNL_TEST_ALL} test_math                    )
  ADD_TEST( vnl_test_matlab                   ${VNL_TEST_ALL} test_matlab                  )
  ADD_TEST( vnl_test_matrix                   ${VNL_TEST_ALL} test_matrix                  )
  ADD_TEST( vnl_test_matrix_exp               ${VNL_TEST_ALL} test_matrix_exp              )
  ADD_TEST( vnl_test_matrix_fixed             ${VNL_TEST_ALL} test_matrix_fixed            )
  ADD_TEST( vnl_test_minimizers               ${VNL_TEST_ALL} test_minimizers              )
  ADD_TEST( vnl_test_numeric_limits           ${VNL_TEST_ALL} test_numeric_limits          )
  ADD_TEST( vnl_test_numeric_traits           ${VNL_TEST_ALL} test_numeric_traits          )
  ADD_TEST( vnl_test_qr                       ${VNL_TEST_ALL} test_qr                      )
  ADD_TEST( vnl_test_qsvd                     ${VNL_TEST_ALL} test_qsvd                    )
  ADD_TEST( vnl_test_rational                 ${VNL_TEST_ALL} test_rational                )
  ADD_TEST( vnl_test_real_eigensystem         ${VNL_TEST_ALL} test_real_eigensystem        )
  ADD_TEST( vnl_test_real_polynomial          ${VNL_TEST_ALL} test_real_polynomial         )
  ADD_TEST( vnl_test_resize                   ${VNL_TEST_ALL} test_resize                  )
  ADD_TEST( vnl_test_rpoly_roots              ${VNL_TEST_ALL} test_rpoly_roots             )
  ADD_TEST( vnl_test_sample                   ${VNL_TEST_ALL} test_sample                  )
  ADD_TEST( vnl_test_sparse_matrix            ${VNL_TEST_ALL} test_sparse_matrix           )
  ADD_TEST( vnl_test_svd                      ${VNL_TEST_ALL} test_svd                     )
  ADD_TEST( vnl_test_sym_matrix               ${VNL_TEST_ALL} test_sym_matrix              )
  ADD_TEST( vnl_test_symmetric_eigensystem    ${VNL_TEST_ALL} test_symmetric_eigensystem   )
  ADD_TEST( vnl_test_transpose                ${VNL_TEST_ALL} test_transpose               )
  ADD_TEST( vnl_test_vector                   ${VNL_TEST_ALL} test_vector                  )
  ADD_TEST( vnl_test_gamma                    ${VNL_TEST_ALL} test_gamma                   )
  ADD_TEST( vnl_test_arithmetic               ${VNL_TEST_ALL} test_arithmetic              )

ENDIF(NETLIB_FOUND)

ADD_EXECUTABLE( vnl_test_include test_include.cxx )
TARGET_LINK_LIBRARIES( vnl_test_include itkvnl_algo itkvnl )
