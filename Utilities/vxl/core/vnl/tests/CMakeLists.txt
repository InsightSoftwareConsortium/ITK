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
    test_rank.cxx
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
    test_vector_fixed_ref.cxx
    test_matrix_fixed_ref.cxx
    test_minimizers.cxx
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
    test_algo.cxx
  )

  IF(CMAKE_COMPILER_IS_GNUCXX)
    SET_SOURCE_FILES_PROPERTIES(test_rational.cxx PROPERTIES COMPILE_FLAGS -O1)
    SET_SOURCE_FILES_PROPERTIES(test_bignum.cxx PROPERTIES COMPILE_FLAGS -O3)
    SET_SOURCE_FILES_PROPERTIES(test_finite.cxx PROPERTIES COMPILE_FLAGS -O0)
  ENDIF(CMAKE_COMPILER_IS_GNUCXX)

  TARGET_LINK_LIBRARIES( vnl_test_all vnl_algo vnl vpl vul testlib ${CMAKE_THREAD_LIBS} )


  ADD_EXECUTABLE( vnl_basic_operation_timings basic_operation_timings.cxx  )
  TARGET_LINK_LIBRARIES( vnl_basic_operation_timings vnl )


  ADD_TEST( vnl_test_amoeba                   vnl_test_all test_amoeba                  )
  ADD_TEST( vnl_test_bignum                   vnl_test_all test_bignum                  )
  ADD_TEST( vnl_test_cholesky                 vnl_test_all test_cholesky                )
  ADD_TEST( vnl_test_complex                  vnl_test_all test_complex                 )
  ADD_TEST( vnl_test_complex_eigensystem      vnl_test_all test_complex_eigensystem     )
  ADD_TEST( vnl_test_convolve                 vnl_test_all test_convolve                )
  ADD_TEST( vnl_test_cpoly_roots              vnl_test_all test_cpoly_roots             )
  ADD_TEST( vnl_test_determinant              vnl_test_all test_determinant             )
  ADD_TEST( vnl_test_diag_matrix              vnl_test_all test_diag_matrix             )
  ADD_TEST( vnl_test_fft                      vnl_test_all test_fft                     )
  ADD_TEST( vnl_test_fft1d                    vnl_test_all test_fft1d                   )
  ADD_TEST( vnl_test_fft2d                    vnl_test_all test_fft2d                   )
  ADD_TEST( vnl_test_file_matrix              vnl_test_all test_file_matrix             )
  ADD_TEST( vnl_test_finite                   vnl_test_all test_finite                  )
  ADD_TEST( vnl_test_functions                vnl_test_all test_functions               )
  ADD_TEST( vnl_test_generalized_eigensystem  vnl_test_all test_generalized_eigensystem )
  ADD_TEST( vnl_test_inverse                  vnl_test_all test_inverse                 )
  ADD_TEST( vnl_test_levenberg_marquardt      vnl_test_all test_levenberg_marquardt     )
  ADD_TEST( vnl_test_math                     vnl_test_all test_math                    )
  ADD_TEST( vnl_test_matlab                   vnl_test_all test_matlab                  )
  ADD_TEST( vnl_test_matrix                   vnl_test_all test_matrix                  )
  ADD_TEST( vnl_test_matrix_exp               vnl_test_all test_matrix_exp              )
  ADD_TEST( vnl_test_matrix_fixed             vnl_test_all test_matrix_fixed            )
  ADD_TEST( vnl_test_vector_fixed_ref         vnl_test_all test_vector_fixed_ref        )
  ADD_TEST( vnl_test_matrix_fixed_ref         vnl_test_all test_matrix_fixed_ref        )
  ADD_TEST( vnl_test_minimizers               vnl_test_all test_minimizers              )
  ADD_TEST( vnl_test_numeric_traits           vnl_test_all test_numeric_traits          )
  ADD_TEST( vnl_test_qr                       vnl_test_all test_qr                      )
  ADD_TEST( vnl_test_qsvd                     vnl_test_all test_qsvd                    )
  ADD_TEST( vnl_test_rational                 vnl_test_all test_rational                )
  ADD_TEST( vnl_test_real_eigensystem         vnl_test_all test_real_eigensystem        )
  ADD_TEST( vnl_test_real_polynomial          vnl_test_all test_real_polynomial         )
  ADD_TEST( vnl_test_resize                   vnl_test_all test_resize                  )
  ADD_TEST( vnl_test_rnpoly_roots             vnl_test_all test_rnpoly_roots            )
  ADD_TEST( vnl_test_rpoly_roots              vnl_test_all test_rpoly_roots             )
  ADD_TEST( vnl_test_sample                   vnl_test_all test_sample                  )
  ADD_TEST( vnl_test_sparse_matrix            vnl_test_all test_sparse_matrix           )
  ADD_TEST( vnl_test_svd                      vnl_test_all test_svd                     )
  ADD_TEST( vnl_test_sym_matrix               vnl_test_all test_sym_matrix              )
  ADD_TEST( vnl_test_symmetric_eigensystem    vnl_test_all test_symmetric_eigensystem   )
  ADD_TEST( vnl_test_rank                     vnl_test_all test_rank                    )
  ADD_TEST( vnl_test_transpose                vnl_test_all test_transpose               )
  ADD_TEST( vnl_test_vector                   vnl_test_all test_vector                  )
  ADD_TEST( vnl_test_gamma                    vnl_test_all test_gamma                   )
  ADD_TEST( vnl_test_arithmetic               vnl_test_all test_arithmetic              )
  ADD_TEST( vnl_test_algo                     vnl_test_all test_algo                    )

ENDIF(NETLIB_FOUND)

ADD_EXECUTABLE( vnl_test_include test_include.cxx )
TARGET_LINK_LIBRARIES( vnl_test_include vnl_algo vnl )
