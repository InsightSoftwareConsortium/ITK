SET(EXECUTABLE_OUTPUT_PATH ${vxl_BINARY_DIR}/v3p/netlib/tests)

IF(BUILD_TESTING)
  LINK_LIBRARIES( netlib )

  # test
  ADD_EXECUTABLE( netlib_lbfgs_example lbfgs-example.c )
  ADD_TEST( netlib_test_lbfgs netlib_lbfgs_example )
  # test
  ADD_EXECUTABLE( netlib_lsqr_test lsqr-test.c )
  ADD_TEST( netlib_test_lsqr netlib_lsqr_test )
  # test
  ADD_EXECUTABLE( netlib_slamch_test slamch-test.c )
  ADD_TEST( netlib_test_slamch netlib_slamch_test )
  # test
  ADD_EXECUTABLE( netlib_tricall tricall.c )
  ADD_TEST( netlib_test_tricall netlib_tricall )
  # test
  ADD_EXECUTABLE( netlib_integral_test integral-test.c )
  ADD_TEST( netlib_test_integral netlib_integral_test )
ENDIF(BUILD_TESTING)
