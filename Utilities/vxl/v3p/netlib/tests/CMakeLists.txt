IF(BUILD_TESTING)
  LINK_LIBRARIES( netlib )

  # test
  ADD_EXECUTABLE( netlib_lbfgs_example lbfgs-example.c )
  ADD_TEST( netlib_lbfgs_example netlib_lbfgs_example )
  # test
  ADD_EXECUTABLE( netlib_lsqr_test lsqr-test.c )
  ADD_TEST( netlib_lsqr_test netlib_lsqr_test )
  # test
  ADD_EXECUTABLE( netlib_slamch_test slamch-test.c )
  ADD_TEST( netlib_slamch_test netlib_slamch_test )
  # test
  ADD_EXECUTABLE( netlib_tricall tricall.c )
  ADD_TEST( netlib_tricall netlib_tricall )
ENDIF(BUILD_TESTING)
