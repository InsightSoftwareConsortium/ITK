IF(BUILD_TESTING)
  LINK_LIBRARIES( itknetlib )


  # test
  ADD_EXECUTABLE( netlib_lbfgs_example lbfgs-example.c )
  ADD_TEST( netlib_lbfgs_example ${CXX_TEST_PATH}/netlib_lbfgs_example )
  # test
  ADD_EXECUTABLE( netlib_lsqr_test lsqr-test.c )
  ADD_TEST( netlib_lsqr_test ${CXX_TEST_PATH}/netlib_lsqr_test )
  # test
  ADD_EXECUTABLE( netlib_tricall tricall.c )
  ADD_TEST( netlib_tricall ${CXX_TEST_PATH}/netlib_tricall )
ENDIF(BUILD_TESTING)
