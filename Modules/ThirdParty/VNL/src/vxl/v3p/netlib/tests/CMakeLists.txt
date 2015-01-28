IF(BUILD_TESTING)
  # test
  ADD_EXECUTABLE( netlib_lbfgs_example lbfgs-example.c )
  itk_add_test( NAME netlib_test_lbfgs COMMAND netlib_lbfgs_example )
  TARGET_LINK_LIBRARIES(netlib_lbfgs_example itkv3p_netlib)
  # test
  ADD_EXECUTABLE( netlib_lbfgsb_example lbfgsb-example1.c )
  itk_add_test( NAME netlib_test_lbfgsb COMMAND netlib_lbfgsb_example )
  TARGET_LINK_LIBRARIES(netlib_lbfgsb_example itkv3p_netlib)
  # test
  # FIXME   ADD_EXECUTABLE( netlib_lsqr_test lsqr-test.c )
  # FIXME   itk_add_test( NAME netlib_test_lsqr COMMAND netlib_lsqr_test )
  # FIXME   TARGET_LINK_LIBRARIES(netlib_lsqr_test itkv3p_netlib)
  # test
  ADD_EXECUTABLE( netlib_slamch_test slamch-test.c )
  itk_add_test( NAME netlib_test_slamch COMMAND netlib_slamch_test )
  TARGET_LINK_LIBRARIES(netlib_slamch_test itkv3p_netlib)
  # test
  ADD_EXECUTABLE( netlib_integral_test integral-test.c )
  itk_add_test( NAME netlib_test_integral COMMAND netlib_integral_test )
  TARGET_LINK_LIBRARIES(netlib_integral_test itkv3p_netlib)
ENDIF(BUILD_TESTING)
