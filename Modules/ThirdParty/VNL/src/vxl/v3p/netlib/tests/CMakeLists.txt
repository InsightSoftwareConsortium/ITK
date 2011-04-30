IF(BUILD_TESTING)
  # test
  ADD_EXECUTABLE( netlib_lbfgs_example lbfgs-example.c )
  itk_add_test( netlib_test_lbfgs ${EXECUTABLE_OUTPUT_PATH}/netlib_lbfgs_example )
  TARGET_LINK_LIBRARIES(netlib_lbfgs_example itkv3p_netlib)
  # test
  ADD_EXECUTABLE( netlib_lbfgsb_example lbfgsb-example1.c )
  itk_add_test( netlib_test_lbfgsb ${EXECUTABLE_OUTPUT_PATH}/netlib_lbfgsb_example )
  TARGET_LINK_LIBRARIES(netlib_lbfgsb_example itkv3p_netlib)
  # test
  # FIXME   ADD_EXECUTABLE( netlib_lsqr_test lsqr-test.c )
  # FIXME   itk_add_test( netlib_test_lsqr ${EXECUTABLE_OUTPUT_PATH}/netlib_lsqr_test )
  # FIXME   TARGET_LINK_LIBRARIES(netlib_lsqr_test itkv3p_netlib)
  # test
  ADD_EXECUTABLE( netlib_slamch_test slamch-test.c )
  itk_add_test( netlib_test_slamch ${EXECUTABLE_OUTPUT_PATH}/netlib_slamch_test )
  TARGET_LINK_LIBRARIES(netlib_slamch_test itkv3p_netlib)
  # test
  ADD_EXECUTABLE( netlib_integral_test integral-test.c )
  itk_add_test( netlib_test_integral ${EXECUTABLE_OUTPUT_PATH}/netlib_integral_test )
  TARGET_LINK_LIBRARIES(netlib_integral_test itkv3p_netlib)
ENDIF(BUILD_TESTING)
