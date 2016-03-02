if(BUILD_TESTING)
  # test
  add_executable( netlib_lbfgs_example lbfgs-example.c )
  add_test( NAME netlib_test_lbfgs COMMAND $<TARGET_FILE:netlib_lbfgs_example> )
  target_link_libraries(netlib_lbfgs_example ${VXL_LIB_PREFIX}v3p_netlib)
  # test
  add_executable( netlib_lbfgsb_example lbfgsb-example1.c )
  add_test( NAME netlib_test_lbfgsb COMMAND $<TARGET_FILE:netlib_lbfgsb_example> )
  target_link_libraries(netlib_lbfgsb_example ${VXL_LIB_PREFIX}v3p_netlib)
  # test
  add_executable( netlib_lsqr_test lsqr-test.c )
  add_test( NAME netlib_test_lsqr COMMAND $<TARGET_FILE:netlib_lsqr_test> )
  target_link_libraries(netlib_lsqr_test ${VXL_LIB_PREFIX}v3p_netlib)
  # test
  add_executable( netlib_slamch_test slamch-test.c )
  add_test( NAME netlib_test_slamch COMMAND $<TARGET_FILE:netlib_slamch_test> )
  target_link_libraries(netlib_slamch_test ${VXL_LIB_PREFIX}v3p_netlib)
  # test
  add_executable( netlib_tricall tricall.c )
  add_test( NAME netlib_test_tricall COMMAND $<TARGET_FILE:netlib_tricall> )
  target_link_libraries(netlib_tricall ${VXL_LIB_PREFIX}netlib)
  # test
  add_executable( netlib_integral_test integral-test.c )
  add_test( NAME netlib_test_integral COMMAND $<TARGET_FILE:netlib_integral_test> )
  target_link_libraries(netlib_integral_test ${VXL_LIB_PREFIX}v3p_netlib)
endif()
