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
  add_executable( netlib_lsqrTest1 lsqrTest1.cxx )
  add_test( NAME netlib_test1_lsqr COMMAND $<TARGET_FILE:netlib_lsqrTest1> )
  target_link_libraries(netlib_lsqrTest1 ${VXL_LIB_PREFIX}v3p_netlib)
  # test
  add_executable( netlib_lsqrTest2 lsqrTest2.cxx )
  add_test( NAME netlib_test2_lsqr COMMAND $<TARGET_FILE:netlib_lsqrTest2> )
  target_link_libraries(netlib_lsqrTest2 ${VXL_LIB_PREFIX}v3p_netlib)
  # test
  add_executable( netlib_lsmrTest2 lsmrTest2.cxx )
  add_test( NAME netlib_test2_lsmr COMMAND $<TARGET_FILE:netlib_lsmrTest2> )
  target_link_libraries(netlib_lsmrTest2 ${VXL_LIB_PREFIX}v3p_netlib)
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
