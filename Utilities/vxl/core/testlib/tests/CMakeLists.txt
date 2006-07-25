# This is core/testlib/tests/CMakeLists.txt
SET(EXECUTABLE_OUTPUT_PATH ${vxl_BINARY_DIR}/core/testlib/tests)

ADD_EXECUTABLE( testlib_test_all
   test_driver.cxx

   test_assert.cxx
   test_macros.cxx
   test_args.cxx
   test_root_dir.cxx
)
TARGET_LINK_LIBRARIES( testlib_test_all itktestlib )

ADD_EXECUTABLE( testlib_test_link
   test_link.cxx
)
TARGET_LINK_LIBRARIES( testlib_test_link itktestlib )

ADD_TEST( testlib_assert     ${EXECUTABLE_OUTPUT_PATH}/testlib_test_all test_assert )
ADD_TEST( testlib_macros     ${EXECUTABLE_OUTPUT_PATH}/testlib_test_all test_macros )
ADD_TEST( testlib_args       ${EXECUTABLE_OUTPUT_PATH}/testlib_test_all test_args one two )
ADD_TEST( testlib_root_dir   ${EXECUTABLE_OUTPUT_PATH}/testlib_test_all test_root_dir )
ADD_TEST( testlib_test_link  ${EXECUTABLE_OUTPUT_PATH}/testlib_test_link )
ADD_TEST( testlib_all        ${EXECUTABLE_OUTPUT_PATH}/testlib_test_all all one two )

ADD_EXECUTABLE( testlib_test_include test_include.cxx )
TARGET_LINK_LIBRARIES( testlib_test_include itktestlib )
