# This is core/testlib/tests/CMakeLists.txt

ADD_EXECUTABLE( testlib_test_all
   test_driver.cxx

   test_assert.cxx
   test_macros.cxx
   test_args.cxx
   test_root_dir.cxx
)
TARGET_LINK_LIBRARIES( testlib_test_all itktestlib )

itk_add_test( testlib_assert     ${EXECUTABLE_OUTPUT_PATH}/testlib_test_all test_assert )
itk_add_test( testlib_macros     ${EXECUTABLE_OUTPUT_PATH}/testlib_test_all test_macros )
itk_add_test( testlib_args       ${EXECUTABLE_OUTPUT_PATH}/testlib_test_all test_args one two )
itk_add_test( testlib_root_dir   ${EXECUTABLE_OUTPUT_PATH}/testlib_test_all test_root_dir )
itk_add_test( testlib_all        ${EXECUTABLE_OUTPUT_PATH}/testlib_test_all all one two )

ADD_EXECUTABLE( testlib_test_link
   test_link.cxx
)
TARGET_LINK_LIBRARIES( testlib_test_link itktestlib )

itk_add_test( testlib_test_link  ${EXECUTABLE_OUTPUT_PATH}/testlib_test_link )

ADD_EXECUTABLE( testlib_test_include test_include.cxx )
TARGET_LINK_LIBRARIES( testlib_test_include itktestlib )
