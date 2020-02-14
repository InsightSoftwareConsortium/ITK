# This is core/testlib/tests/CMakeLists.txt

add_executable( testlib_test_all
   test_driver.cxx

   test_assert.cxx
   test_macros.cxx
   test_args.cxx
   test_root_dir.cxx
)
target_link_libraries( testlib_test_all ${VXL_LIB_PREFIX}testlib )

add_test( NAME testlib_assert COMMAND testlib_test_all test_assert )
add_test( NAME testlib_macros COMMAND testlib_test_all test_macros )
add_test( NAME testlib_args COMMAND testlib_test_all test_args one two )
add_test( NAME testlib_root_dir COMMAND testlib_test_all test_root_dir )
add_test( NAME testlib_all COMMAND testlib_test_all all one two )

add_executable( testlib_test_link
   test_link.cxx
)
target_link_libraries( testlib_test_link ${VXL_LIB_PREFIX}testlib )

add_test( NAME testlib_test_link COMMAND testlib_test_link )

add_executable( testlib_test_include test_include.cxx )
target_link_libraries( testlib_test_include ${VXL_LIB_PREFIX}testlib )
