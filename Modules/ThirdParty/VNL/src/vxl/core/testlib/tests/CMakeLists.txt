# This is core/testlib/tests/CMakeLists.txt

ADD_EXECUTABLE( testlib_test_all
   test_driver.cxx

   test_assert.cxx
   test_macros.cxx
   test_args.cxx
   test_root_dir.cxx
)
TARGET_LINK_LIBRARIES( testlib_test_all itktestlib )

itk_add_test( NAME testlib_assert     COMMAND testlib_test_all test_assert )
itk_add_test( NAME testlib_macros     COMMAND testlib_test_all test_macros )
itk_add_test( NAME testlib_args       COMMAND testlib_test_all test_args one two )
itk_add_test( NAME testlib_root_dir   COMMAND testlib_test_all test_root_dir )
itk_add_test( NAME testlib_all        COMMAND testlib_test_all all one two )

ADD_EXECUTABLE( testlib_test_link
   test_link.cxx
)
TARGET_LINK_LIBRARIES( testlib_test_link itktestlib )

itk_add_test( NAME testlib_test_link  COMMAND testlib_test_link )

ADD_EXECUTABLE( testlib_test_include test_include.cxx )
TARGET_LINK_LIBRARIES( testlib_test_include itktestlib )
