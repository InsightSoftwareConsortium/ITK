# ./vxl/testlib/CMakeLists.txt

find_file(TESTLIB_VCL_WHERE_ROOT_DIR_H
          vcl_where_root_dir.h
          ${VXL_BINARY_DIR}/vcl )

if(TESTLIB_VCL_WHERE_ROOT_DIR_H)
  add_compile_definitions(VCL_WHERE_ROOT_DIR_H_EXISTS)
else()
  unset(TESTLIB_VCL_WHERE_ROOT_DIR_H)
endif()

set(testlib_sources

  # Useful utilities for the tests
  testlib_test.h                testlib_test.cxx

  # For use by the driver program
  testlib_register.h

  # The main function of the driver executable
  testlib_main.cxx

  # Used to locate test files in source tree
  testlib_root_dir.h            testlib_root_dir.cxx
)

vxl_add_library(LIBRARY_NAME itktestlib
  LIBRARY_SOURCES ${testlib_sources}
  HEADER_INSTALL_DIR core/testlib)
target_link_libraries(itktestlib PUBLIC vcl)

if( BUILD_TESTING )
  add_subdirectory(tests)
endif()
