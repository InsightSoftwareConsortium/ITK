# ./vxl/testlib/CMakeLists.txt

doxygen_add_library(core/testlib
  PACKAGE core-test
  DESCRIPTION "Testing Library"
  )

find_file(TESTLIB_VCL_WHERE_ROOT_DIR_H
          vcl_where_root_dir.h
          ${PROJECT_BINARY_DIR} )

if(TESTLIB_VCL_WHERE_ROOT_DIR_H)
  add_definitions(-DVCL_WHERE_ROOT_DIR_H_EXISTS)
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

vxl_add_library(LIBRARY_NAME ${VXL_LIB_PREFIX}testlib
  LIBRARY_SOURCES ${testlib_sources}
  HEADER_INSTALL_DIR core/testlib)
target_link_libraries(${VXL_LIB_PREFIX}testlib ${VXL_LIB_PREFIX}vcl)

if( BUILD_TESTING )
  add_subdirectory(tests)
endif()
