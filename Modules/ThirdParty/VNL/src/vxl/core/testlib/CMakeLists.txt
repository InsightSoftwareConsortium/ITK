# ./vxl/testlib/CMakeLists.txt

set(testlib_sources

  # Useful utilities for the tests
  testlib_test.h                testlib_test.cxx

  # For use by the driver program
  testlib_register.h

  # The main function of the driver executable
  testlib_main.cxx
)

vxl_add_library(LIBRARY_NAME itktestlib
  LIBRARY_SOURCES ${testlib_sources}
  HEADER_INSTALL_DIR core/testlib)
target_link_libraries(itktestlib PUBLIC vcl)

if( BUILD_TESTING )
  add_subdirectory(tests)
endif()
