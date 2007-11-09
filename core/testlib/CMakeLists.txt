# ./vxl/testlib/CMakeLists.txt

FIND_FILE(TESTLIB_VCL_WHERE_ROOT_DIR_H
          vcl_where_root_dir.h
          ${vxl_BINARY_DIR}/vcl )

IF (TESTLIB_VCL_WHERE_ROOT_DIR_H)
  ADD_DEFINITIONS(-DVCL_WHERE_ROOT_DIR_H_EXISTS)
ENDIF(TESTLIB_VCL_WHERE_ROOT_DIR_H)

SET( testlib_sources

  # Useful utilities for the tests
  testlib_test.h                testlib_test.cxx

  # For use by the driver program
  testlib_register.h

  # The main function of the driver executable
  testlib_main.cxx

  # Used to locate test files in source tree
  testlib_root_dir.h            testlib_root_dir.cxx
)

ADD_LIBRARY( testlib ${testlib_sources} )
TARGET_LINK_LIBRARIES( testlib vcl )
INSTALL_TARGETS( /lib testlib)
INSTALL_NOBASE_HEADER_FILES(/include/vxl/core/testlib ${testlib_sources})

IF( BUILD_TESTING )
  SUBDIRS( tests )
ENDIF( BUILD_TESTING )
