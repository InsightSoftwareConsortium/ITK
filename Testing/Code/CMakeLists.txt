SUBDIRS( Numerics Common BasicFilters Algorithms IO SpatialObject )

IF(ITK_BINARY_DIR)
  SET(ITK_TEST_OUTPUT_DIR "${ITK_BINARY_DIR}/Testing/Temporary")
  IF(UNIX)
    IF("x${CMAKE_INSTALL_PREFIX}" MATCHES "^x${ITK_BINARY_DIR}/InstallTest$")
      CONFIGURE_FILE(${ITKTesting_SOURCE_DIR}/Code/install_test.sh.in
                     ${ITKTesting_BINARY_DIR}/Code/install_test.sh @ONLY
                     IMMEDIATE)
      ADD_TEST(Install /bin/sh ${ITKTesting_BINARY_DIR}/Code/install_test.sh)
    ENDIF("x${CMAKE_INSTALL_PREFIX}" MATCHES "^x${ITK_BINARY_DIR}/InstallTest$")
  ENDIF(UNIX)
ELSE(ITK_BINARY_DIR)
  SET(ITK_TEST_OUTPUT_DIR "${ITKTesting_BINARY_DIR}/Testing/Temporary")
ENDIF(ITK_BINARY_DIR)
