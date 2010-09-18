if(ITK_BINARY_DIR)
  set(ITK_TEST_OUTPUT_DIR "${ITK_BINARY_DIR}/Testing/Temporary")
  if(UNIX)
    if("x${CMAKE_INSTALL_PREFIX}" STREQUAL "^x${ITK_BINARY_DIR}/InstallTest$")
      configure_file(${ITKTesting_SOURCE_DIR}/Code/install_test.sh.in
                     ${ITKTesting_BINARY_DIR}/Code/install_test.sh @ONLY
                     IMMEDIATE)
      add_test(Install /bin/sh ${ITKTesting_BINARY_DIR}/Code/install_test.sh)
    endif("x${CMAKE_INSTALL_PREFIX}" STREQUAL "^x${ITK_BINARY_DIR}/InstallTest$")
  endif(UNIX)
else(ITK_BINARY_DIR)
  set(ITK_TEST_OUTPUT_DIR "${ITKTesting_BINARY_DIR}/Testing/Temporary")
endif(ITK_BINARY_DIR)

add_subdirectory(Numerics)
add_subdirectory(Common)
add_subdirectory(BasicFilters)
add_subdirectory(Algorithms)
add_subdirectory(IO)
add_subdirectory(SpatialObject)

if(ITK_USE_PATENTED)
  add_subdirectory(Patented)
endif(ITK_USE_PATENTED)

if(ITK_USE_REVIEW)
  add_subdirectory(Review)
endif(ITK_USE_REVIEW)
