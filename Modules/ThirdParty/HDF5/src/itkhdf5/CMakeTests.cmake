#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://www.hdfgroup.org/licenses.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#

#-----------------------------------------------------------------------------
# Dashboard and Testing Settings
#-----------------------------------------------------------------------------
  set (DART_TESTING_TIMEOUT 1200
      CACHE STRING
      "Timeout in seconds for each test (default 1200=20minutes)"
  )

  # Generate a list of timeouts based on DART_TESTING_TIMEOUT
  math (EXPR CTEST_SHORT_TIMEOUT "${DART_TESTING_TIMEOUT} / 2")
  math (EXPR CTEST_LONG_TIMEOUT "${DART_TESTING_TIMEOUT} * 2")
  math (EXPR CTEST_VERY_LONG_TIMEOUT "${DART_TESTING_TIMEOUT} * 3")

  option (HDF5_DISABLE_TESTS_REGEX "Regex pattern to set execution of specific tests to DISABLED" "")
  mark_as_advanced (HDF5_DISABLE_TESTS_REGEX)

  option (HDF5_TEST_API "Execute HDF5 API tests" OFF)
  mark_as_advanced (HDF5_TEST_API)
  if (HDF5_TEST_API)
    option (HDF5_TEST_API_INSTALL "Install HDF5 API tests" OFF)
    mark_as_advanced (HDF5_TEST_API_INSTALL)

    # Enable HDF5 Async API tests
    option (HDF5_TEST_API_ENABLE_ASYNC "Enable HDF5 Async API tests" OFF)
    mark_as_advanced (HDF5_TEST_API_ENABLE_ASYNC)

    # Build and use HDF5 test driver program for API tests
    option (HDF5_TEST_API_ENABLE_DRIVER "Enable HDF5 API test driver program" OFF)
    mark_as_advanced (HDF5_TEST_API_ENABLE_DRIVER)
    if (HDF5_TEST_API_ENABLE_DRIVER)
      set (HDF5_TEST_API_SERVER "" CACHE STRING "Server executable for running API tests")
      mark_as_advanced (HDF5_TEST_API_SERVER)
    endif ()
  endif ()

  option (HDF5_TEST_VFD "Execute tests with different VFDs" OFF)
  mark_as_advanced (HDF5_TEST_VFD)
  if (HDF5_TEST_VFD)
    option (HDF5_TEST_FHEAP_VFD "Execute tests with different VFDs" ON)
    mark_as_advanced (HDF5_TEST_FHEAP_VFD)

    # Initialize the list of VFDs to be used for testing and create a test folder for each VFD
    H5_SET_VFD_LIST()
  endif ()

  option (HDF5_TEST_PASSTHROUGH_VOL "Execute tests with different passthrough VOL connectors" OFF)
  mark_as_advanced (HDF5_TEST_PASSTHROUGH_VOL)
  if (HDF5_TEST_PASSTHROUGH_VOL)
    option (HDF5_TEST_FHEAP_PASSTHROUGH_VOL "Execute fheap test with different passthrough VOL connectors" ON)
    mark_as_advanced (HDF5_TEST_FHEAP_PASSTHROUGH VOL)
  endif ()

  set (H5_TEST_EXPRESS_LEVEL_DEFAULT "3")
  set (HDF_TEST_EXPRESS "${H5_TEST_EXPRESS_LEVEL_DEFAULT}"
      CACHE STRING "Control testing framework (0-3) (0 = exhaustive testing; 3 = quicker testing)")
  mark_as_advanced (HDF_TEST_EXPRESS)
  if (NOT "${HDF_TEST_EXPRESS}" STREQUAL "")
    set (H5_TEST_EXPRESS_LEVEL_DEFAULT "${HDF_TEST_EXPRESS}")
  endif ()

  enable_testing ()
  include (CTest)

  include (${HDF5_SOURCE_DIR}/CTestConfig.cmake)
  configure_file (${HDF_RESOURCES_DIR}/CTestCustom.cmake ${HDF5_BINARY_DIR}/CTestCustom.ctest @ONLY)

  option (HDF5_TEST_SERIAL "Execute non-parallel tests" ON)
  mark_as_advanced (HDF5_TEST_SERIAL)

  option (HDF5_TEST_TOOLS "Execute tools tests" ON)
  mark_as_advanced (HDF5_TEST_TOOLS)

  option (HDF5_TEST_EXAMPLES "Execute tests on examples" ON)
  mark_as_advanced (HDF5_TEST_EXAMPLES)

  option (HDF5_TEST_SWMR "Execute SWMR tests" ON)
  mark_as_advanced (HDF5_TEST_SWMR)

  option (HDF5_TEST_PARALLEL "Execute parallel tests" ON)
  mark_as_advanced (HDF5_TEST_PARALLEL)

  option (HDF5_TEST_FORTRAN "Execute fortran tests" ON)
  mark_as_advanced (HDF5_TEST_FORTRAN)

  option (HDF5_TEST_CPP "Execute cpp tests" ON)
  mark_as_advanced (HDF5_TEST_CPP)

  option (HDF5_TEST_JAVA "Execute java tests" ON)
  mark_as_advanced (HDF5_TEST_JAVA)

  if (NOT HDF5_EXTERNALLY_CONFIGURED)
    if (EXISTS "${HDF5_TEST_SRC_DIR}" AND IS_DIRECTORY "${HDF5_TEST_SRC_DIR}")
      add_subdirectory (test)
    endif ()
    if (H5_HAVE_PARALLEL)
      if (EXISTS "${HDF5_TEST_PAR_DIR}" AND IS_DIRECTORY "${HDF5_TEST_PAR_DIR}")
        add_subdirectory (testpar)
      endif ()
    endif ()
  endif ()
