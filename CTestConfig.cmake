#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
## This file should be placed in the root directory of your project.
## Then modify the CMakeLists.txt file in the root directory of your
## project to incorporate the testing dashboard.
## # The following are required to uses Dart and the Cdash dashboard
##   ENABLE_TESTING()
##   INCLUDE(CTest)
set (CTEST_PROJECT_NAME "HDF5")
set (CTEST_NIGHTLY_START_TIME "18:00:00 CST")

set (CTEST_DROP_METHOD "http")
if (CTEST_DROP_SITE_INIT)
  set (CTEST_DROP_SITE "${CTEST_DROP_SITE_INIT}")
else ()
  if (CDASH_LOCAL)
    set (CTEST_DROP_SITE "cdash-internal.hdfgroup.org")
  else ()
    set (CTEST_DROP_SITE "cdash.hdfgroup.org")
  endif ()
endif ()
if (CTEST_DROP_LOCATION_INIT)
  set (CTEST_DROP_LOCATION "${CTEST_DROP_LOCATION_INIT}")
else ()
  if (CDASH_LOCAL)
    set (CTEST_DROP_LOCATION "/submit.php?project=HDF5110")
  else ()
    set (CTEST_DROP_LOCATION "/submit.php?project=HDF5")
  endif ()
endif ()
set (CTEST_DROP_SITE_CDASH TRUE)

set (UPDATE_TYPE git)
set (VALGRIND_COMMAND "/usr/bin/valgrind")
set (VALGRIND_COMMAND_OPTIONS "-v --tool=memcheck --leak-check=full --track-fds=yes --num-callers=50 --show-reachable=yes --track-origins=yes --malloc-fill=0xff --free-fill=0xfe")
set (CTEST_MEMORYCHECK_COMMAND "/usr/bin/valgrind")
set (CTEST_MEMORYCHECK_COMMAND_OPTIONS "-v --tool=memcheck --leak-check=full --track-fds=yes --num-callers=50 --show-reachable=yes --track-origins=yes --malloc-fill=0xff --free-fill=0xfe")

set (CTEST_TEST_TIMEOUT 1200 CACHE STRING
    "Maximum time allowed before CTest will kill the test.")
set (DART_TESTING_TIMEOUT 1200 CACHE STRING
    "Maximum time allowed before CTest will kill the test." FORCE)

set (CTEST_SUBMIT_RETRY_DELAY 20 CACHE STRING
    "How long to wait between timed-out CTest submissions.")
