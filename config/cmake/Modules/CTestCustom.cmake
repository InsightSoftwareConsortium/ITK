# VXL custom settings for ctest

# This file is only used by ctest if it is in the build tree so it is
# copied to the build tree by vxl/CMakeLists.txt

# default is to report 50 errors and 50 warnings
set(CTEST_CUSTOM_MAXIMUM_NUMBER_OF_ERRORS 500)
set(CTEST_CUSTOM_MAXIMUM_NUMBER_OF_WARNINGS 500)

set(CTEST_CUSTOM_MAXIMUM_FAILED_TEST_OUTPUT_SIZE 102400)

# default is 1024
set(CTEST_CUSTOM_MAXIMUM_PASSED_TEST_OUTPUT_SIZE 102400)
