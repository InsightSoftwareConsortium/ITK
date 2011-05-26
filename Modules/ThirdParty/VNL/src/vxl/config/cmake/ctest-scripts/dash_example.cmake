# VXL Example Dashboard Script
#
# Copy this example script and edit as necessary for your client.
# See vxl_common.cmake for more instructions.

# Client maintainer: someone@users.sourceforge.net
set(CTEST_SITE "machine.site")
set(CTEST_BUILD_NAME "Linux-gcc")
#set(CTEST_BUILD_FLAGS "-j2") # parallel build for makefiles
set(CTEST_BUILD_CONFIGURATION Release)
set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
#set(CTEST_UPDATE_COMMAND /path/to/svn)

#set(dashboard_model Experimental)
#set(dashboard_model Continuous)

#set(dashboard_do_memcheck 1)
#set(dashboard_do_coverage 1)

#set(dashboard_cache "
#BUILD_SHARED_LIBS:BOOL=ON
#")

include(${CTEST_SCRIPT_DIRECTORY}/vxl_common.cmake)
