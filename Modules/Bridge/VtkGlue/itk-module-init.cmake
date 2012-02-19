#
# Find the packages required by this module
#
find_package(VTK REQUIRED)
set(VERSION_MIN "5.9.20110419")
if (${VTK_VERSION} VERSION_LESS ${VERSION_MIN})
  message(ERROR " VtkGlue requires VTK version ${VERSION_MIN} or newer but the current version is ${VTK_VERSION}")
endif()
