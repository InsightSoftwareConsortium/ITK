# This CMakeLists.txt file handles the creation of files needed by
# other client projects that use VXL.  Nothing is built by this
# CMakeLists.txt file.  This CMakeLists.txt file must be processed by
# CMake after all the other CMakeLists.txt files in the VXL tree,
# which is why the SUBDIRS(config/cmake/export) command is at the end
# of the top level CMakeLists.txt file.

# Needed to get non-cached variable settings used in VXLConfig.cmake.in
INCLUDE( ${MODULE_PATH}/FindNetlib.cmake )
INCLUDE( ${MODULE_PATH}/FindQv.cmake )
INCLUDE( ${MODULE_PATH}/FindZLIB.cmake )
INCLUDE( ${MODULE_PATH}/FindPNG.cmake )
INCLUDE( ${MODULE_PATH}/FindJPEG.cmake )
INCLUDE( ${MODULE_PATH}/FindTIFF.cmake )
INCLUDE( ${MODULE_PATH}/FindMPEG2.cmake )

# Save the compiler settings so another project can import them.
INCLUDE(${CMAKE_ROOT}/Modules/CMakeExportBuildSettings.cmake)
SET(VXL_BUILD_SETTINGS_FILE ${vxl_BINARY_DIR}/VXLBuildSettings.cmake)
CMAKE_EXPORT_BUILD_SETTINGS(${VXL_BUILD_SETTINGS_FILE})

# Save library dependencies.
SET(VXL_LIBRARY_DEPENDS_FILE ${vxl_BINARY_DIR}/VXLLibraryDepends.cmake)
EXPORT_LIBRARY_DEPENDENCIES(${VXL_LIBRARY_DEPENDS_FILE})

# Create the VXLConfig.cmake file for the build tree.
CONFIGURE_FILE(${VXL_CMAKE_DIR}/VXLConfig.cmake.in
               ${vxl_BINARY_DIR}/VXLConfig.cmake @ONLY IMMEDIATE)
