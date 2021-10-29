# This is the CMakeCache file.

########################
# EXTERNAL cache entries
########################

# examples are the tests for plugins
set (H5PL_BUILD_TESTING ON CACHE BOOL "Enable h5pl testing" FORCE)
set (BUILD_EXAMPLES ON CACHE BOOL "Build h5pl Examples" FORCE)

set (HDF5_HDF5_HEADER "h5pubconf.h" CACHE STRING "Name of HDF5 header" FORCE)
set (HDF5_LINK_LIBS ${HDF5_LIBSH_TARGET} CACHE STRING "hdf5 target" FORCE)
#set (HDF5_INCLUDE_DIR $<TARGET_PROPERTY:${HDF5_LIBSH_TARGET},INCLUDE_DIRECTORIES> CACHE PATH "hdf5 include dirs" FORCE)
set (HDF5_INCLUDE_DIR "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR}" CACHE PATH "hdf5 include dirs" FORCE)
set (HDF5_INCLUDE_DIRS "${HDF5_SRC_DIR};${HDF5_SRC_BINARY_DIR}" CACHE PATH "hdf5 include dirs" FORCE)
set (HDF5_DIR ${CMAKE_CURRENT_BINARY_DIR} CACHE STRING "hdf5 build folder" FORCE)

set (HDF5_DUMP_EXECUTABLE $<TARGET_FILE:h5dump-shared> CACHE STRING "hdf5 h5dump target" FORCE)
set (HDF5_REPACK_EXECUTABLE $<TARGET_FILE:h5repack-shared> CACHE STRING "hdf5 h5repack target" FORCE)

set (H5PL_ALLOW_EXTERNAL_SUPPORT "${HDF5_ALLOW_EXTERNAL_SUPPORT}" CACHE STRING "Allow External Library Building (NO GIT TGZ)" FORCE)

set (H5PL_GIT_URL "https://github.com/HDFGroup/hdf5_plugins.git" CACHE STRING "Use plugins from HDF repository" FORCE)
set (H5PL_GIT_BRANCH "master" CACHE STRING "" FORCE)

set (H5PL_TGZ_NAME "${PLUGIN_TGZ_NAME}" CACHE STRING "Use plugins from compressed file" FORCE)

set (PL_PACKAGE_NAME "${PLUGIN_PACKAGE_NAME}" CACHE STRING "Name of plugins package" FORCE)
set (H5PL_CPACK_ENABLE OFF CACHE BOOL "Enable the CPACK include and components" FORCE)
