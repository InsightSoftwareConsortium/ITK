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
#-------------------------------------------------------------------------------
macro (ORIGINAL_ZLIB_LIBRARY compress_type)
  if (${compress_type} MATCHES "GIT")
    FetchContent_Declare (HDF5_ZLIB
        GIT_REPOSITORY ${ZLIB_URL}
        GIT_TAG ${ZLIB_BRANCH}
    )
  elseif (${compress_type} MATCHES "TGZ")
    message (VERBOSE "Filter ZLIB file ${ZLIB_URL}")
    FetchContent_Declare (HDF5_ZLIB
        URL ${ZLIB_URL}
        URL_HASH ""
    )
  endif ()
  FetchContent_GetProperties(HDF5_ZLIB)
  if(NOT hdf5_zlib_POPULATED)
    FetchContent_Populate(HDF5_ZLIB)

    # Copy an additional/replacement files into the populated source
    file(COPY ${HDF_RESOURCES_DIR}/ZLIB/CMakeLists.txt DESTINATION ${hdf5_zlib_SOURCE_DIR})

    add_subdirectory(${hdf5_zlib_SOURCE_DIR} ${hdf5_zlib_BINARY_DIR})
  endif()

  add_library(${HDF_PACKAGE_NAMESPACE}zlib-static ALIAS zlib-static)
  set (ZLIB_STATIC_LIBRARY "${HDF_PACKAGE_NAMESPACE}zlib-static")
  set (ZLIB_LIBRARIES ${ZLIB_STATIC_LIBRARY})

  set (ZLIB_INCLUDE_DIR_GEN "${hdf5_zlib_BINARY_DIR}")
  set (ZLIB_INCLUDE_DIR "${hdf5_zlib_SOURCE_DIR}")
  set (ZLIB_FOUND 1)
  set (ZLIB_INCLUDE_DIRS ${ZLIB_INCLUDE_DIR_GEN} ${ZLIB_INCLUDE_DIR})
endmacro ()

#-------------------------------------------------------------------------------
macro (ORIGINAL_SZIP_LIBRARY compress_type encoding)
  # Only libaec library is usable
  if (${compress_type} MATCHES "GIT")
    FetchContent_Declare (SZIP
        GIT_REPOSITORY ${SZIP_URL}
        GIT_TAG ${SZIP_BRANCH}
    )
  elseif (${compress_type} MATCHES "TGZ")
    message (VERBOSE "Filter SZIP file ${SZIP_URL}")
    FetchContent_Declare (SZIP
        URL ${SZIP_URL}
        URL_HASH ""
    )
  endif ()
  FetchContent_GetProperties(SZIP)
  if(NOT szip_POPULATED)
    FetchContent_Populate(SZIP)

    # Copy an additional/replacement files into the populated source
    file(COPY ${HDF_RESOURCES_DIR}/LIBAEC/CMakeLists.txt DESTINATION ${szip_SOURCE_DIR})

    add_subdirectory(${szip_SOURCE_DIR} ${szip_BINARY_DIR})
  endif()

  add_library (${HDF_PACKAGE_NAMESPACE}szaec-static ALIAS szaec-static)
  add_library (${HDF_PACKAGE_NAMESPACE}aec-static ALIAS aec-static)
  set (SZIP_STATIC_LIBRARY "${HDF_PACKAGE_NAMESPACE}szaec-static;${HDF_PACKAGE_NAMESPACE}aec-static")
  set (SZIP_LIBRARIES ${SZIP_STATIC_LIBRARY})

  set (SZIP_INCLUDE_DIR_GEN "${szip_BINARY_DIR}")
  set (SZIP_INCLUDE_DIR "${szip_SOURCE_DIR}/include")
  set (SZIP_FOUND 1)
  set (SZIP_INCLUDE_DIRS ${SZIP_INCLUDE_DIR_GEN} ${SZIP_INCLUDE_DIR})
endmacro ()

#-------------------------------------------------------------------------------
macro (EXTERNAL_SZIP_LIBRARY compress_type encoding)
  if (${compress_type} MATCHES "GIT")
    EXTERNALPROJECT_ADD (SZIP
        GIT_REPOSITORY ${SZIP_URL}
        GIT_TAG ${SZIP_BRANCH}
        INSTALL_COMMAND ""
        CMAKE_ARGS
            -DBUILD_SHARED_LIBS:BOOL=OFF
            -DSZIP_PACKAGE_EXT:STRING=${HDF_PACKAGE_EXT}
            -DSZIP_EXTERNALLY_CONFIGURED:BOOL=OFF
            -DCMAKE_BUILD_TYPE:STRING=${HDF_CFG_NAME}
            -DCMAKE_DEBUG_POSTFIX:STRING=${CMAKE_DEBUG_POSTFIX}
            -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
            -DCMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            -DCMAKE_LIBRARY_OUTPUT_DIRECTORY:PATH=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
            -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY:PATH=${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}
            -DCMAKE_PDB_OUTPUT_DIRECTORY:PATH=${CMAKE_PDB_OUTPUT_DIRECTORY}
            -DSZIP_ENABLE_ENCODING:BOOL=${encoding}
            -DHDF_USE_GNU_DIRS:STRING=${HDF5_USE_GNU_DIRS}
            -DCMAKE_OSX_ARCHITECTURES:STRING=${CMAKE_OSX_ARCHITECTURES}
            -DCMAKE_TOOLCHAIN_FILE:STRING=${CMAKE_TOOLCHAIN_FILE}
            -DPACKAGE_NAMESPACE=${HDF_PACKAGE_NAMESPACE}
    )
  elseif (${compress_type} MATCHES "TGZ")
    EXTERNALPROJECT_ADD (SZIP
        URL ${SZIP_URL}
        URL_MD5 ""
        INSTALL_COMMAND ""
        CMAKE_ARGS
            -DBUILD_SHARED_LIBS:BOOL=OFF
            -DSZIP_PACKAGE_EXT:STRING=${HDF_PACKAGE_EXT}
            -DSZIP_EXTERNALLY_CONFIGURED:BOOL=OFF
            -DCMAKE_BUILD_TYPE:STRING=${HDF_CFG_NAME}
            -DCMAKE_DEBUG_POSTFIX:STRING=${CMAKE_DEBUG_POSTFIX}
            -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
            -DCMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            -DCMAKE_LIBRARY_OUTPUT_DIRECTORY:PATH=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
            -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY:PATH=${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}
            -DCMAKE_PDB_OUTPUT_DIRECTORY:PATH=${CMAKE_PDB_OUTPUT_DIRECTORY}
            -DSZIP_ENABLE_ENCODING:BOOL=${encoding}
            -DHDF_USE_GNU_DIRS:STRING=${HDF5_USE_GNU_DIRS}
            -DCMAKE_OSX_ARCHITECTURES:STRING=${CMAKE_OSX_ARCHITECTURES}
            -DCMAKE_TOOLCHAIN_FILE:STRING=${CMAKE_TOOLCHAIN_FILE}
            -DPACKAGE_NAMESPACE=${HDF_PACKAGE_NAMESPACE}
    )
  endif ()
  externalproject_get_property (SZIP BINARY_DIR SOURCE_DIR)
#
##include (${BINARY_DIR}/${LIBAEC_PACKAGE_NAME}${HDF_PACKAGE_EXT}-targets.cmake)
# Create imported target szip-static
  add_library(${HDF_PACKAGE_NAMESPACE}szaec-static STATIC IMPORTED)
  HDF_IMPORT_SET_LIB_OPTIONS (${HDF_PACKAGE_NAMESPACE}szaec-static "szaec" STATIC "")
  add_dependencies (${HDF_PACKAGE_NAMESPACE}szaec-static SZIP)
  add_library(${HDF_PACKAGE_NAMESPACE}aec-static STATIC IMPORTED)
  HDF_IMPORT_SET_LIB_OPTIONS (${HDF_PACKAGE_NAMESPACE}aec-static "aec" STATIC "")
  add_dependencies (${HDF_PACKAGE_NAMESPACE}aec-static SZIP)
  set (SZIP_STATIC_LIBRARY "${HDF_PACKAGE_NAMESPACE}szaec-static;${HDF_PACKAGE_NAMESPACE}aec-static")
  set (SZIP_LIBRARIES ${SZIP_STATIC_LIBRARY})

  set (SZIP_INCLUDE_DIR_GEN "${BINARY_DIR}")
  set (SZIP_INCLUDE_DIR "${SOURCE_DIR}/include")
  set (SZIP_FOUND 1)
  set (SZIP_INCLUDE_DIRS ${SZIP_INCLUDE_DIR_GEN} ${SZIP_INCLUDE_DIR})
endmacro ()

#-------------------------------------------------------------------------------
macro (PACKAGE_SZIP_LIBRARY compress_type)
  add_custom_target (SZIP-GenHeader-Copy ALL
      COMMAND ${CMAKE_COMMAND} -E copy_if_different ${SZIP_INCLUDE_DIR_GEN}/aec_config.h ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/
      COMMENT "Copying ${SZIP_INCLUDE_DIR_GEN}/aec_config.h to ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/"
  )
  set (EXTERNAL_HEADER_LIST ${EXTERNAL_HEADER_LIST} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/aec_config.h)
  if (${compress_type} MATCHES "GIT" OR ${compress_type} MATCHES "TGZ")
    add_dependencies (SZIP-GenHeader-Copy SZIP)
  endif ()
endmacro ()

#-------------------------------------------------------------------------------
macro (EXTERNAL_ZLIB_LIBRARY compress_type)
  if (${compress_type} MATCHES "GIT")
    EXTERNALPROJECT_ADD (HDF5_ZLIB
        GIT_REPOSITORY ${ZLIB_URL}
        GIT_TAG ${ZLIB_BRANCH}
        INSTALL_COMMAND ""
        CMAKE_ARGS
            -DBUILD_SHARED_LIBS:BOOL=OFF
            -DZLIB_PACKAGE_EXT:STRING=${HDF_PACKAGE_EXT}
            -DZLIB_EXTERNALLY_CONFIGURED:BOOL=OFF
            -DCMAKE_BUILD_TYPE:STRING=${HDF_CFG_NAME}
            -DCMAKE_DEBUG_POSTFIX:STRING=${CMAKE_DEBUG_POSTFIX}
            -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
            -DCMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            -DCMAKE_LIBRARY_OUTPUT_DIRECTORY:PATH=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
            -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY:PATH=${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}
            -DCMAKE_PDB_OUTPUT_DIRECTORY:PATH=${CMAKE_PDB_OUTPUT_DIRECTORY}
            -DHDF_USE_GNU_DIRS:STRING=${HDF5_USE_GNU_DIRS}
            -DCMAKE_OSX_ARCHITECTURES:STRING=${CMAKE_OSX_ARCHITECTURES}
            -DCMAKE_TOOLCHAIN_FILE:STRING=${CMAKE_TOOLCHAIN_FILE}
            -DPACKAGE_NAMESPACE=${HDF_PACKAGE_NAMESPACE}
    )
  elseif (${compress_type} MATCHES "TGZ")
    EXTERNALPROJECT_ADD (HDF5_ZLIB
        URL ${ZLIB_URL}
        URL_MD5 ""
        INSTALL_COMMAND ""
        CMAKE_ARGS
            -DBUILD_SHARED_LIBS:BOOL=OFF
            -DZLIB_PACKAGE_EXT:STRING=${HDF_PACKAGE_EXT}
            -DZLIB_EXTERNALLY_CONFIGURED:BOOL=OFF
            -DCMAKE_BUILD_TYPE:STRING=${HDF_CFG_NAME}
            -DCMAKE_DEBUG_POSTFIX:STRING=${CMAKE_DEBUG_POSTFIX}
            -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
            -DCMAKE_RUNTIME_OUTPUT_DIRECTORY:PATH=${CMAKE_RUNTIME_OUTPUT_DIRECTORY}
            -DCMAKE_LIBRARY_OUTPUT_DIRECTORY:PATH=${CMAKE_LIBRARY_OUTPUT_DIRECTORY}
            -DCMAKE_ARCHIVE_OUTPUT_DIRECTORY:PATH=${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}
            -DCMAKE_PDB_OUTPUT_DIRECTORY:PATH=${CMAKE_PDB_OUTPUT_DIRECTORY}
            -DHDF_USE_GNU_DIRS:STRING=${HDF5_USE_GNU_DIRS}
            -DCMAKE_OSX_ARCHITECTURES:STRING=${CMAKE_OSX_ARCHITECTURES}
            -DCMAKE_TOOLCHAIN_FILE:STRING=${CMAKE_TOOLCHAIN_FILE}
            -DPACKAGE_NAMESPACE=${HDF_PACKAGE_NAMESPACE}
    )
  endif ()
  externalproject_get_property (HDF5_ZLIB BINARY_DIR SOURCE_DIR)

  if (NOT ZLIB_LIB_NAME)
    set (ZLIB_LIB_NAME "z")
  endif ()
##include (${BINARY_DIR}/${ZLIB_PACKAGE_NAME}${HDF_PACKAGE_EXT}-targets.cmake)
# Create imported target zlib-static
  add_library(${HDF_PACKAGE_NAMESPACE}zlib-static STATIC IMPORTED)
  HDF_IMPORT_SET_LIB_OPTIONS (${HDF_PACKAGE_NAMESPACE}zlib-static ${ZLIB_LIB_NAME} STATIC "")
  add_dependencies (${HDF_PACKAGE_NAMESPACE}zlib-static HDF5_ZLIB)
  set (ZLIB_STATIC_LIBRARY "${HDF_PACKAGE_NAMESPACE}zlib-static")
  set (ZLIB_LIBRARIES ${ZLIB_STATIC_LIBRARY})

  set (ZLIB_INCLUDE_DIR_GEN "${BINARY_DIR}")
  set (ZLIB_INCLUDE_DIR "${SOURCE_DIR}")
  set (ZLIB_FOUND 1)
  set (ZLIB_INCLUDE_DIRS ${ZLIB_INCLUDE_DIR_GEN} ${ZLIB_INCLUDE_DIR})
endmacro ()

#-------------------------------------------------------------------------------
macro (PACKAGE_ZLIB_LIBRARY compress_type)
  add_custom_target (ZLIB-GenHeader-Copy ALL
      COMMAND ${CMAKE_COMMAND} -E copy_if_different ${ZLIB_INCLUDE_DIR_GEN}/zconf.h ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/
      COMMENT "Copying ${ZLIB_INCLUDE_DIR_GEN}/zconf.h to ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/"
  )
  set (EXTERNAL_HEADER_LIST ${EXTERNAL_HEADER_LIST} ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/zconf.h)
  if (${compress_type} MATCHES "GIT" OR ${compress_type} MATCHES "TGZ")
    add_dependencies (ZLIB-GenHeader-Copy HDF5_ZLIB)
  endif ()
endmacro ()
