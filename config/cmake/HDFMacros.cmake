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
macro (SET_HDF_BUILD_TYPE)
  get_property (_isMultiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
  if (_isMultiConfig)
    # HDF_CFG_BUILD_TYPE is used in the Fortran install commands for the build location of the .mod files
    set (HDF_CFG_BUILD_TYPE \${CMAKE_INSTALL_CONFIG_NAME})
    if (CMAKE_BUILD_TYPE)
      # set the default to the specified command line define
      set (HDF_CFG_NAME ${CMAKE_BUILD_TYPE})
    else ()
      # set the default to the MultiConfig variable
      set (HDF_CFG_NAME "$<CONFIG>")
    endif ()
  else ()
    set (HDF_CFG_BUILD_TYPE ".")
    if (CMAKE_BUILD_TYPE)
      set (HDF_CFG_NAME ${CMAKE_BUILD_TYPE})
    else ()
      set (HDF_CFG_NAME "Release")
    endif ()
  endif ()
endmacro ()

#-------------------------------------------------------------------------------
macro (SET_GLOBAL_VARIABLE name value)
  set (${name} ${value} CACHE INTERNAL "Used to pass variables between directories" FORCE)
endmacro ()

#-------------------------------------------------------------------------------
macro (IDE_GENERATED_PROPERTIES SOURCE_PATH HEADERS SOURCES)
  #set (source_group_path "Source/AIM/${NAME}")
  string (REPLACE "/" "\\\\" source_group_path ${SOURCE_PATH})
  source_group (${source_group_path} FILES ${HEADERS} ${SOURCES})

  #-- The following is needed if we ever start to use OS X Frameworks but only
  #--  works on CMake 2.6 and greater
  #set_property (SOURCE ${HEADERS}
  #       PROPERTY MACOSX_PACKAGE_LOCATION Headers/${NAME}
  #)
endmacro ()

#-------------------------------------------------------------------------------
macro (IDE_SOURCE_PROPERTIES SOURCE_PATH HEADERS SOURCES)
  #  install (FILES ${HEADERS}
  #       DESTINATION include/R3D/${NAME}
  #       COMPONENT Headers
  #  )

  string (REPLACE "/" "\\\\" source_group_path ${SOURCE_PATH}  )
  source_group (${source_group_path} FILES ${HEADERS} ${SOURCES})

  #-- The following is needed if we ever start to use OS X Frameworks but only
  #--  works on CMake 2.6 and greater
  #set_property (SOURCE ${HEADERS}
  #       PROPERTY MACOSX_PACKAGE_LOCATION Headers/${NAME}
  #)
endmacro ()

#-------------------------------------------------------------------------------
macro (INSTALL_TARGET_PDB libtarget targetdestination targetcomponent)
  option (DISABLE_PDB_FILES "Do not install PDB files" OFF)
  mark_as_advanced (DISABLE_PDB_FILES)
  if (WIN32 AND MSVC AND NOT DISABLE_PDB_FILES)
    get_target_property (target_type ${libtarget} TYPE)
    if (${target_type} MATCHES "SHARED")
      set (targetfilename $<TARGET_PDB_FILE:${libtarget}>)
    else ()
      get_property (target_name TARGET ${libtarget} PROPERTY "$<IF:$<OR:$<CONFIG:Debug>,$<CONFIG:Developer>>,OUTPUT_NAME_DEBUG,OUTPUT_NAME_RELWITHDEBINFO>")
      set (targetfilename $<TARGET_FILE_DIR:${libtarget}>/${target_name}.pdb)
    endif ()
    install (
      FILES ${targetfilename}
      DESTINATION ${targetdestination}
      CONFIGURATIONS Debug RelWithDebInfo
      COMPONENT ${targetcomponent}
      OPTIONAL
    )
  endif ()
endmacro ()

#-------------------------------------------------------------------------------
macro (INSTALL_PROGRAM_PDB progtarget targetdestination targetcomponent)
  if (WIN32 AND MSVC)
    install (
      FILES $<TARGET_PDB_FILE:${progtarget}>
      DESTINATION ${targetdestination}
      CONFIGURATIONS Debug RelWithDebInfo
      COMPONENT ${targetcomponent}
      OPTIONAL
    )
  endif ()
endmacro ()

#-------------------------------------------------------------------------------
macro (HDF_SET_LIB_OPTIONS libtarget libname libtype)
  if (${libtype} MATCHES "SHARED")
    set (LIB_RELEASE_NAME "${libname}")
    set (LIB_DEBUG_NAME "${libname}${CMAKE_DEBUG_POSTFIX}")
  else ()
    if (WIN32 AND NOT MINGW)
      set (LIB_RELEASE_NAME "lib${libname}")
      set (LIB_DEBUG_NAME "lib${libname}${CMAKE_DEBUG_POSTFIX}")
    else ()
      set (LIB_RELEASE_NAME "${libname}")
      set (LIB_DEBUG_NAME "${libname}${CMAKE_DEBUG_POSTFIX}")
    endif ()
  endif ()

  set_target_properties (${libtarget} PROPERTIES
      OUTPUT_NAME                ${LIB_RELEASE_NAME}
#      OUTPUT_NAME_DEBUG          ${LIB_DEBUG_NAME}
      OUTPUT_NAME_DEVELOPER      ${LIB_DEBUG_NAME}
      OUTPUT_NAME_RELEASE        ${LIB_RELEASE_NAME}
      OUTPUT_NAME_MINSIZEREL     ${LIB_RELEASE_NAME}
      OUTPUT_NAME_RELWITHDEBINFO ${LIB_RELEASE_NAME}
  )

  if (${libtype} MATCHES "STATIC")
    if (WIN32)
      set_target_properties (${libtarget} PROPERTIES
          COMPILE_PDB_NAME_DEBUG          ${LIB_DEBUG_NAME}
          COMPILE_PDB_NAME_DEVELOPER      ${LIB_DEBUG_NAME}
          COMPILE_PDB_NAME_RELEASE        ${LIB_RELEASE_NAME}
          COMPILE_PDB_NAME_MINSIZEREL     ${LIB_RELEASE_NAME}
          COMPILE_PDB_NAME_RELWITHDEBINFO ${LIB_RELEASE_NAME}
          COMPILE_PDB_OUTPUT_DIRECTORY    "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}"
      )
    endif ()
  endif ()

  option (HDF5_MSVC_NAMING_CONVENTION "Use MSVC Naming conventions for Shared Libraries" OFF)
  if (HDF5_MSVC_NAMING_CONVENTION AND MINGW AND ${libtype} MATCHES "SHARED")
    set_target_properties (${libtarget} PROPERTIES
        IMPORT_SUFFIX ".lib"
        IMPORT_PREFIX ""
        PREFIX ""
    )
  endif ()
endmacro ()

#-------------------------------------------------------------------------------
macro (HDF_IMPORT_SET_LIB_OPTIONS libtarget libname libtype libversion)
  HDF_SET_LIB_OPTIONS (${libtarget} ${libname} ${libtype})

  if (${importtype} MATCHES "IMPORT")
    set (importprefix "${CMAKE_STATIC_LIBRARY_PREFIX}")
  endif ()
  if (${HDF_CFG_NAME} MATCHES "Debug" OR ${HDF_CFG_NAME} MATCHES "Developer")
    set (IMPORT_LIB_NAME ${LIB_DEBUG_NAME})
  else ()
    set (IMPORT_LIB_NAME ${LIB_RELEASE_NAME})
  endif ()

  if (${libtype} MATCHES "SHARED")
    if (WIN32)
      if (MINGW)
        set_target_properties (${libtarget} PROPERTIES
            IMPORTED_IMPLIB "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${IMPORT_LIB_NAME}.lib"
            IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${IMPORT_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}"
        )
      else ()
        set_target_properties (${libtarget} PROPERTIES
            IMPORTED_IMPLIB "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${HDF_CFG_NAME}/${CMAKE_IMPORT_LIBRARY_PREFIX}${IMPORT_LIB_NAME}${CMAKE_IMPORT_LIBRARY_SUFFIX}"
            IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${HDF_CFG_NAME}/${CMAKE_IMPORT_LIBRARY_PREFIX}${IMPORT_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}"
        )
      endif ()
    else ()
      if (MINGW)
        set_target_properties (${libtarget} PROPERTIES
            IMPORTED_IMPLIB "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${IMPORT_LIB_NAME}.lib"
            IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${IMPORT_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}"
        )
      elseif (CYGWIN)
        set_target_properties (${libtarget} PROPERTIES
            IMPORTED_IMPLIB "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${CMAKE_IMPORT_LIBRARY_PREFIX}${IMPORT_LIB_NAME}${CMAKE_IMPORT_LIBRARY_SUFFIX}"
            IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${CMAKE_IMPORT_LIBRARY_PREFIX}${IMPORT_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}"
        )
      else ()
        set_target_properties (${libtarget} PROPERTIES
            IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${CMAKE_SHARED_LIBRARY_PREFIX}${IMPORT_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}"
            IMPORTED_SONAME "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${CMAKE_SHARED_LIBRARY_PREFIX}${IMPORT_LIB_NAME}${CMAKE_SHARED_LIBRARY_SUFFIX}.${libversion}"
            SOVERSION "${libversion}"
        )
      endif ()
    endif ()
  else ()
    if (WIN32 AND NOT MINGW)
      set_target_properties (${libtarget} PROPERTIES
          IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${HDF_CFG_NAME}/${IMPORT_LIB_NAME}${CMAKE_STATIC_LIBRARY_SUFFIX}"
          IMPORTED_LINK_INTERFACE_LANGUAGES "C"
      )
    else ()
      set_target_properties (${libtarget} PROPERTIES
          IMPORTED_LOCATION "${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/${CMAKE_STATIC_LIBRARY_PREFIX}${IMPORT_LIB_NAME}${CMAKE_STATIC_LIBRARY_SUFFIX}"
          IMPORTED_LINK_INTERFACE_LANGUAGES "C"
      )
    endif ()
  endif ()
endmacro ()

#-------------------------------------------------------------------------------
macro (TARGET_C_PROPERTIES wintarget libtype)
  target_compile_options(${wintarget} PRIVATE
      "$<$<C_COMPILER_ID:MSVC>:${WIN_COMPILE_FLAGS}>"
      "$<$<CXX_COMPILER_ID:MSVC>:${WIN_COMPILE_FLAGS}>"
  )
  if(MSVC)
    set_property(TARGET ${wintarget} APPEND PROPERTY LINK_FLAGS "${WIN_LINK_FLAGS}")
  endif()
endmacro ()

#-----------------------------------------------------------------------------
# Configure the README.md file for the binary package
#-----------------------------------------------------------------------------
macro (HDF_README_PROPERTIES target_fortran)
  set (BINARY_SYSTEM_NAME ${CMAKE_SYSTEM_NAME})
  set (BINARY_PLATFORM "${CMAKE_SYSTEM_NAME}")
  if (WIN32)
    set (BINARY_EXAMPLE_ENDING "zip")
    set (BINARY_INSTALL_ENDING "msi")
    if (CMAKE_CL_64)
      set (BINARY_SYSTEM_NAME "win64")
    else ()
      set (BINARY_SYSTEM_NAME "win32")
    endif ()
    if (${CMAKE_SYSTEM_VERSION} MATCHES "6.1")
      set (BINARY_PLATFORM "${BINARY_PLATFORM} 7")
    elseif (${CMAKE_SYSTEM_VERSION} MATCHES "6.2")
      set (BINARY_PLATFORM "${BINARY_PLATFORM} 8")
    elseif (${CMAKE_SYSTEM_VERSION} MATCHES "6.3")
      set (BINARY_PLATFORM "${BINARY_PLATFORM} 10")
    endif ()
    if (CMAKE_C_COMPILER_ID MATCHES "Intel")
      set (BINARY_PLATFORM "${BINARY_PLATFORM} Intel")
      if (${CMAKE_C_COMPILER_VERSION} MATCHES "^17.*")
        set (BINARY_PLATFORM "${BINARY_PLATFORM}, using Intel 17")
      elseif (${CMAKE_C_COMPILER_VERSION} MATCHES "^18.*")
        set (BINARY_PLATFORM "${BINARY_PLATFORM}, using Intel 18")
      elseif (${CMAKE_C_COMPILER_VERSION} MATCHES "^19.*")
        set (BINARY_PLATFORM "${BINARY_PLATFORM}, using Intel 19")
      else ()
        set (BINARY_PLATFORM "${BINARY_PLATFORM}, using Intel ${CMAKE_C_COMPILER_VERSION}")
      endif ()
    elseif (CMAKE_C_COMPILER_ID MATCHES "MSVC")
      set (BINARY_PLATFORM "${BINARY_PLATFORM} ${MSVC_C_ARCHITECTURE_ID}")
      if (${CMAKE_C_COMPILER_VERSION} MATCHES "^16.*")
        set (BINARY_PLATFORM "${BINARY_PLATFORM}, using VISUAL STUDIO 2010")
      elseif (${CMAKE_C_COMPILER_VERSION} MATCHES "^15.*")
        set (BINARY_PLATFORM "${BINARY_PLATFORM}, using VISUAL STUDIO 2008")
      elseif (${CMAKE_C_COMPILER_VERSION} MATCHES "^17.*")
        set (BINARY_PLATFORM "${BINARY_PLATFORM}, using VISUAL STUDIO 2012")
      elseif (${CMAKE_C_COMPILER_VERSION} MATCHES "^18.*")
        set (BINARY_PLATFORM "${BINARY_PLATFORM}, using VISUAL STUDIO 2013")
      elseif (${CMAKE_C_COMPILER_VERSION} MATCHES "^19.*")
        if (${CMAKE_C_COMPILER_VERSION} MATCHES "^19.0.*")
          set (BINARY_PLATFORM "${BINARY_PLATFORM}, using VISUAL STUDIO 2015")
        elseif (${CMAKE_C_COMPILER_VERSION} MATCHES "^19.1.*")
          set (BINARY_PLATFORM "${BINARY_PLATFORM}, using VISUAL STUDIO 2017")
        elseif (${CMAKE_C_COMPILER_VERSION} MATCHES "^19.2.*")
          set (BINARY_PLATFORM "${BINARY_PLATFORM}, using VISUAL STUDIO 2019")
        elseif (${CMAKE_C_COMPILER_VERSION} MATCHES "^19.3.*")
          set (BINARY_PLATFORM "${BINARY_PLATFORM}, using VISUAL STUDIO 2022")
        else ()
          set (BINARY_PLATFORM "${BINARY_PLATFORM}, using VISUAL STUDIO ???")
        endif ()
      else ()
        set (BINARY_PLATFORM "${BINARY_PLATFORM}, using VISUAL STUDIO ${CMAKE_C_COMPILER_VERSION}")
      endif ()
    endif ()
  elseif (APPLE)
    set (BINARY_EXAMPLE_ENDING "tar.gz")
    set (BINARY_INSTALL_ENDING "sh") # if packaging changes - use dmg
    set (BINARY_PLATFORM "${BINARY_PLATFORM} ${CMAKE_SYSTEM_VERSION} ${CMAKE_SYSTEM_PROCESSOR}")
    set (BINARY_PLATFORM "${BINARY_PLATFORM}, using ${CMAKE_C_COMPILER_ID} C ${CMAKE_C_COMPILER_VERSION}")
  else ()
    set (BINARY_EXAMPLE_ENDING "tar.gz")
    set (BINARY_INSTALL_ENDING "sh")
    set (BINARY_PLATFORM "${BINARY_PLATFORM} ${CMAKE_SYSTEM_VERSION} ${CMAKE_SYSTEM_PROCESSOR}")
    set (BINARY_PLATFORM "${BINARY_PLATFORM}, using ${CMAKE_C_COMPILER_ID} C ${CMAKE_C_COMPILER_VERSION}")
  endif ()

  if (target_fortran)
    set (BINARY_PLATFORM "${BINARY_PLATFORM} / ${CMAKE_Fortran_COMPILER_ID} Fortran")
  endif ()

  if (ONLY_SHARED_LIBS)
    set (LIB_TYPE "Shared")
  elseif (BUILD_SHARED_LIBS)
    set (LIB_TYPE "Static and Shared")
  else ()
    set (LIB_TYPE "Static")
  endif ()

  configure_file (
      ${HDF_RESOURCES_DIR}/README.md.cmake.in
      ${CMAKE_BINARY_DIR}/README.md @ONLY
  )
endmacro ()

macro (HDFTEST_COPY_FILE src dest target)
    add_custom_command(
        OUTPUT  "${dest}"
        COMMAND "${CMAKE_COMMAND}"
        ARGS     -E copy_if_different "${src}" "${dest}"
        DEPENDS "${src}"
    )
    list (APPEND ${target}_list "${dest}")
endmacro ()

macro (HDF_DIR_PATHS package_prefix)
  option (HDF5_USE_GNU_DIRS "TRUE to use GNU Coding Standard install directory variables, FALSE to use historical settings" FALSE)
  if (HDF5_USE_GNU_DIRS)
    include(GNUInstallDirs)
    if (NOT ${package_prefix}_INSTALL_BIN_DIR)
      set (${package_prefix}_INSTALL_BIN_DIR ${CMAKE_INSTALL_BINDIR})
    endif ()
    if (NOT ${package_prefix}_INSTALL_LIB_DIR)
      set (${package_prefix}_INSTALL_LIB_DIR ${CMAKE_INSTALL_LIBDIR})
    endif ()
    if (NOT ${package_prefix}_INSTALL_JAR_DIR)
      set (${package_prefix}_INSTALL_JAR_DIR ${CMAKE_INSTALL_LIBDIR})
    endif ()
    if (NOT ${package_prefix}_INSTALL_INCLUDE_DIR)
      set (${package_prefix}_INSTALL_INCLUDE_DIR ${CMAKE_INSTALL_INCLUDEDIR})
    endif ()
    if (NOT ${package_prefix}_INSTALL_MODULE_DIR)
      set (${package_prefix}_INSTALL_MODULE_DIR ${CMAKE_INSTALL_INCLUDEDIR}/mod)
    endif ()
    if (NOT ${package_prefix}_INSTALL_DATA_DIR)
      set (${package_prefix}_INSTALL_DATA_DIR ${CMAKE_INSTALL_DATADIR})
    endif ()
    if (NOT ${package_prefix}_INSTALL_CMAKE_DIR)
      set (${package_prefix}_INSTALL_CMAKE_DIR ${CMAKE_INSTALL_LIBDIR}/cmake)
    endif ()
    if (NOT ${package_prefix}_INSTALL_DOC_DIR)
      set (${package_prefix}_INSTALL_DOC_DIR ${CMAKE_INSTALL_DOCDIR})
    endif ()
    message(STATUS "GNU: ${${package_prefix}_INSTALL_DOC_DIR}")
  endif ()

  if (APPLE)
    option (${package_prefix}_BUILD_FRAMEWORKS "TRUE to build as frameworks libraries, FALSE to build according to BUILD_SHARED_LIBS" FALSE)
  endif ()

  if (NOT ${package_prefix}_INSTALL_BIN_DIR)
    set (${package_prefix}_INSTALL_BIN_DIR bin)
  endif ()
  if (NOT ${package_prefix}_INSTALL_LIB_DIR)
    if (APPLE)
      if (${package_prefix}_BUILD_FRAMEWORKS)
        set (${package_prefix}_INSTALL_JAR_DIR ../Java)
      else ()
        set (${package_prefix}_INSTALL_JAR_DIR lib)
      endif ()
      set (${package_prefix}_INSTALL_FMWK_DIR ${CMAKE_INSTALL_FRAMEWORK_PREFIX})
    else ()
      set (${package_prefix}_INSTALL_JAR_DIR lib)
    endif ()
    set (${package_prefix}_INSTALL_LIB_DIR lib)
  endif ()
  if (NOT ${package_prefix}_INSTALL_INCLUDE_DIR)
    set (${package_prefix}_INSTALL_INCLUDE_DIR include)
  endif ()
  if (NOT ${package_prefix}_INSTALL_MODULE_DIR)
    set (${package_prefix}_INSTALL_MODULE_DIR mod)
  endif ()
  if (NOT ${package_prefix}_INSTALL_DATA_DIR)
    if (NOT MSVC)
      if (APPLE)
        if (${package_prefix}_BUILD_FRAMEWORKS)
          set (${package_prefix}_INSTALL_EXTRA_DIR ../SharedSupport)
        else ()
          set (${package_prefix}_INSTALL_EXTRA_DIR share)
        endif ()
        set (${package_prefix}_INSTALL_FWRK_DIR ${CMAKE_INSTALL_FRAMEWORK_PREFIX})
      endif ()
      set (${package_prefix}_INSTALL_DATA_DIR share)
    else ()
      set (${package_prefix}_INSTALL_DATA_DIR ".")
    endif ()
  endif ()
  if (NOT ${package_prefix}_INSTALL_CMAKE_DIR)
    set (${package_prefix}_INSTALL_CMAKE_DIR cmake)
  endif ()
  if (NOT ${package_prefix}_INSTALL_DOC_DIR)
    set (${package_prefix}_INSTALL_DOC_DIR ${${package_prefix}_INSTALL_DATA_DIR})
  endif ()
  message(STATUS "Final: ${${package_prefix}_INSTALL_DOC_DIR}")

  # Always use full RPATH, i.e. don't skip the full RPATH for the build tree
  set (CMAKE_SKIP_BUILD_RPATH  FALSE)
  # when building, don't use the install RPATH already
  # (but later on when installing)
  set (CMAKE_INSTALL_RPATH_USE_LINK_PATH  FALSE)
  # add the automatically determined parts of the RPATH
  # which point to directories outside the build tree to the install RPATH
  set (CMAKE_BUILD_WITH_INSTALL_RPATH ON)
  if (APPLE)
    set (CMAKE_INSTALL_NAME_DIR "@rpath")
    set (CMAKE_INSTALL_RPATH
        "@executable_path/../${${package_prefix}_INSTALL_LIB_DIR}"
        "@executable_path/"
        "@loader_path/../${${package_prefix}_INSTALL_LIB_DIR}"
        "@loader_path/"
    )
  else ()
    set (CMAKE_INSTALL_RPATH "\$ORIGIN/../${${package_prefix}_INSTALL_LIB_DIR}:\$ORIGIN/")
  endif ()

  if (DEFINED ADDITIONAL_CMAKE_PREFIX_PATH AND EXISTS "${ADDITIONAL_CMAKE_PREFIX_PATH}")
    set (CMAKE_PREFIX_PATH ${ADDITIONAL_CMAKE_PREFIX_PATH} ${CMAKE_PREFIX_PATH})
  endif ()

  #set the default debug suffix for all library targets
  if(NOT CMAKE_DEBUG_POSTFIX)
    if (WIN32)
      set (CMAKE_DEBUG_POSTFIX "_D")
    else ()
      set (CMAKE_DEBUG_POSTFIX "_debug")
    endif ()
  endif ()

  SET_HDF_BUILD_TYPE()

#-----------------------------------------------------------------------------
# Setup output Directories
#-----------------------------------------------------------------------------
  if (NOT ${package_prefix}_EXTERNALLY_CONFIGURED)
    set (CMAKE_RUNTIME_OUTPUT_DIRECTORY
        ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all Executables."
    )
    set (CMAKE_LIBRARY_OUTPUT_DIRECTORY
        ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all Libraries"
    )
    set (CMAKE_ARCHIVE_OUTPUT_DIRECTORY
        ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all static libraries."
    )
    set (CMAKE_Fortran_MODULE_DIRECTORY
        ${PROJECT_BINARY_DIR}/mod CACHE PATH "Single Directory for all fortran modules."
    )
    get_property(_isMultiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
    if(_isMultiConfig)
      set (CMAKE_TEST_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${HDF_CFG_NAME})
      set (CMAKE_PDB_OUTPUT_DIRECTORY
          ${PROJECT_BINARY_DIR}/bin CACHE PATH "Single Directory for all pdb files."
      )
    else ()
      set (CMAKE_TEST_OUTPUT_DIRECTORY ${CMAKE_RUNTIME_OUTPUT_DIRECTORY})
    endif ()
  else ()
    # if we are externally configured, but the project uses old cmake scripts
    # this may not be set and utilities like H5detect will fail
    if (NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)
      set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${EXECUTABLE_OUTPUT_PATH})
    endif ()
  endif ()

  if (NOT ${package_prefix}_EXTERNALLY_CONFIGURED AND CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
    if (CMAKE_HOST_UNIX)
      set (CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}/HDF_Group/${HDF5_PACKAGE_NAME}/${HDF5_PACKAGE_VERSION}"
        CACHE PATH "Install path prefix, prepended onto install directories." FORCE)
    else ()
      GetDefaultWindowsPrefixBase(CMAKE_GENERIC_PROGRAM_FILES)
      set (CMAKE_INSTALL_PREFIX
        "${CMAKE_GENERIC_PROGRAM_FILES}/HDF_Group/${HDF5_PACKAGE_NAME}/${HDF5_PACKAGE_VERSION}"
        CACHE PATH "Install path prefix, prepended onto install directories." FORCE)
      set (CMAKE_GENERIC_PROGRAM_FILES)
    endif ()
    set (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT 0 CACHE PATH "" FORCE)
  endif ()

  include (FetchContent)
endmacro ()

macro (ADD_H5_FLAGS h5_flag_var infile)
  file (STRINGS ${infile} TEST_FLAG_STREAM)
  #message (TRACE "TEST_FLAG_STREAM=${TEST_FLAG_STREAM}")
  list (LENGTH TEST_FLAG_STREAM len_flag)
  if (len_flag GREATER 0)
    math (EXPR _FP_LEN "${len_flag} - 1")
    foreach (line RANGE 0 ${_FP_LEN})
      list (GET TEST_FLAG_STREAM ${line} str_flag)
      string (REGEX REPLACE "^#.*" "" str_flag "${str_flag}")
      #message (TRACE "str_flag=${str_flag}")
      if (NOT HDF5_ENABLE_WARNINGS_AS_ERRORS)
        string (REGEX REPLACE "-Werror=" "-W" str_flag "${str_flag}")
      endif ()
      if (str_flag)
        list (APPEND ${h5_flag_var} "${str_flag}")
      endif ()
    endforeach ()
  endif ()
  #message (TRACE "h5_flag_var=${${h5_flag_var}}")
endmacro ()

