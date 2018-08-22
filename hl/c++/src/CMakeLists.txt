cmake_minimum_required (VERSION 3.10)
project (HDF5_HL_CPP_SRC CXX)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------

set (HDF5_HL_CPP_SOURCES ${HDF5_HL_CPP_SRC_SOURCE_DIR}/H5PacketTable.cpp)
set (HDF5_HL_CPP_HDRS ${HDF5_HL_CPP_SRC_SOURCE_DIR}/H5PacketTable.h)

add_library (${HDF5_HL_CPP_LIB_TARGET} STATIC ${HDF5_HL_CPP_SOURCES})
target_include_directories(${HDF5_HL_CPP_LIB_TARGET}
    PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
    INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)
TARGET_C_PROPERTIES (${HDF5_HL_CPP_LIB_TARGET} STATIC)
target_link_libraries (${HDF5_HL_CPP_LIB_TARGET} PUBLIC ${HDF5_HL_LIB_TARGET} ${HDF5_LIB_TARGET})
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_CPP_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_HL_CPP_LIB_TARGET} ${HDF5_HL_CPP_LIB_NAME} STATIC 0)
set_target_properties (${HDF5_HL_CPP_LIB_TARGET} PROPERTIES FOLDER libraries/hl)
set (install_targets ${HDF5_HL_CPP_LIB_TARGET})

if (BUILD_SHARED_LIBS)
  add_library (${HDF5_HL_CPP_LIBSH_TARGET} SHARED ${HDF5_HL_CPP_SOURCES})
  target_include_directories(${HDF5_HL_CPP_LIBSH_TARGET}
      PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_definitions(${HDF5_HL_CPP_LIBSH_TARGET}
      PUBLIC "H5_BUILT_AS_DYNAMIC_LIB"
  )
  TARGET_C_PROPERTIES (${HDF5_HL_CPP_LIBSH_TARGET} SHARED)
  target_link_libraries (${HDF5_HL_CPP_LIBSH_TARGET} PUBLIC ${HDF5_HL_LIBSH_TARGET} ${HDF5_LIBSH_TARGET})
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_CPP_LIBSH_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_HL_CPP_LIBSH_TARGET} ${HDF5_HL_CPP_LIB_NAME} SHARED "HL_CXX")
  set_target_properties (${HDF5_HL_CPP_LIBSH_TARGET} PROPERTIES FOLDER libraries/hl)
  set (install_targets ${install_targets} ${HDF5_HL_CPP_LIBSH_TARGET})
endif ()

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
install (
    FILES
        ${HDF5_HL_CPP_HDRS}
    DESTINATION
        ${HDF5_INSTALL_INCLUDE_DIR}
    COMPONENT
        hlcppheaders
)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
if (HDF5_EXPORTED_TARGETS)
  if (BUILD_SHARED_LIBS)
    INSTALL_TARGET_PDB (${HDF5_HL_CPP_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} hlcpplibraries)
  endif ()
  INSTALL_TARGET_PDB (${HDF5_HL_CPP_LIB_TARGET} ${HDF5_INSTALL_BIN_DIR} hlcpplibraries)

  install (
      TARGETS
          ${install_targets}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT hlcpplibraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT hlcpplibraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT hlcpplibraries
      FRAMEWORK DESTINATION ${HDF5_INSTALL_FWRK_DIR} COMPONENT hlcpplibraries
      INCLUDES DESTINATION include
  )
endif ()

#-----------------------------------------------------------------------------
# Create pkgconfig files
#-----------------------------------------------------------------------------
set (_PKG_CONFIG_PREFIX ${CMAKE_INSTALL_PREFIX})
set (_PKG_CONFIG_EXEC_PREFIX \${prefix})
set (_PKG_CONFIG_LIBDIR \${exec_prefix}/lib)
set (_PKG_CONFIG_INCLUDEDIR \${prefix}/include)
set (_PKG_CONFIG_LIBNAME "${HDF5_HL_CPP_LIB_CORENAME}")
set (_PKG_CONFIG_VERSION "${HDF5_PACKAGE_VERSION}")

set (_PKG_CONFIG_LIBS_PRIVATE)

set (_PKG_CONFIG_LIBS "${_PKG_CONFIG_LIBS} -l${HDF5_HL_CPP_LIB_CORENAME}")
if (BUILD_SHARED_LIBS)
  set (_PKG_CONFIG_SH_LIBS "${_PKG_CONFIG_SH_LIBS} -l${HDF5_HL_CPP_LIB_CORENAME}")
endif ()

set (_PKG_CONFIG_REQUIRES "${HDF5_HL_LIB_CORENAME}")
set (_PKG_CONFIG_REQUIRES_PRIVATE "${HDF5_HL_LIB_CORENAME}")

configure_file (
    ${HDF_RESOURCES_DIR}/libhdf5.pc.in
    ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_HL_CPP_LIB_CORENAME}-${HDF5_PACKAGE_VERSION}.pc
    @ONLY
)
install (
    FILES ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_HL_CPP_LIB_CORENAME}-${HDF5_PACKAGE_VERSION}.pc
    DESTINATION ${HDF5_INSTALL_LIB_DIR}/pkgconfig
    COMPONENT hlcpplibraries
)

if (NOT WIN32)
  set (_PKG_CONFIG_COMPILER ${CMAKE_CXX_COMPILER})
  configure_file (
      ${HDF_RESOURCES_DIR}/libh5cc.in
      ${HDF5_BINARY_DIR}/CMakeFiles/h5hlc++
      @ONLY
  )
  install (
      FILES ${HDF5_BINARY_DIR}/CMakeFiles/h5hlc++
      DESTINATION ${HDF5_INSTALL_BIN_DIR}
      PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
      COMPONENT hlcpplibraries
  )
endif ()

