PROJECT (HDF5_HL_CPP_SRC)

#-----------------------------------------------------------------------------
# Define Sources
#-----------------------------------------------------------------------------
INCLUDE_DIRECTORIES (${HDF5_HL_SRC_DIR}/src)
INCLUDE_DIRECTORIES (${HDF5_HL_CPP_SRC_SOURCE_DIR})

set (HDF5_HL_CPP_SRCS ${HDF5_HL_CPP_SRC_SOURCE_DIR}/H5PacketTable.cpp)
set (HDF5_HL_CPP_HDRS ${HDF5_HL_CPP_SRC_SOURCE_DIR}/H5PacketTable.h)

if(NOT BUILD_SHARED_LIBS)
add_library (${HDF5_HL_CPP_LIB_TARGET} STATIC ${HDF5_HL_CPP_SRCS})
TARGET_C_PROPERTIES (${HDF5_HL_CPP_LIB_TARGET} STATIC " " " ")
target_link_libraries (
    ${HDF5_HL_CPP_LIB_TARGET}
    ${HDF5_HL_LIB_TARGET}
    ${HDF5_LIB_TARGET}
)
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_CPP_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_HL_CPP_LIB_TARGET} ${HDF5_HL_CPP_LIB_NAME} STATIC)
set_target_properties (${HDF5_HL_CPP_LIB_TARGET} PROPERTIES
    FOLDER libraries/hl
    INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)
set (install_targets ${HDF5_HL_CPP_LIB_TARGET})
endif(NOT BUILD_SHARED_LIBS)

if (BUILD_SHARED_LIBS)
  add_library (${HDF5_HL_CPP_LIBSH_TARGET} SHARED ${HDF5_HL_CPP_SRCS})
  TARGET_C_PROPERTIES (${HDF5_HL_CPP_LIBSH_TARGET} SHARED " " " ")
  target_link_libraries (
      ${HDF5_HL_CPP_LIBSH_TARGET}
      ${HDF5_HL_LIBSH_TARGET}
      ${HDF5_LIBSH_TARGET}
  )
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_HL_CPP_LIBSH_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_HL_CPP_LIBSH_TARGET} ${HDF5_HL_CPP_LIB_NAME} SHARED ${HDF5_HL_CXX_PACKAGE_SOVERSION})
  set_target_properties (${HDF5_HL_CPP_LIBSH_TARGET} PROPERTIES
      FOLDER libraries/hl
      COMPILE_DEFINITIONS "H5_BUILT_AS_DYNAMIC_LIB"
      INTERFACE_INCLUDE_DIRECTORIES "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
      INTERFACE_COMPILE_DEFINITIONS H5_BUILT_AS_DYNAMIC_LIB=1
  )
  set (install_targets ${install_targets} ${HDF5_HL_CPP_LIBSH_TARGET})
endif (BUILD_SHARED_LIBS)

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
  endif (BUILD_SHARED_LIBS)

  install (
      TARGETS
          ${install_targets}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT hlcpplibraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT hlcpplibraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT hlcpplibraries
      FRAMEWORK DESTINATION ${HDF5_INSTALL_FWRK_DIR} COMPONENT hlcpplibraries
  )
endif (HDF5_EXPORTED_TARGETS)
