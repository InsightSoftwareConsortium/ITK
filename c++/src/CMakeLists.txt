cmake_minimum_required (VERSION 3.10)
project (HDF5_CPP_SRC CXX)

#-----------------------------------------------------------------------------
# Generate configure file
#-----------------------------------------------------------------------------
configure_file (${HDF_RESOURCES_DIR}/H5cxx_config.h.in
                ${HDF5_BINARY_DIR}/H5cxx_pubconf.h
)

#-----------------------------------------------------------------------------
# Define cpp Library
#-----------------------------------------------------------------------------
set (CPP_SOURCES
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5AbstractDs.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5ArrayType.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5AtomType.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Attribute.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5CommonFG.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5CompType.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5DataSet.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5DataSpace.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5DataType.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5DaccProp.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5DcreatProp.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5DxferProp.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5EnumType.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Exception.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5FaccProp.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5FcreatProp.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5File.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5FloatType.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Group.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5IdComponent.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5IntType.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5LaccProp.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5LcreatProp.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Library.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Location.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Object.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5OcreatProp.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5PredType.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5PropList.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5StrType.cpp
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5VarLenType.cpp
)

set (CPP_HDRS
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5AbstractDs.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Alltypes.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5ArrayType.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5AtomType.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Attribute.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Classes.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5CommonFG.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5CompType.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Cpp.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5CppDoc.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5DataSet.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5DataSpace.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5DataType.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5DaccProp.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5DcreatProp.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5DxferProp.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5EnumType.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Exception.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5FaccProp.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5FcreatProp.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5File.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5FloatType.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Group.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5IdComponent.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Include.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5IntType.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5LaccProp.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5LcreatProp.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Library.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Location.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5Object.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5OcreatProp.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5PredType.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5PropList.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5StrType.h
    ${HDF5_CPP_SRC_SOURCE_DIR}/H5VarLenType.h
)

add_library (${HDF5_CPP_LIB_TARGET} STATIC ${CPP_SOURCES} ${CPP_HDRS})
target_include_directories(${HDF5_CPP_LIB_TARGET}
    PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
    INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
)
target_compile_definitions(${HDF5_CPP_LIB_TARGET}
    PRIVATE $<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPICH_SKIP_MPICXX;MPICH_IGNORE_CXX_SEEK># Parallel/MPI, prevent spurious cpp/cxx warnings
)
TARGET_C_PROPERTIES (${HDF5_CPP_LIB_TARGET} STATIC)
target_link_libraries (${HDF5_CPP_LIB_TARGET} PUBLIC ${HDF5_LIB_TARGET})
set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_CPP_LIB_TARGET}")
H5_SET_LIB_OPTIONS (${HDF5_CPP_LIB_TARGET} ${HDF5_CPP_LIB_NAME} STATIC 0)
set_target_properties (${HDF5_CPP_LIB_TARGET} PROPERTIES FOLDER libraries/cpp)
set (install_targets ${HDF5_CPP_LIB_TARGET})

if (BUILD_SHARED_LIBS)
  add_library (${HDF5_CPP_LIBSH_TARGET} SHARED ${CPP_SOURCES} ${CPP_HDRS})
  target_include_directories(${HDF5_CPP_LIBSH_TARGET}
      PRIVATE "${HDF5_SRC_DIR};${HDF5_BINARY_DIR};$<$<BOOL:${HDF5_ENABLE_PARALLEL}>:${MPI_C_INCLUDE_DIRS}>"
      INTERFACE "$<INSTALL_INTERFACE:$<INSTALL_PREFIX>/include>"
  )
  target_compile_definitions(${HDF5_CPP_LIBSH_TARGET}
      PUBLIC "H5_BUILT_AS_DYNAMIC_LIB"
      PRIVATE $<$<BOOL:${HDF5_ENABLE_PARALLEL}>:MPICH_SKIP_MPICXX;MPICH_IGNORE_CXX_SEEK># Parallel/MPI, prevent spurious cpp/cxx warnings
  )
  TARGET_C_PROPERTIES (${HDF5_CPP_LIBSH_TARGET} SHARED)
  target_link_libraries (${HDF5_CPP_LIBSH_TARGET} PUBLIC ${HDF5_LIBSH_TARGET})
  set_global_variable (HDF5_LIBRARIES_TO_EXPORT "${HDF5_LIBRARIES_TO_EXPORT};${HDF5_CPP_LIBSH_TARGET}")
  H5_SET_LIB_OPTIONS (${HDF5_CPP_LIBSH_TARGET} ${HDF5_CPP_LIB_NAME} SHARED "CXX")
  set_target_properties (${HDF5_CPP_LIBSH_TARGET} PROPERTIES FOLDER libraries/cpp)
  set (install_targets ${install_targets} ${HDF5_CPP_LIBSH_TARGET})
endif ()

#-----------------------------------------------------------------------------
# Add file(s) to CMake Install
#-----------------------------------------------------------------------------
install (
    FILES
        ${CPP_HDRS}
    DESTINATION
        ${HDF5_INSTALL_INCLUDE_DIR}
    COMPONENT
        cppheaders
)

#-----------------------------------------------------------------------------
# Add Target(s) to CMake Install for import into other projects
#-----------------------------------------------------------------------------
if (HDF5_EXPORTED_TARGETS)
  if (BUILD_SHARED_LIBS)
    INSTALL_TARGET_PDB (${HDF5_CPP_LIBSH_TARGET} ${HDF5_INSTALL_BIN_DIR} cpplibraries)
  endif ()
  INSTALL_TARGET_PDB (${HDF5_CPP_LIB_TARGET} ${HDF5_INSTALL_BIN_DIR} cpplibraries)

  install (
      TARGETS
          ${install_targets}
      EXPORT
          ${HDF5_EXPORTED_TARGETS}
      LIBRARY DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT cpplibraries
      ARCHIVE DESTINATION ${HDF5_INSTALL_LIB_DIR} COMPONENT cpplibraries
      RUNTIME DESTINATION ${HDF5_INSTALL_BIN_DIR} COMPONENT cpplibraries
      FRAMEWORK DESTINATION ${HDF5_INSTALL_FWRK_DIR} COMPONENT cpplibraries
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
set (_PKG_CONFIG_LIBNAME "${HDF5_CPP_LIB_CORENAME}")
set (_PKG_CONFIG_VERSION "${HDF5_PACKAGE_VERSION}")

set (_PKG_CONFIG_LIBS_PRIVATE)

set (_PKG_CONFIG_LIBS "${_PKG_CONFIG_LIBS} -l${HDF5_CPP_LIB_CORENAME}")
if (BUILD_SHARED_LIBS)
  set (_PKG_CONFIG_SH_LIBS "${_PKG_CONFIG_SH_LIBS} -l${HDF5_CPP_LIB_CORENAME}")
endif ()

set (_PKG_CONFIG_REQUIRES "${HDF5_LIB_CORENAME}")
set (_PKG_CONFIG_REQUIRES_PRIVATE "${HDF5_LIB_CORENAME}")

configure_file (
    ${HDF_RESOURCES_DIR}/libhdf5.pc.in
    ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_CPP_LIB_CORENAME}-${HDF5_PACKAGE_VERSION}.pc
    @ONLY
)
install (
    FILES ${HDF5_BINARY_DIR}/CMakeFiles/${HDF5_CPP_LIB_CORENAME}-${HDF5_PACKAGE_VERSION}.pc
    DESTINATION ${HDF5_INSTALL_LIB_DIR}/pkgconfig
    COMPONENT cpplibraries
)

if (NOT WIN32)
  set (_PKG_CONFIG_COMPILER ${CMAKE_CXX_COMPILER})
  configure_file (
      ${HDF_RESOURCES_DIR}/libh5cc.in
      ${HDF5_BINARY_DIR}/CMakeFiles/h5c++
      @ONLY
  )
  install (
      FILES ${HDF5_BINARY_DIR}/CMakeFiles/h5c++
      DESTINATION ${HDF5_INSTALL_BIN_DIR}
      PERMISSIONS OWNER_READ OWNER_WRITE OWNER_EXECUTE GROUP_READ GROUP_EXECUTE WORLD_READ WORLD_EXECUTE
      COMPONENT cpplibraries
  )
endif ()

