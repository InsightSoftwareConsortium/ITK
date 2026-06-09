# vcl/CMakeLists.txt
#
# vcl is header-only: it provides the configured vcl_compiler.h /
# vcl_where_root_dir.h and the vcl_msvc_warnings.h shim. The "vcl" INTERFACE
# target carries the vcl and core include roots (source and generated) so any
# target that links it resolves <vcl_compiler.h>, <vxl_config.h> and <vnl/...>
# without a global include_directories.

project( vcl )

# If VXL_INSTALL_INCLUDE_DIR is the default value
if("${VXL_INSTALL_INCLUDE_DIR}" STREQUAL "include/vxl")
  set(_config_destination ${VXL_INSTALL_INCLUDE_DIR}/vcl)
else()
  set(_config_destination ${VXL_INSTALL_INCLUDE_DIR})
endif()
vxl_configure_file(${CMAKE_CURRENT_LIST_DIR}/vcl_compiler.h.in ${PROJECT_BINARY_DIR}/vcl_compiler.h ${_config_destination})
vxl_configure_file(${CMAKE_CURRENT_LIST_DIR}/vcl_where_root_dir.h.in ${PROJECT_BINARY_DIR}/vcl_where_root_dir.h  ${_config_destination})

install(FILES vcl_msvc_warnings.h
    DESTINATION ${_config_destination}
    PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ
    COMPONENT Development )

# vcl carries the vcl + core include roots. By default it is a real static
# archive holding one dummy symbol (vcl.cxx) so downstream link lines that still
# reference -lvcl resolve; with ITK_FUTURE_LEGACY_REMOVE it becomes a pure
# header-only INTERFACE target. Either way the include paths propagate.
if(ITK_FUTURE_LEGACY_REMOVE)
  add_library(vcl INTERFACE)
  set(_vcl_scope INTERFACE)
else()
  add_library(vcl STATIC vcl.cxx)
  set(_vcl_scope PUBLIC)
endif()
target_include_directories(vcl ${_vcl_scope}
    "$<BUILD_INTERFACE:${VCL_INCLUDE_DIR}>"
    "$<BUILD_INTERFACE:${VCL_BINARY_INCLUDE_DIR}>"
    "$<BUILD_INTERFACE:${VXLCORE_INCLUDE_DIR}>"
    "$<BUILD_INTERFACE:${VXLCORE_BINARY_INCLUDE_DIR}>")
if("${VXL_INSTALL_INCLUDE_DIR}" STREQUAL "include/vxl")
  target_include_directories(vcl ${_vcl_scope}
      "$<INSTALL_INTERFACE:${VXL_INSTALL_INCLUDE_DIR}/vcl>"
      "$<INSTALL_INTERFACE:${VXL_INSTALL_INCLUDE_DIR}/core>")
else()
  target_include_directories(vcl ${_vcl_scope}
      "$<INSTALL_INTERFACE:${VXL_INSTALL_INCLUDE_DIR}>")
endif()
# Join the install export set so exported consumers do not trip
# "target not in any export set" when this tree is embedded in ITK.
install(TARGETS vcl EXPORT ${VXL_INSTALL_EXPORT_NAME}
    ARCHIVE DESTINATION ${VXL_INSTALL_LIBRARY_DIR}
    LIBRARY DESTINATION ${VXL_INSTALL_LIBRARY_DIR}
    RUNTIME DESTINATION ${VXL_INSTALL_RUNTIME_DIR})

# When embedded in an ITK module build, ITK only auto-exports its own module
# libraries to the build-tree targets file, not vxl's vcl. Add vcl there too so
# build-tree consumers resolve it instead of emitting a dangling -lvcl.
if(DEFINED itk-module AND DEFINED ${itk-module}-targets-build)
  export(TARGETS vcl APPEND FILE ${${itk-module}-targets-build})
endif()

if( BUILD_TESTING)
  add_subdirectory(tests)
endif()
