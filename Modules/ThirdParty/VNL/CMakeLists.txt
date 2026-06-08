project(ITKVNL)
set(ITKVNL_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_VXL "Use an outside build of VXL." OFF)
mark_as_advanced(ITK_USE_SYSTEM_VXL)

if(ITK_USE_SYSTEM_VXL)
  find_package(VXL 4.0.0 REQUIRED)
  set(
    ITKVNL_SYSTEM_INCLUDE_DIRS
    ${VXL_NETLIB_INCLUDE_DIR}
    ${VXL_VCL_INCLUDE_DIRS}
    ${VXL_VCL_INCLUDE_DIR}
    ${VXL_CORE_INCLUDE_DIRS}
    ${VXL_CORE_INCLUDE_DIR}
  )
  set(
    ITKVNL_LIBRARIES
    vnl_algo
    vnl
    v3p_netlib
    netlib
  )
  set(ITKVNL_SYSTEM_LIBRARY_DIRS "${VXL_LIBRARY_DIR}")
  set(ITKVNL_NO_SRC 1)
else()
  # The ITK-stripped vxl is numerics-only: the non-numerics core groups, the
  # v3p image-I/O backends, DCMTK, the GUI, historical-compatibility modes,
  # large-file support, object libraries, the standalone export machinery and
  # the configurable library prefix have all been removed upstream, so the
  # long list of forced VXL_* options they gated is gone. Only the handful of
  # options that still exist in that tree need forcing here.
  set(VNL_CONFIG_CHECK_BOUNDS OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_BUILD_CORE_NUMERICS ON CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_RUN_FAILING_TESTS OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_UPDATE_CONFIGURATION OFF CACHE BOOL "VXL configuration for ITK" FORCE)

  set(VXL_LIBRARY_PROPERTIES ${ITK_LIBRARY_PROPERTIES})

  set(
    ITKVNL_INCLUDE_DIRS
    ${ITKVNL_SOURCE_DIR}/src/vxl/v3p/netlib
    ${ITKVNL_SOURCE_DIR}/src/vxl/vcl
    ${ITKVNL_SOURCE_DIR}/src/vxl/core
    ${ITKVNL_BINARY_DIR}/src/vxl/v3p/netlib
    ${ITKVNL_BINARY_DIR}/src/vxl/vcl
    ${ITKVNL_BINARY_DIR}/src/vxl/core
  )
  set(
    ITKVNL_LIBRARIES
    itkvnl_algo
    itkvnl
    itkv3p_netlib
  )

  if(ITK_TEMPLATE_VISIBILITY_DEFAULT)
    add_definitions(
      "-DVNL_TEMPLATE_EXPORT=__attribute__((visibility(\"default\")))"
    )
  endif()
endif()

itk_module_impl()

mark_as_advanced(
  FORCE
  TESTLIB_VCL_WHERE_ROOT_DIR_H
  VNL_CONFIG_CHECK_BOUNDS
  VXL_BUILD_CORE_NUMERICS
  VXL_EXTRA_CMAKE_C_FLAGS
  VXL_EXTRA_CMAKE_CXX_FLAGS
  VXL_EXTRA_CMAKE_EXE_LINKER_FLAGS
  VXL_EXTRA_CMAKE_MODULE_LINKER_FLAGS
  VXL_EXTRA_CMAKE_SHARED_LINKER_FLAGS
  VXL_LEGACY_ERROR_REPORTING
  VXL_RUN_FAILING_TESTS
  VXL_UPDATE_CONFIGURATION
)
