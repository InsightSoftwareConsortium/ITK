project(ITKVNL)
set(ITKVNL_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_VXL "Use an outside build of VXL." OFF)
mark_as_advanced(ITK_USE_SYSTEM_VXL)

if(ITK_USE_SYSTEM_VXL)
  find_package(VXL REQUIRED)
  set(ITKVNL_SYSTEM_INCLUDE_DIRS
    ${VXL_NETLIB_INCLUDE_DIR}
    ${VXL_VCL_INCLUDE_DIRS}
    ${VXL_VCL_INCLUDE_DIR}
    ${VXL_CORE_INCLUDE_DIRS}
    ${VXL_CORE_INCLUDE_DIR}
    )
  set(ITKVNL_LIBRARIES vnl_algo vnl v3p_netlib netlib vcl)
  set(ITKVNL_SYSTEM_LIBRARY_DIRS "${VXL_LIBRARY_DIR}")
  set(ITKVNL_NO_SRC 1)
else()

  # ITK and several external packages still use legacy method signatures for VNL
  set(VNL_CONFIG_LEGACY_METHODS ON CACHE BOOL "Whether backward-compatibility methods are provided by vnl." FORCE )
  set(BUILD_CORE_NUMERICS       ON  CACHE BOOL "VXL configuration for ITK" FORCE )
  set(VXL_NO_EXPORT             ON CACHE BOOL "VXL configuration for ITK" FORCE )
  set(BUILD_CORE_GEOMETRY       OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(BUILD_CORE_IMAGING        OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(BUILD_CORE_SERIALISATION  OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(BUILD_CORE_UTILITIES      OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(BUILD_CORE_VIDEO          OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(BUILD_CONTRIB             OFF CACHE BOOL "VXL configuration for ITK" FORCE )
#
  set(VXL_FORCE_V3P_CLIPPER     OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(VXL_FORCE_V3P_ZLIB        OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(BUILD_CORE_IMAGING        OFF CACHE BOOL "VXL configuration for ITK" FORCE )

  mark_as_advanced(FORCE BUILD_CONTRIB
    BUILD_CORE_GEOMETRY
    BUILD_CORE_IMAGING
    BUILD_CORE_NUMERICS
    BUILD_CORE_SERIALISATION
    BUILD_CORE_UTILITIES
    BUILD_FOR_VXL_DASHBOARD
    BUILD_NONDEPRECATED_ONLY
    TESTLIB_VCL_WHERE_ROOT_DIR_H
    VCL_INCLUDE_CXX_0X
    VXL_EXTRA_CMAKE_CXX_FLAGS
    VXL_EXTRA_CMAKE_C_FLAGS
    VXL_EXTRA_CMAKE_EXE_LINKER_FLAGS
    VXL_EXTRA_CMAKE_MODULE_LINKER_FLAGS
    VXL_EXTRA_CMAKE_SHARED_LINKER_FLAGS
    VXL_LEGACY_ERROR_REPORTING
    VXL_NO_EXPORT
    )

  set(VXL_LIB_PREFIX            "itk")
  set(VXL_LIBRARY_PROPERTIES ${ITK_LIBRARY_PROPERTIES})

  set(ITKVNL_INCLUDE_DIRS
    ${ITKVNL_SOURCE_DIR}/src/vxl/v3p/netlib
    ${ITKVNL_SOURCE_DIR}/src/vxl/vcl
    ${ITKVNL_SOURCE_DIR}/src/vxl/core
    ${ITKVNL_BINARY_DIR}/src/vxl/v3p/netlib
    ${ITKVNL_BINARY_DIR}/src/vxl/vcl
    ${ITKVNL_BINARY_DIR}/src/vxl/core
    )
  set(ITKVNL_LIBRARIES itkvnl_algo itkvnl itkv3p_netlib itknetlib itkvcl)

  if(ITK_TEMPLATE_VISIBILITY_DEFAULT)
    add_definitions( "-DVNL_TEMPLATE_EXPORT=__attribute__((visibility(\"default\")))")
  endif()
endif()

itk_module_impl()
