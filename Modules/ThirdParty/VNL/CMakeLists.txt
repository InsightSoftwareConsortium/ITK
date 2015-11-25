project(ITKVNL)
set(ITKVNL_THIRD_PARTY 1)

# ITK and several external packages still use legacy method signatures for VNL
set(VNL_CONFIG_LEGACY_METHODS CACHE BOOL "Whether backward-compatibility methods are provided by vnl." FORCE ON)

option(ITK_USE_SYSTEM_VXL "Use an outside build of VXL." OFF)
mark_as_advanced(ITK_USE_SYSTEM_VXL)

if(ITK_USE_SYSTEM_VXL)
  find_package(VXL REQUIRED)
  set(ITKVNL_SYSTEM_INCLUDE_DIRS
    ${VXL_NETLIB_INCLUDE_DIR}
    ${VXL_VCL_INCLUDE_DIR}
    ${VXL_CORE_INCLUDE_DIR}
    )
  set(ITKVNL_LIBRARIES vnl_algo vnl v3p_netlib netlib vcl)
  set(ITKVNL_SYSTEM_LIBRARY_DIRS "${VXL_LIBRARY_DIR}")
  set(ITKVNL_NO_SRC 1)
else()
  set(ITKVNL_INCLUDE_DIRS
    ${ITKVNL_SOURCE_DIR}/src/vxl/v3p/netlib
    ${ITKVNL_SOURCE_DIR}/src/vxl/vcl
    ${ITKVNL_SOURCE_DIR}/src/vxl/core
    ${ITKVNL_BINARY_DIR}/src/vxl/v3p/netlib
    ${ITKVNL_BINARY_DIR}/src/vxl/vcl
    ${ITKVNL_BINARY_DIR}/src/vxl/core

    )
  set(ITKVNL_LIBRARIES itkvnl_algo itkvnl itkv3p_netlib)
endif()

itk_module_impl()
