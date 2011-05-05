project(ITK-VNL)
set(ITK-VNL_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_VXL "Use an outside build of VXL." OFF)
mark_as_advanced(ITK_USE_SYSTEM_VXL)

if(ITK_USE_SYSTEM_VXL)
  find_package(VXL REQUIRED)
  set(ITK-VNL_SYSTEM_INCLUDE_DIRS
    ${VXL_NETLIB_INCLUDE_DIR}
    ${VXL_VCL_INCLUDE_DIR}
    ${VXL_CORE_INCLUDE_DIR}
    )
  set(ITK-VNL_LIBRARIES vnl_algo vnl v3p_netlib netlib vcl)
  set(ITK-VNL_SYSTEM_LIBRARY_DIRS "${VXL_LIBRARY_DIR}")
  set(ITK-VNL_NO_SRC 1)
else()
  set(ITK-VNL_INCLUDE_DIRS
    ${ITK-VNL_SOURCE_DIR}/src/vxl/v3p/netlib
    ${ITK-VNL_SOURCE_DIR}/src/vxl/vcl
    ${ITK-VNL_SOURCE_DIR}/src/vxl/core
    ${ITK-VNL_BINARY_DIR}/src/vxl/v3p/netlib
    ${ITK-VNL_BINARY_DIR}/src/vxl/vcl
    ${ITK-VNL_BINARY_DIR}/src/vxl/core

    )
  set(ITK-VNL_LIBRARIES itkvnl_algo itkvnl itkv3p_netlib)
endif()

itk_module_impl()
