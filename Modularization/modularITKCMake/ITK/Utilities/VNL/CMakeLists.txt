cmake_minimum_required(VERSION 2.8 FATAL_ERROR)
project(ITK-VNL)
find_package(itk-base 4.0 EXACT REQUIRED)

option(ITK_USE_SYSTEM_VXL "Use an outside build of VXL." OFF)
mark_as_advanced(ITK_USE_SYSTEM_VXL)

if(ITK_USE_SYSTEM_VXL)
  find_package(VXL REQUIRED)
  set(ITK-VNL_INCLUDE_DIRS
    ${VXL_NETLIB_INCLUDE_DIR}
    ${VXL_VCL_INCLUDE_DIR}
    ${VXL_CORE_INCLUDE_DIR}
    )
  set(ITK-VNL_LIBRARIES vnl_algo vnl v3p_netlib netlib vcl)
  set(itk-module-config-install "
list(APPEND ITK-VNL_LIBRARY_DIRS \"${VXL_LIBRARY_DIR}\")
list(APPEND ITK-VNL_INCLUDE_DIRS \"${ITK-VNL_INCLUDE_DIRS}\")
")
  set(itk-module-config-build "
list(APPEND ITK-VNL_LIBRARY_DIRS \"${VXL_LIBRARY_DIR}\")
")
  set(itk-module-no-src 1)
else()
  set(ITK-VNL-target itkvnl)
  set(ITK-VNL_INCLUDE_DIRS
    ${ITK-VNL_SOURCE_DIR}/src/v3p/netlib
    ${ITK-VNL_SOURCE_DIR}/src/vcl
    ${ITK-VNL_SOURCE_DIR}/src/core
    ${ITK-VNL_BINARY_DIR}/src/v3p/netlib
    ${ITK-VNL_BINARY_DIR}/src/vcl
    ${ITK-VNL_BINARY_DIR}/src/core
    )
  set(ITK-VNL_LIBRARIES itkvnl_algo itkvnl itkv3p_netlib)
endif()

itk_module_impl()

if(NOT ITK_USE_SYSTEM_VXL)
  foreach(lib itkvcl itkv3p_netlib itkv3p_lsqr itktestlib itkvnl itkvnl_algo)
    itk_module_target_name(${lib})
    itk_module_target_export(${lib})
  endforeach()
endif()
