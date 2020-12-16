project(ITKVNL)
set(ITKVNL_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_VXL "Use an outside build of VXL." OFF)
mark_as_advanced(ITK_USE_SYSTEM_VXL)

if(ITK_USE_SYSTEM_VXL)
  find_package(VXL 3.0.0 REQUIRED)
  # 3.0.0 is the first version of VXL that supports
  # the requirement of front() and back() members
  # in the vnl_vector and vnl_vector_fixed.
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

  # ITKv5 no longer supports legacy method signatures for VNL
  set(VNL_CONFIG_LEGACY_METHODS     OFF CACHE BOOL "Whether backward-compatibility methods are provided by vnl." FORCE )
  set(VXL_BUILD_CORE_NUMERICS       ON  CACHE BOOL "VXL configuration for ITK" FORCE )
  set(VXL_BUILD_CORE_NUMERICS_ONLY  ON  CACHE BOOL "VXL configuration for ITK" FORCE ) #This hides other VXL core options from CMake
  set(VXL_NO_EXPORT                 ON  CACHE BOOL "VXL configuration for ITK" FORCE )
  set(VXL_BUILD_CORE_GEOMETRY       OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(VXL_BUILD_CORE_IMAGING        OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(VXL_BUILD_CORE_IMAGING        OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(VXL_BUILD_CORE_SERIALISATION  OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(VXL_BUILD_CORE_UTILITIES      OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(VXL_BUILD_CORE_VIDEO          OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(VXL_BUILD_CONTRIB             OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(VXL_BUILD_EXAMPLES            OFF CACHE BOOL "VXL configuration for ITK" FORCE )
#
  set(VXL_FORCE_V3P_CLIPPER     OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(VXL_FORCE_V3P_ZLIB        OFF CACHE BOOL "VXL configuration for ITK" FORCE )
  set(VXL_FORCE_B3P_EXPAT       OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_B3P_SHAPELIB    OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_BZLIB2      OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_CLIPPER     OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_DCMTK       OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_GEOTIFF     OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_J2K         OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_JPEG        OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_OPENJPEG2   OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_PNG         OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_TIFF        OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_ZLIB        OFF CACHE BOOL "VXL configuration for ITK" FORCE)

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
  set(ITKVNL_LIBRARIES itkvnl_algo itkvnl itkv3p_netlib itkvcl)

  if(ITK_TEMPLATE_VISIBILITY_DEFAULT)
    add_definitions( "-DVNL_TEMPLATE_EXPORT=__attribute__((visibility(\"default\")))")
  endif()
endif()

itk_module_impl()

mark_as_advanced(FORCE
  VXL_BUILD_CONTRIB
  VXL_FORCE_B3P_EXPAT
  VXL_FORCE_B3P_SHAPELIB
  VXL_BUILD_CONTRIB
  VXL_BUILD_CORE_GEOMETRY
  VXL_BUILD_CORE_IMAGING
  VXL_BUILD_CORE_NUMERICS
  VXL_BUILD_CORE_SERIALISATION
  VXL_BUILD_CORE_UTILITIES
  VXL_BUILD_FOR_VXL_DASHBOARD
  VXL_BUILD_NONDEPRECATED_ONLY
  VXL_TESTLIB_VCL_WHERE_ROOT_DIR_H
  TESTLIB_VCL_WHERE_ROOT_DIR_H
  VXL_EXTRA_CMAKE_CXX_FLAGS
  VXL_EXTRA_CMAKE_C_FLAGS
  VXL_EXTRA_CMAKE_EXE_LINKER_FLAGS
  VXL_EXTRA_CMAKE_MODULE_LINKER_FLAGS
  VXL_EXTRA_CMAKE_SHARED_LINKER_FLAGS
  VXL_LEGACY_ERROR_REPORTING
  VXL_NO_EXPORT
  )
