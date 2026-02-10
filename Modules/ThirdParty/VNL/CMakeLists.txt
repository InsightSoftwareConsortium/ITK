project(ITKVNL)
set(ITKVNL_THIRD_PARTY 1)

option(ITK_USE_SYSTEM_VXL "Use an outside build of VXL." OFF)
mark_as_advanced(ITK_USE_SYSTEM_VXL)

if(ITK_USE_SYSTEM_VXL)
  find_package(VXL 3.0.0 REQUIRED)
  # 3.0.0 is the first version of VXL that supports
  # the requirement of front() and back() members
  # in the vnl_vector and vnl_vector_fixed.
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
    vcl
  )
  set(ITKVNL_SYSTEM_LIBRARY_DIRS "${VXL_LIBRARY_DIR}")
  set(ITKVNL_NO_SRC 1)
else()
  # ITKv5 no longer supports legacy method signatures for VNL
  set(
    VNL_CONFIG_LEGACY_METHODS
    OFF
    CACHE BOOL
    "Whether backward-compatibility methods are provided by vnl."
    FORCE
  )
  set(VNL_CONFIG_CHECK_BOUNDS OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_BUILD_CONTRIB OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_BUILD_CORE_GEOMETRY OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_BUILD_CORE_IMAGING OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_BUILD_CORE_NUMERICS ON CACHE BOOL "VXL configuration for ITK" FORCE)
  set(
    VXL_BUILD_CORE_NUMERICS_ONLY
    ON
    CACHE BOOL
    "VXL configuration for ITK"
    FORCE
  ) #This hides other VXL core options from CMake
  set(
    VXL_BUILD_CORE_PROBABILITY
    OFF
    CACHE BOOL
    "VXL configuration for ITK"
    FORCE
  )
  set(
    VXL_BUILD_CORE_SERIALISATION
    OFF
    CACHE BOOL
    "VXL configuration for ITK"
    FORCE
  )
  set(VXL_BUILD_CORE_UTILITIES OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_BUILD_CORE_VIDEO OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_BUILD_DCMTK OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_BUILD_DOCUMENTATION OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_BUILD_EXAMPLES OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(
    VXL_BUILD_NONDEPRECATED_ONLY
    ON
    CACHE BOOL
    "VXL configuration for ITK"
    FORCE
  )
  set(
    VXL_BUILD_OBJECT_LIBRARIES
    OFF
    CACHE BOOL
    "VXL configuration for ITK"
    FORCE
  )
  set(VXL_BUILD_VGUI OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_NO_EXPORT ON CACHE BOOL "VXL configuration for ITK" FORCE)
  #
  set(VXL_FORCE_B3P_EXPAT OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_B3P_SHAPELIB OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_BZLIB2 OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_CLIPPER OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_DCMTK OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_GEOTIFF OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_J2K OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_JPEG OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_OPENJPEG2 OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_PNG OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_RPLY OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_TIFF OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_FORCE_V3P_ZLIB OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  #
  set(VXL_RUN_FAILING_TESTS OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  set(VXL_UPDATE_CONFIGURATION OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  # set(VXL_USE_DCMTK                    OFF CACHE BOOL "VXL configuration for ITK" FORCE) # <- Not visible in itk extracted code
  # set(VXL_USE_ECW                      OFF CACHE BOOL "VXL configuration for ITK" FORCE) # <- Visible, but unreachable in itk extracted code
  set(VXL_USE_GEOTIFF OFF CACHE BOOL "VXL configuration for ITK" FORCE)
  if(ITK_LEGACY_REMOVE OR ITK_FUTURE_LEGACY_REMOVE)
    set(
      VXL_USE_HISTORICAL_IMPLICIT_CONVERSIONS
      OFF
      CACHE BOOL
      "Allow default deprecated implicit conversions."
      FORCE
    )
  else()
    set(
      VXL_USE_HISTORICAL_IMPLICIT_CONVERSIONS
      ON
      CACHE BOOL
      "Allow default deprecated implicit conversions."
      FORCE
    )
  endif()
  # MUST ALWAYS BE ON currently
  set(
    VXL_USE_HISTORICAL_PROTECTED_IVARS
    ON
    CACHE BOOL
    "VXL configuration for ITK"
    FORCE
  )

  set(VXL_USE_LFS OFF CACHE BOOL "VXL configuration for ITK" FORCE) # <-visible in ITK vnl, must be OFF
  # set(VXL_VIL_INCLUDE_IMAGE_IO         ON  CACHE BOOL "VXL configuration for ITK" FORCE) # <- not visible in ITK
  set(VXL_LIB_PREFIX "itk")
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
    itkvcl
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
  VXL_BUILD_CONTRIB
  VXL_BUILD_CORE_GEOMETRY
  VXL_BUILD_CORE_IMAGING
  VXL_BUILD_CORE_NUMERICS
  VXL_BUILD_CORE_NUMERICS_ONLY
  VXL_BUILD_CORE_PROBABILITY
  VXL_BUILD_CORE_SERIALISATION
  VXL_BUILD_CORE_UTILITIES
  VXL_BUILD_CORE_VIDEO
  VXL_BUILD_DCMTK
  VXL_BUILD_DOCUMENTATION
  VXL_BUILD_EXAMPLES
  VXL_BUILD_FOR_VXL_DASHBOARD
  VXL_BUILD_NONDEPRECATED_ONLY
  VXL_BUILD_OBJECT_LIBRARIES
  VXL_BUILD_VGUI
  VXL_EXTRA_CMAKE_C_FLAGS
  VXL_EXTRA_CMAKE_CXX_FLAGS
  VXL_EXTRA_CMAKE_EXE_LINKER_FLAGS
  VXL_EXTRA_CMAKE_MODULE_LINKER_FLAGS
  VXL_EXTRA_CMAKE_SHARED_LINKER_FLAGS
  VXL_FORCE_B3P_EXPAT
  VXL_FORCE_B3P_SHAPELIB
  VXL_FORCE_V3P_BZLIB2
  VXL_FORCE_V3P_CLIPPER
  VXL_FORCE_V3P_DCMTK
  VXL_FORCE_V3P_GEOTIFF
  VXL_FORCE_V3P_J2K
  VXL_FORCE_V3P_JPEG
  VXL_FORCE_V3P_OPENJPEG2
  VXL_FORCE_V3P_PNG
  VXL_FORCE_V3P_RPLY
  VXL_FORCE_V3P_TIFF
  VXL_FORCE_V3P_ZLIB
  VXL_LEGACY_ERROR_REPORTING
  VXL_NO_EXPORT
  VXL_RUN_FAILING_TESTS
  VXL_UPDATE_CONFIGURATION
  #VXL_USE_DCMTK <- Not visible in itk extracted code
  #VXL_USE_ECW   <- Visible, but unreachable in itk extracted code
  VXL_USE_GEOTIFF # <- Visible, and must be off
  VXL_USE_HISTORICAL_IMPLICIT_CONVERSIONS # <- must track ITK LegacyRemove setting
  VXL_USE_HISTORICAL_PROTECTED_IVARS # <- must track ITK LegacyRemove setting
  VXL_USE_LFS # <- Visible, and must be off
  #VXL_VIL_INCLUDE_IMAGE_IO <- Not visible in itk extracted code
)
