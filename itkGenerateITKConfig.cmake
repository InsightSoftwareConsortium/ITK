# Generate the ITKConfig.cmake file in the build tree.  Also configure
# one for installation.  The file tells external projects how to use
# ITK.

#-----------------------------------------------------------------------------
# Settings specific to the build tree.

# The "use" file.
SET(ITK_USE_FILE ${ITK_BINARY_DIR}/UseITK.cmake)

# The library dependencies file.
SET(ITK_LIBRARY_DEPENDS_FILE ${ITK_BINARY_DIR}/ITKLibraryDepends.cmake)

# The build settings file.
IF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} GREATER 1.4)
  SET(ITK_BUILD_SETTINGS_FILE ${ITK_BINARY_DIR}/ITKBuildSettings.cmake)
ENDIF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} GREATER 1.4)

# Library directory.
SET(ITK_LIBRARY_DIRS_CONFIG "${ITK_LIBRARY_PATH}")

# The include directories.
SET(ITK_INCLUDE_DIRS_CONFIG
  "${ITK_BINARY_DIR}"
  "${ITK_BINARY_DIR}/Code/Numerics/vxl"
  "${ITK_BINARY_DIR}/Code/Numerics/vxl/vcl"
  "${ITK_SOURCE_DIR}/Code/Algorithms"
  "${ITK_SOURCE_DIR}/Code/BasicFilters"
  "${ITK_SOURCE_DIR}/Code/Common"
  "${ITK_SOURCE_DIR}/Code/IO"
  "${ITK_SOURCE_DIR}/Code/Numerics/FEM"
  "${ITK_SOURCE_DIR}/Code/Numerics"
  "${ITK_SOURCE_DIR}/Code/Numerics/Statistics"
  "${ITK_SOURCE_DIR}/Code/Numerics/vxl"
  "${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl"
  "${ITK_SOURCE_DIR}/Code/SpatialObject"
  "${ITK_SOURCE_DIR}/Utilities/GlutMaster"
  "${ITK_SOURCE_DIR}/Utilities/MetaIO"
  "${ITK_SOURCE_DIR}/Utilities/MetaIO/SpatialObject"
  "${ITK_SOURCE_DIR}/Utilities/DICOMParser"
  "${ITK_SOURCE_DIR}/Utilities/png"
  "${ITK_SOURCE_DIR}/Utilities/zlib"
  "${ITK_SOURCE_DIR}/Auxiliary/FltkImageViewer"
  "${ITK_BINARY_DIR}/Auxiliary/FltkImageViewer"
  "${ITK_SOURCE_DIR}/Auxiliary/VtkFltk"
  "${ITK_BINARY_DIR}/Auxiliary/VtkFltk"
  "${ITK_SOURCE_DIR}/Auxiliary/vtk"
)
IF(NOT ITK_USE_SYSTEM_STDLIB)
  SET(ITK_INCLUDE_DIRS ${ITK_INCLUDE_DIRS} "${ITK_SOURCE_DIR}/Utilities/stdlib")
ENDIF(NOT ITK_USE_SYSTEM_STDLIB)

#-----------------------------------------------------------------------------
# Configure ITKConfig.cmake for the build tree.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/ITKConfig.cmake.in
               ${ITK_BINARY_DIR}/ITKConfig.cmake @ONLY IMMEDIATE)

#-----------------------------------------------------------------------------
# Settings specific to the install tree.

# The "use" file.
SET(ITK_USE_FILE ${CMAKE_INSTALL_PREFIX}/lib/InsightToolkit/UseITK.cmake)

# The library dependencies file.
SET(ITK_LIBRARY_DEPENDS_FILE ${CMAKE_INSTALL_PREFIX}/lib/InsightToolkit/ITKLibraryDepends.cmake)

# The build settings file.
IF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} GREATER 1.4)
  SET(ITK_BUILD_SETTINGS_FILE ${CMAKE_INSTALL_PREFIX}/lib/InsightToolkit/ITKBuildSettings.cmake)
ENDIF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} GREATER 1.4)

# Include directories.
SET(ITK_INSTALL_INCLUDE_DIR "${CMAKE_INSTALL_PREFIX}/include/InsightToolkit")
SET(ITK_INCLUDE_DIRS_CONFIG
  "${ITK_INSTALL_INCLUDE_DIR}"
  "${ITK_INSTALL_INCLUDE_DIR}/Numerics/vxl"
  "${ITK_INSTALL_INCLUDE_DIR}/Numerics/vxl/vcl"
  "${ITK_INSTALL_INCLUDE_DIR}/Algorithms"
  "${ITK_INSTALL_INCLUDE_DIR}/BasicFilters"
  "${ITK_INSTALL_INCLUDE_DIR}/Common"
  "${ITK_INSTALL_INCLUDE_DIR}/IO"
  "${ITK_INSTALL_INCLUDE_DIR}/Numerics/FEM"
  "${ITK_INSTALL_INCLUDE_DIR}/Numerics"
  "${ITK_INSTALL_INCLUDE_DIR}/Numerics/Statistics"
  "${ITK_INSTALL_INCLUDE_DIR}/Numerics/vxl"
  "${ITK_INSTALL_INCLUDE_DIR}/Numerics/vxl/vcl"
  "${ITK_INSTALL_INCLUDE_DIR}/SpatialObject"
  "${ITK_INSTALL_INCLUDE_DIR}/Utilities/GlutMaster"
  "${ITK_INSTALL_INCLUDE_DIR}/Utilities/MetaIO"
  "${ITK_INSTALL_INCLUDE_DIR}/Utilities/MetaIO/SpatialObject"
  "${ITK_INSTALL_INCLUDE_DIR}/Utilities/DICOMParser"
  "${ITK_INSTALL_INCLUDE_DIR}/Utilities/png"
  "${ITK_INSTALL_INCLUDE_DIR}/Utilities/zlib"
  "${ITK_INSTALL_INCLUDE_DIR}/Auxiliary/FltkImageViewer"
  "${ITK_INSTALL_INCLUDE_DIR}/Auxiliary/VtkFltk"
  "${ITK_INSTALL_INCLUDE_DIR}/Auxiliary/vtk"
)
IF(NOT ITK_USE_SYSTEM_STDLIB)
  SET(ITK_INCLUDE_DIRS ${ITK_INCLUDE_DIRS} "${ITK_INSTALL_INCLUDE_DIR}/Utilities/stdlib")
ENDIF(NOT ITK_USE_SYSTEM_STDLIB)

# Link directories.
SET(ITK_LIBRARY_DIRS_CONFIG ${CMAKE_INSTALL_PREFIX}/lib/InsightToolkit)

#-----------------------------------------------------------------------------
# Configure ITKConfig.cmake for the install tree.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/ITKConfig.cmake.in
               ${ITK_BINARY_DIR}/Utilities/ITKConfig.cmake @ONLY IMMEDIATE)

#-----------------------------------------------------------------------------
# Configure ITKLibraryDepends.cmake for both trees.

IF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} GREATER 1.4)
  SET(ITK_LIBS
    ITKAlgorithms ITKBasicFilters ITKCommon ITKFEM ITKIO ITKMetaIO
    ITKNumerics ITKStatistics VXLNumerics itkpng itkzlib Cio
    )

  # Write an input file that will be configured.
  STRING(ASCII 35 ITK_STRING_POUND)
  STRING(ASCII 64 ITK_STRING_AT)
  WRITE_FILE(${ITK_BINARY_DIR}/ITKLibraryDepends.cmake.in
             "${ITK_STRING_POUND} ITK Library Dependencies (for external projects)")
  FOREACH(lib ${ITK_LIBS})
    WRITE_FILE(${ITK_BINARY_DIR}/ITKLibraryDepends.cmake.in
     "SET(${lib}_LIB_DEPENDS \"${ITK_STRING_AT}${lib}_LIB_DEPENDS${ITK_STRING_AT}\")"
      APPEND
    )
  ENDFOREACH(lib)

  # Configure the file during the final pass so that the latest settings
  # for the *_LIB_DEPENDS cache entries will be available.
  CONFIGURE_FILE(${ITK_BINARY_DIR}/ITKLibraryDepends.cmake.in
                 ${ITK_BINARY_DIR}/ITKLibraryDepends.cmake @ONLY)
ELSE(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} GREATER 1.4)
  CONFIGURE_FILE(${ITK_SOURCE_DIR}/ITKLibraryDepends.cmake14.in
                 ${ITK_BINARY_DIR}/ITKLibraryDepends.cmake @ONLY)
ENDIF(${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION} GREATER 1.4)
