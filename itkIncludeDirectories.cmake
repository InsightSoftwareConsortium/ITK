#-----------------------------------------------------------------------------
# Include directories for other projects installed on the system.
SET(ITK_INCLUDE_DIRS_SYSTEM "")

IF(ITK_USE_SYSTEM_VXL)
  # System VXL include directories.
  SET(ITK_INCLUDE_DIRS_SYSTEM ${ITK_INCLUDE_DIRS_SYSTEM}
    ${VXL_VCL_INCLUDE_DIR} ${VXL_CORE_INCLUDE_DIR}
    )
ENDIF(ITK_USE_SYSTEM_VXL)

#-----------------------------------------------------------------------------
# Include directories from the build tree.
SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_BINARY_DIR})

# These directories are always needed.
SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
  ${ITK_SOURCE_DIR}/Code/Algorithms
  ${ITK_SOURCE_DIR}/Code/BasicFilters
  ${ITK_SOURCE_DIR}/Code/Common
  ${ITK_SOURCE_DIR}/Code/Numerics
  ${ITK_SOURCE_DIR}/Code/IO
  ${ITK_SOURCE_DIR}/Code/Numerics/FEM
  ${ITK_SOURCE_DIR}/Code/Numerics/Statistics
  ${ITK_SOURCE_DIR}/Code/Numerics/NeuralNetworks
  ${ITK_SOURCE_DIR}/Code/SpatialObject
  ${ITK_SOURCE_DIR}/Utilities/MetaIO
  ${ITK_SOURCE_DIR}/Utilities/NrrdIO
  ${ITK_SOURCE_DIR}/Utilities/DICOMParser
  ${ITK_BINARY_DIR}/Utilities/DICOMParser
  ${ITK_BINARY_DIR}/Utilities/expat
  ${ITK_SOURCE_DIR}/Utilities/expat
  ${ITK_SOURCE_DIR}/Utilities/nifti/niftilib
  ${ITK_SOURCE_DIR}/Utilities/nifti/znzlib
  ${ITK_BINARY_DIR}/Utilities/gdcm
  ${ITK_BINARY_DIR}/Utilities
  ${ITK_SOURCE_DIR}/Utilities
)

IF(USE_LOG4CXX)
SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
  ${ITK_SOURCE_DIR}/Utilities/log4cxx/include
  ${ITK_BINARY_DIR}/Utilities/log4cxx/include
)
ENDIF(USE_LOG4CXX)

IF(USE_FFTWF OR USE_FFTWD)
SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
  ${FFTW_INCLUDE_PATH}
)
ENDIF(USE_FFTWF OR USE_FFTWD)

# VXL include directories.
IF(NOT ITK_USE_SYSTEM_VXL)
  SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_SOURCE_DIR}/Utilities/vxl/vcl
    ${ITK_SOURCE_DIR}/Utilities/vxl/core
    ${ITK_BINARY_DIR}/Utilities/vxl/vcl
    ${ITK_BINARY_DIR}/Utilities/vxl/core
    )
ENDIF(NOT ITK_USE_SYSTEM_VXL)

# Patended include directories added only if the user explicitly enabled the
# ITK_USE_PATENTED option. Users are responsible for getting a license from the
# patent holders in order to use any of those methods.
IF(ITK_USE_PATENTED)
  SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_SOURCE_DIR}/Code/Patented
    )
ENDIF(ITK_USE_PATENTED)


#-----------------------------------------------------------------------------
# Include directories needed for .cxx files in ITK.  These include
# directories will NOT be available to user projects.
SET(ITK_INCLUDE_DIRS_BUILD_TREE_CXX)
IF(ITK_USE_SYSTEM_VXL)
  SET(ITK_INCLUDE_DIRS_BUILD_TREE_CXX ${ITK_INCLUDE_DIRS_BUILD_TREE_CXX}
    ${VXL_NETLIB_INCLUDE_DIR})
ELSE(ITK_USE_SYSTEM_VXL)
  SET(ITK_INCLUDE_DIRS_BUILD_TREE_CXX ${ITK_INCLUDE_DIRS_BUILD_TREE_CXX}
    ${ITK_SOURCE_DIR}/Utilities/vxl/v3p/netlib)
ENDIF(ITK_USE_SYSTEM_VXL)

#-----------------------------------------------------------------------------
# Include directories from the install tree.
SET(ITK_INSTALL_INCLUDE_DIR "${CMAKE_INSTALL_PREFIX}/include/InsightToolkit")
SET(ITK_INCLUDE_DIRS_INSTALL_TREE ${ITK_INCLUDE_DIRS_INSTALL_TREE}
  ${ITK_INSTALL_INCLUDE_DIR}
  ${ITK_INSTALL_INCLUDE_DIR}/Algorithms
  ${ITK_INSTALL_INCLUDE_DIR}/BasicFilters
  ${ITK_INSTALL_INCLUDE_DIR}/Common
  ${ITK_INSTALL_INCLUDE_DIR}/expat
  ${ITK_INSTALL_INCLUDE_DIR}/Numerics
  ${ITK_INSTALL_INCLUDE_DIR}/IO
  ${ITK_INSTALL_INCLUDE_DIR}/Numerics/FEM
  ${ITK_INSTALL_INCLUDE_DIR}/Numerics/Statistics
  ${ITK_INSTALL_INCLUDE_DIR}/Numerics/NeuralNetworks
  ${ITK_INSTALL_INCLUDE_DIR}/SpatialObject
  ${ITK_INSTALL_INCLUDE_DIR}/Utilities/MetaIO
  ${ITK_INSTALL_INCLUDE_DIR}/Utilities/NrrdIO
  ${ITK_INSTALL_INCLUDE_DIR}/Utilities/DICOMParser
  ${ITK_INSTALL_INCLUDE_DIR}/Utilities
)

IF(NOT ITK_USE_SYSTEM_VXL)
  SET(ITK_INCLUDE_DIRS_INSTALL_TREE ${ITK_INCLUDE_DIRS_INSTALL_TREE}
    ${ITK_INSTALL_INCLUDE_DIR}/Utilities/vxl/vcl
    ${ITK_INSTALL_INCLUDE_DIR}/Utilities/vxl/core
    )
ENDIF(NOT ITK_USE_SYSTEM_VXL)

# Patended include directories added only if the user explicitly enabled the
# ITK_USE_PATENTED option. Users are responsible for getting a license from the
# patent holders in order to use any of those methods.
IF(ITK_USE_PATENTED)
  SET(ITK_INCLUDE_DIRS_INSTALL_TREE ${ITK_INCLUDE_DIRS_INSTALL_TREE}
    ${ITK_INSTALL_INCLUDE_DIR}/Patented
    )
ENDIF(ITK_USE_PATENTED)

#-----------------------------------------------------------------------------
# Include directories for 3rd-party utilities provided by ITK.
ITK_THIRD_PARTY_INCLUDE(ZLIB zlib)
ITK_THIRD_PARTY_INCLUDE(PNG  png)
ITK_THIRD_PARTY_INCLUDE(TIFF tiff)
