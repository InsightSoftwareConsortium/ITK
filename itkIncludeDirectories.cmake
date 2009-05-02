#-----------------------------------------------------------------------------
# Include directories for other projects installed on the system.
SET(ITK_INCLUDE_DIRS_SYSTEM "")

IF(ITK_USE_SYSTEM_VXL)
  # System VXL include directories.
  SET(ITK_INCLUDE_DIRS_SYSTEM ${ITK_INCLUDE_DIRS_SYSTEM}
    ${VXL_VCL_INCLUDE_DIR} ${VXL_CORE_INCLUDE_DIR}
    )
ENDIF(ITK_USE_SYSTEM_VXL)

IF(ITK_USE_SYSTEM_GDCM)
  SET(ITK_INCLUDE_DIRS_SYSTEM ${GDCM_DIR})
ENDIF(ITK_USE_SYSTEM_GDCM)

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
  ${ITK_SOURCE_DIR}/Code/Numerics/NeuralNetworks
  ${ITK_SOURCE_DIR}/Code/SpatialObject
  ${ITK_SOURCE_DIR}/Utilities/MetaIO
  ${ITK_SOURCE_DIR}/Utilities/NrrdIO
  ${ITK_BINARY_DIR}/Utilities/NrrdIO
  ${ITK_SOURCE_DIR}/Utilities/DICOMParser
  ${ITK_BINARY_DIR}/Utilities/DICOMParser
  ${ITK_BINARY_DIR}/Utilities/expat
  ${ITK_SOURCE_DIR}/Utilities/expat
  ${ITK_SOURCE_DIR}/Utilities/nifti/niftilib
  ${ITK_SOURCE_DIR}/Utilities/nifti/znzlib
  ${ITK_SOURCE_DIR}/Utilities/itkExtHdrs
  ${ITK_BINARY_DIR}/Utilities
  ${ITK_SOURCE_DIR}/Utilities
)


IF( NOT ITK_USE_REVIEW_STATISTICS )
  SET( ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
       ${ITK_SOURCE_DIR}/Code/Numerics/Statistics )
ENDIF( NOT ITK_USE_REVIEW_STATISTICS )


# For explicit instantiation.  Probably should make it ${ITK_BINARY_DIR}/Code/Common
IF(ITK_EXPLICIT_INSTANTIATION)
  SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_BINARY_DIR}/Code/Common
    )
ENDIF(ITK_EXPLICIT_INSTANTIATION)

# Directories needed when using FFTW
IF(USE_FFTWF OR USE_FFTWD)
SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
  ${FFTW_INCLUDE_PATH}
)
ENDIF(USE_FFTWF OR USE_FFTWD)

# VXL include directories.
IF(NOT ITK_USE_SYSTEM_VXL)
  SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_SOURCE_DIR}/Utilities/vxl/v3p/netlib
    ${ITK_SOURCE_DIR}/Utilities/vxl/vcl
    ${ITK_SOURCE_DIR}/Utilities/vxl/core
    ${ITK_BINARY_DIR}/Utilities/vxl/v3p/netlib
    ${ITK_BINARY_DIR}/Utilities/vxl/vcl
    ${ITK_BINARY_DIR}/Utilities/vxl/core
    )
ENDIF(NOT ITK_USE_SYSTEM_VXL)

# GDCM include directories.
IF(NOT ITK_USE_SYSTEM_GDCM)
  SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_BINARY_DIR}/Utilities/gdcm
    ${ITK_SOURCE_DIR}/Utilities/gdcm/src
    )
ENDIF(NOT ITK_USE_SYSTEM_GDCM)

# LIBXML2 include directories.
IF(ITK_USE_LIBXML2)
  SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_BINARY_DIR}/Utilities/itklibxml2
    )
ENDIF(ITK_USE_LIBXML2)

# Patended include directories added only if the user explicitly enabled the
# ITK_USE_PATENTED option. Users are responsible for getting a license from the
# patent holders in order to use any of those methods.
IF(ITK_USE_PATENTED)
  SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_SOURCE_DIR}/Code/Patented
    )
ENDIF(ITK_USE_PATENTED)


# Review include directories added only if the user explicitly enabled the
# ITK_USE_REVIEW option. Users are responsible for getting a license from the
# copyright holders in order to use any of those methods. The methods are not
# covered by the backward compatibility policy either.
IF(ITK_USE_REVIEW)
  SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_SOURCE_DIR}/Code/Review
    )
  IF(ITK_USE_REVIEW_STATISTICS)
    SET(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
      ${ITK_SOURCE_DIR}/Code/Review/Statistics )
  ENDIF(ITK_USE_REVIEW_STATISTICS)
ENDIF(ITK_USE_REVIEW)

IF( NOT ITK_USE_REVIEW_STATISTICS )
  SET(ITK_INCLUDE_RELATIVE_DIRS ${ITK_INCLUDE_RELATIVE_DIRS} Numerics/Statistics)
ENDIF( NOT ITK_USE_REVIEW_STATISTICS )



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
SET(ITK_INSTALL_INCLUDE_PATH "${CMAKE_INSTALL_PREFIX}${ITK_INSTALL_INCLUDE_DIR}")
SET(ITK_INCLUDE_RELATIVE_DIRS ${ITK_INCLUDE_RELATIVE_DIRS}
  Algorithms
  BasicFilters
  Common
  Numerics
  IO
  Numerics/FEM
  Numerics/NeuralNetworks
  SpatialObject
  Utilities/MetaIO
  Utilities/NrrdIO
  Utilities/DICOMParser
  Utilities/expat
  Utilities/nifti/niftilib
  Utilities/nifti/znzlib
  Utilities/itkExtHdrs
  Utilities
)

IF(NOT ITK_USE_SYSTEM_VXL)
  SET(ITK_INCLUDE_RELATIVE_DIRS ${ITK_INCLUDE_RELATIVE_DIRS}
    Utilities/vxl/v3p/netlib
    Utilities/vxl/vcl
    Utilities/vxl/core
    )
ENDIF(NOT ITK_USE_SYSTEM_VXL)

IF(NOT ITK_USE_SYSTEM_GDCM)
  SET(ITK_INCLUDE_RELATIVE_DIRS ${ITK_INCLUDE_RELATIVE_DIRS}
    gdcm
    gdcm/src
    )
ENDIF(NOT ITK_USE_SYSTEM_GDCM)

# Patended include directories added only if the user explicitly enabled the
# ITK_USE_PATENTED option. Users are responsible for getting a license from the
# patent holders in order to use any of those methods.
IF(ITK_USE_PATENTED)
  SET(ITK_INCLUDE_RELATIVE_DIRS ${ITK_INCLUDE_RELATIVE_DIRS}
    Patented
    )
ENDIF(ITK_USE_PATENTED)

# Review include directories added only if the user explicitly enabled the
# ITK_USE_REVIEW option. Users are responsible for getting a license from the
# copyright holders in order to use any of those methods. The methods are not
# covered by the backward compatibility policy either.
IF(ITK_USE_REVIEW)
  SET(ITK_INCLUDE_RELATIVE_DIRS ${ITK_INCLUDE_RELATIVE_DIRS}
    Review
    )
  IF(ITK_USE_REVIEW_STATISTICS)
    SET(ITK_INCLUDE_RELATIVE_DIRS ${ITK_INCLUDE_RELATIVE_DIRS}
      Review/Statistics )
  ENDIF(ITK_USE_REVIEW_STATISTICS)  
ENDIF(ITK_USE_REVIEW)


#-----------------------------------------------------------------------------
# Include directories for 3rd-party utilities provided by ITK.
ITK_THIRD_PARTY_INCLUDE2(PNG)
ITK_THIRD_PARTY_INCLUDE2(TIFF)
ITK_THIRD_PARTY_INCLUDE2(ZLIB)
IF(ITK_USE_SYSTEM_LIBXML2 OR ITK_USE_LIBXML2)
  ITK_THIRD_PARTY_INCLUDE(LIBXML2 libxml2)
ENDIF(ITK_USE_SYSTEM_LIBXML2 OR ITK_USE_LIBXML2)

