#-----------------------------------------------------------------------------
# Include directories for other projects installed on the system.
set(ITK_INCLUDE_DIRS_SYSTEM "")

if(ITK_USE_SYSTEM_VXL)
  # System VXL include directories.
  set(ITK_INCLUDE_DIRS_SYSTEM ${ITK_INCLUDE_DIRS_SYSTEM}
    ${VXL_VCL_INCLUDE_DIR} ${VXL_CORE_INCLUDE_DIR}
    )
endif(ITK_USE_SYSTEM_VXL)

if(ITK_USE_SYSTEM_GDCM)
  set(ITK_INCLUDE_DIRS_SYSTEM ${GDCM_DIR})
endif(ITK_USE_SYSTEM_GDCM)

#-----------------------------------------------------------------------------
# Include directories from the build tree.
set(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_BINARY_DIR})

# These directories are always needed.
set(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
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


set( ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE} ${ITK_SOURCE_DIR}/Code/Numerics/Statistics )


# For explicit instantiation.  Probably should make it ${ITK_BINARY_DIR}/Code/Common
if(ITK_EXPLICIT_INSTANTIATION)
  set(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_BINARY_DIR}/Code/Common
    )
endif(ITK_EXPLICIT_INSTANTIATION)

# Directories needed when using FFTW
if(USE_FFTWF OR USE_FFTWD)
set(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
  ${FFTW_INCLUDE_PATH}
)
endif(USE_FFTWF OR USE_FFTWD)

# VXL include directories.
if(NOT ITK_USE_SYSTEM_VXL)
  set(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_SOURCE_DIR}/Utilities/vxl/v3p/netlib
    ${ITK_SOURCE_DIR}/Utilities/vxl/vcl
    ${ITK_SOURCE_DIR}/Utilities/vxl/core
    ${ITK_BINARY_DIR}/Utilities/vxl/v3p/netlib
    ${ITK_BINARY_DIR}/Utilities/vxl/vcl
    ${ITK_BINARY_DIR}/Utilities/vxl/core
    )
endif(NOT ITK_USE_SYSTEM_VXL)

# GDCM include directories.
if(NOT ITK_USE_SYSTEM_GDCM)
  set(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_SOURCE_DIR}/Utilities/gdcm/Source/Common
    ${ITK_SOURCE_DIR}/Utilities/gdcm/Source/DataStructureAndEncodingDefinition
    ${ITK_SOURCE_DIR}/Utilities/gdcm/Source/MediaStorageAndFileFormat
    ${ITK_SOURCE_DIR}/Utilities/gdcm/Source/DataDictionary
    ${ITK_BINARY_DIR}/Utilities/gdcm/Source/Common # generated gdcmConfigure.h
    )
  if(MSVC)
    set(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
      ${ITK_SOURCE_DIR}/Utilities/gdcm/Utilities/C99
    )
  endif(MSVC)
endif(NOT ITK_USE_SYSTEM_GDCM)

# LIBXML2 include directories.
if(ITK_USE_LIBXML2)
  set(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_BINARY_DIR}/Utilities/itklibxml2
    )
endif(ITK_USE_LIBXML2)

# Patended include directories added only if the user explicitly enabled the
# ITK_USE_PATENTED option. Users are responsible for getting a license from the
# patent holders in order to use any of those methods.
if(ITK_USE_PATENTED)
  set(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_SOURCE_DIR}/Code/Patented
    )
endif(ITK_USE_PATENTED)


# Review include directories added only if the user explicitly enabled the
# ITK_USE_REVIEW option. Users are responsible for getting a license from the
# copyright holders in order to use any of those methods. The methods are not
# covered by the backward compatibility policy either.
if(ITK_USE_REVIEW)
  set(ITK_INCLUDE_DIRS_BUILD_TREE ${ITK_INCLUDE_DIRS_BUILD_TREE}
    ${ITK_SOURCE_DIR}/Code/Review
    )
endif(ITK_USE_REVIEW)

set(ITK_INCLUDE_RELATIVE_DIRS ${ITK_INCLUDE_RELATIVE_DIRS} Numerics/Statistics)



#-----------------------------------------------------------------------------
# Include directories needed for .cxx files in ITK.  These include
# directories will NOT be available to user projects.
set(ITK_INCLUDE_DIRS_BUILD_TREE_CXX)
if(ITK_USE_SYSTEM_VXL)
  set(ITK_INCLUDE_DIRS_BUILD_TREE_CXX ${ITK_INCLUDE_DIRS_BUILD_TREE_CXX}
    ${VXL_NETLIB_INCLUDE_DIR})
else(ITK_USE_SYSTEM_VXL)
  set(ITK_INCLUDE_DIRS_BUILD_TREE_CXX ${ITK_INCLUDE_DIRS_BUILD_TREE_CXX}
    ${ITK_SOURCE_DIR}/Utilities/vxl/v3p/netlib)
endif(ITK_USE_SYSTEM_VXL)

#-----------------------------------------------------------------------------
# Include directories from the install tree.
set(ITK_INSTALL_INCLUDE_PATH "${CMAKE_INSTALL_PREFIX}${ITK_INSTALL_INCLUDE_DIR}")
set(ITK_INCLUDE_RELATIVE_DIRS ${ITK_INCLUDE_RELATIVE_DIRS}
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

if(NOT ITK_USE_SYSTEM_VXL)
  set(ITK_INCLUDE_RELATIVE_DIRS ${ITK_INCLUDE_RELATIVE_DIRS}
    Utilities/vxl/v3p/netlib
    Utilities/vxl/vcl
    Utilities/vxl/core
    )
endif(NOT ITK_USE_SYSTEM_VXL)

if(NOT ITK_USE_SYSTEM_GDCM)
  set(ITK_INCLUDE_RELATIVE_DIRS ${ITK_INCLUDE_RELATIVE_DIRS}
    gdcm/Source/Common
    gdcm/Source/DataStructureAndEncodingDefinition
    gdcm/Source/MediaStorageAndFileFormat
    gdcm/Source/DataDictionary
    )
endif(NOT ITK_USE_SYSTEM_GDCM)

# Patended include directories added only if the user explicitly enabled the
# ITK_USE_PATENTED option. Users are responsible for getting a license from the
# patent holders in order to use any of those methods.
if(ITK_USE_PATENTED)
  set(ITK_INCLUDE_RELATIVE_DIRS ${ITK_INCLUDE_RELATIVE_DIRS}
    Patented
    )
endif(ITK_USE_PATENTED)

# Review include directories added only if the user explicitly enabled the
# ITK_USE_REVIEW option. Users are responsible for getting a license from the
# copyright holders in order to use any of those methods. The methods are not
# covered by the backward compatibility policy either.
if(ITK_USE_REVIEW)
  set(ITK_INCLUDE_RELATIVE_DIRS ${ITK_INCLUDE_RELATIVE_DIRS}
    Review
    )
endif(ITK_USE_REVIEW)


#-----------------------------------------------------------------------------
# Include directories for 3rd-party utilities provided by ITK.
ITK_THIRD_PARTY_INCLUDE(OpenJPEG openjpeg)
ITK_THIRD_PARTY_INCLUDE2(JPEG)
ITK_THIRD_PARTY_INCLUDE2(PNG)
ITK_THIRD_PARTY_INCLUDE2(TIFF)
ITK_THIRD_PARTY_INCLUDE2(ZLIB)
if(ITK_USE_SYSTEM_LIBXML2 OR ITK_USE_LIBXML2)
  ITK_THIRD_PARTY_INCLUDE(LIBXML2 libxml2)
endif(ITK_USE_SYSTEM_LIBXML2 OR ITK_USE_LIBXML2)
