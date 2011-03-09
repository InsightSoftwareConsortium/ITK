# If any utility projects have their own testing targets,
# tell Dart.cmake to generate the Experimental test target with the
# project name.  This avoids the conflict in msdev between the
# Experimental testing targets for main ITK and nested projects.
set(DART_EXPERIMENTAL_USE_PROJECT_NAME 1)

# Create a header to hold the options for use of third party
# utilities.
foreach(h
    itk_jpeg.h
    itk_png.h
    itk_tiff.h
    itk_zlib.h
    )
  configure_file(${ITK_SOURCE_DIR}/Utilities/${h}.in
                 ${ITK_BINARY_DIR}/Utilities/${h} @ONLY IMMEDIATE)
endforeach()

#-----------------------------------------------------------------------------
# Create the kwsys library for ITK.
set(KWSYS_NAMESPACE itksys)
set(KWSYS_USE_SystemTools 1)
set(KWSYS_USE_RegularExpression 1)
set(KWSYS_USE_Directory 1)
set(KWSYS_USE_Base64 1)
set(KWSYS_USE_FundamentalType 1)
set(KWSYS_USE_CommandLineArguments 1)
set(KWSYS_USE_Process 1)
set(KWSYS_USE_DynamicLoader 1)
set(KWSYS_USE_Glob 1)
set(KWSYS_USE_Registry 1)
set(KWSYS_USE_SystemInformation 1)
set(KWSYS_HEADER_ROOT ${ITK_BINARY_DIR}/Utilities)
set(KWSYS_PROPERTIES_CXX ${ITK_LIBRARY_PROPERTIES})
set(KWSYS_LIBRARY_INSTALL_DIR)
if(NOT ITK_INSTALL_NO_LIBRARIES)
  set(KWSYS_LIBRARY_INSTALL_DIR ${ITK_INSTALL_LIB_DIR})
endif(NOT ITK_INSTALL_NO_LIBRARIES)
set(KWSYS_HEADER_INSTALL_DIR)
if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  set(KWSYS_HEADER_INSTALL_DIR ${ITK_INSTALL_INCLUDE_DIR}/Utilities)
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)
set(KWSYS_INSTALL_COMPONENT_NAME_RUNTIME RuntimeLibraries)
set(KWSYS_INSTALL_COMPONENT_NAME_DEVELOPMENT Development)
add_subdirectory(kwsys)

#------------------------------------------------------------------------------
# Build third-party utilities.


ITK_THIRD_PARTY_SUBDIR(ZLIB itkzlib)
ITK_THIRD_PARTY_SUBDIR(PNG  itkpng)
# Build jpeg first because tiff needs it
ITK_THIRD_PARTY_SUBDIR(JPEG itkjpeg)
ITK_THIRD_PARTY_SUBDIR(TIFF itktiff)
set(OPJ_PREFIX "itk")
ITK_THIRD_PARTY_SUBDIR(OpenJPEG openjpeg)

# GDCM needs expat
add_subdirectory(expat)

set(GDCM_TARGETS_NAME ITKTargets)
set(GDCM_BUILD_TESTING_DEFAULT OFF)
set(GDCM_BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS} CACHE BOOL "" FORCE)
set(GDCM_USE_SYSTEM_EXPAT ON CACHE BOOL "" FORCE)
# ITKEXPAT can not be a system one:
set(EXPAT_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/expat" CACHE PATH "" FORCE)
set(EXPAT_LIBRARY ITKEXPAT CACHE FILEPATH "" FORCE)
# ITKOpenJPEG can not be a system one:
set(GDCM_USE_SYSTEM_OPENJPEG ON CACHE BOOL "" FORCE)
set(OPENJPEG_MAJOR_VERSION 2)
# FIXME: Simply fill in values from ITK
set(OPENJPEG_LIBRARY itkopenjpeg CACHE FILEPATH "" FORCE)
SET(OPENJPEG_LIBRARIES ${OPENJPEG_LIBRARY})
set(OPENJPEG_INCLUDE_DIR ${ITK_SOURCE_DIR}/Utilities/openjpeg CACHE FILEPATH "" FORCE)
set(OPENJPEG_INCLUDE_DIRS ${OPENJPEG_INCLUDE_DIR})
# ZLIB
set(GDCM_USE_SYSTEM_ZLIB ON CACHE BOOL "" FORCE)
if(NOT ITK_USE_SYSTEM_ZLIB)
  set(ZLIB_INCLUDE_DIR ${ITK_SOURCE_DIR}/Utilities/itkzlib)
  set(ZLIB_LIBRARY itkzlib)
endif()
ITK_THIRD_PARTY_SUBDIR(GDCM gdcm)
mark_as_advanced(
  GDCM_BUILD_APPLICATIONS
  GDCM_BUILD_EXAMPLES
  GDCM_BUILD_SHARED_LIBS
  GDCM_BUILD_TESTING
  GDCM_DOCUMENTATION
  GDCM_USE_VTK
  GDCM_WRAP_CSHARP
  GDCM_WRAP_JAVA
  GDCM_WRAP_PYTHON
  OpenJPEG_DIR
  )

#------------------------------------------------------------------------------
# Other utilities libraries
set(METAIO_FOR_ITK 1)
add_subdirectory(MetaIO)
add_subdirectory(NrrdIO)
add_subdirectory(nifti)
add_subdirectory(itkExtHdrs)
add_subdirectory(KWStyle)

#------------------------------------------------------------------------------
# Create the DICOMParse library for ITK
set(DICOMPARSER_NAMESPACE itkdicomparser)
set(DICOMPARSER_LIBRARY ITKDICOMParser)
set(DICOMPARSER_STANDALONE 0)
add_subdirectory(DICOMParser)

if(ITK_DISABLE_CXX_TESTING)
  set(BUILD_TESTING OFF)
endif(ITK_DISABLE_CXX_TESTING)

#------------------------------------------------------------------------------
# Configure vxl/vnl as needed
if(NOT ITK_USE_SYSTEM_VXL)
  add_subdirectory(vxl)
endif(NOT ITK_USE_SYSTEM_VXL)

#------------------------------------------------------------------------------
# This directory depends on VXL V3p
add_subdirectory(netlib)

#------------------------------------------------------------------------------
# Build testing as needed
if(BUILD_TESTING)
  add_subdirectory(Dart)
endif(BUILD_TESTING)

#-----------------------------------------------------------------------------
# ITK build configuration options.
if(BUILD_DOXYGEN)
  add_subdirectory(Doxygen)
endif(BUILD_DOXYGEN)

#-----------------------------------------------------------------------------
# Install the UseITK.cmake file configured from the top-level.
if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  install(FILES ${ITK_BINARY_DIR}/Utilities/ITKConfig.cmake
    DESTINATION ${ITK_INSTALL_PACKAGE_DIR_CM24}
    COMPONENT Development)
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)

#-----------------------------------------------------------------------------

if(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_UTILITIES ${ITK_INSTALL_INCLUDE_DIR_CM24})
else(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_UTILITIES ${ITK_INSTALL_INCLUDE_DIR_CM24}/Utilities)
endif(ITK_USE_FLAT_DIRECTORY_INSTALL)


if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  install(FILES
    ${ITK_BINARY_DIR}/Utilities/itk_zlib.h
    ${ITK_BINARY_DIR}/Utilities/itk_tiff.h
    ${ITK_BINARY_DIR}/Utilities/itk_jpeg.h
    ${ITK_BINARY_DIR}/Utilities/itk_png.h
    ${ITK_SOURCE_DIR}/Utilities/metaIOConfig.h
    ${ITK_BINARY_DIR}/Utilities/NrrdIO/NrrdConfigure.h
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24_UTILITIES}
    COMPONENT Development
    )
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)
