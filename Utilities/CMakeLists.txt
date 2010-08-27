# If any utility projects have their own testing targets,
# tell Dart.cmake to generate the Experimental test target with the
# project name.  This avoids the conflict in msdev between the
# Experimental testing targets for main ITK and nested projects.
SET(DART_EXPERIMENTAL_USE_PROJECT_NAME 1)

# Create a header to hold the options for use of third party
# utilities.
CONFIGURE_FILE(${ITK_SOURCE_DIR}/Utilities/itkThirdParty.h.in
               ${ITK_BINARY_DIR}/Utilities/itkThirdParty.h @ONLY IMMEDIATE)

#-----------------------------------------------------------------------------
# Create the kwsys library for ITK.
SET(KWSYS_NAMESPACE itksys)
SET(KWSYS_USE_SystemTools 1)
SET(KWSYS_USE_RegularExpression 1)
SET(KWSYS_USE_Directory 1)
SET(KWSYS_USE_Base64 1)
SET(KWSYS_USE_FundamentalType 1)
SET(KWSYS_USE_CommandLineArguments 1)
SET(KWSYS_USE_Process 1)
SET(KWSYS_USE_DynamicLoader 1)
SET(KWSYS_USE_Glob 1)
SET(KWSYS_USE_Registry 1)
SET(KWSYS_USE_SystemInformation 1)
SET(KWSYS_HEADER_ROOT ${ITK_BINARY_DIR}/Utilities)
SET(KWSYS_PROPERTIES_CXX ${ITK_LIBRARY_PROPERTIES})
SET(KWSYS_LIBRARY_INSTALL_DIR)
IF(NOT ITK_INSTALL_NO_LIBRARIES)
  SET(KWSYS_LIBRARY_INSTALL_DIR ${ITK_INSTALL_LIB_DIR})
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)
SET(KWSYS_HEADER_INSTALL_DIR)
IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  SET(KWSYS_HEADER_INSTALL_DIR ${ITK_INSTALL_INCLUDE_DIR}/Utilities)
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)
SET(KWSYS_INSTALL_COMPONENT_NAME_RUNTIME RuntimeLibraries)
SET(KWSYS_INSTALL_COMPONENT_NAME_DEVELOPMENT Development)
ADD_SUBDIRECTORY(kwsys)

#------------------------------------------------------------------------------
# Build third-party utilities.

ITK_THIRD_PARTY_SUBDIR(ZLIB itkzlib)
ITK_THIRD_PARTY_SUBDIR(PNG  itkpng)
# Build jpeg first because tiff needs it
ITK_THIRD_PARTY_SUBDIR(JPEG itkjpeg)
ITK_THIRD_PARTY_SUBDIR(TIFF itktiff)
SET(OPJ_PREFIX "itk")
ITK_THIRD_PARTY_SUBDIR(OpenJPEG openjpeg)

# GDCM needs expat
ADD_SUBDIRECTORY(expat)

SET(GDCM_TARGETS_NAME ITKTargets)
SET(GDCM_BUILD_TESTING_DEFAULT OFF)
SET(GDCM_BUILD_SHARED_LIBS ${BUILD_SHARED_LIBS} CACHE BOOL "" FORCE)
SET(GDCM_USE_SYSTEM_EXPAT ON CACHE BOOL "" FORCE)
# ITKEXPAT can not be a system one:
SET(EXPAT_INCLUDE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/expat" CACHE PATH "" FORCE)
SET(EXPAT_LIBRARY ITKEXPAT CACHE FILEPATH "" FORCE)
# ITKOpenJPEG can not be a system one:
SET(GDCM_USE_SYSTEM_OPENJPEG ON CACHE BOOL "" FORCE)
SET(OPENJPEG_MAJOR_VERSION 2)
# FIXME: Simply fill in values from ITK
SET(OPENJPEG_LIBRARY itkopenjpeg CACHE FILEPATH "" FORCE)
SET(OPENJPEG_INCLUDE_DIR ${ITK_SOURCE_DIR}/Utilities/openjpeg CACHE FILEPATH "" FORCE)
# ZLIB
SET(GDCM_USE_SYSTEM_ZLIB ON CACHE BOOL "" FORCE)
IF(NOT ITK_USE_SYSTEM_ZLIB)
  SET(ZLIB_INCLUDE_DIR ${ITK_SOURCE_DIR}/Utilities/itkzlib CACHE FILEPATH "" FORCE)
  SET(ZLIB_LIBRARY itkzlib CACHE FILEPATH "" FORCE)
ENDIF()
ITK_THIRD_PARTY_SUBDIR(GDCM gdcm)
MARK_AS_ADVANCED(
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

#
# Handle the special case of building libxml2
#  Fetch libxml2 using subversion if requested
IF(ITK_USE_LIBXML2)
  IF(NOT ITK_USE_SYSTEM_LIBXML2)
    IF(EXISTS "${ITK_BINARY_DIR}/Utilities/itklibxml2/VERSION_2_6_27_1.txt")
      SUBDIRS( "${ITK_BINARY_DIR}/Utilities/itklibxml2" )
    ELSE(EXISTS "${ITK_BINARY_DIR}/Utilities/itklibxml2/VERSION_2_6_27_1.txt")
      IF(NOT SVNCOMMAND)
        IF(Subversion_SVN_EXECUTABLE)
          SET(SVNCOMMAND ${Subversion_SVN_EXECUTABLE})
        ELSE(Subversion_SVN_EXECUTABLE)
          FIND_PACKAGE(Subversion REQUIRED)
          IF(Subversion_SVN_FOUND)
            SET(SVNCOMMAND ${Subversion_SVN_EXECUTABLE})
          ELSE(Subversion_SVN_FOUND)
            MESSAGE(FATAL_ERROR "Subversion is required to install itklibxml2.  Point SVNCOMMAND to your svn executable.")
          ENDIF(Subversion_SVN_FOUND)
        ENDIF(Subversion_SVN_EXECUTABLE)
      ENDIF(NOT SVNCOMMAND)
      SET(svnCmd "co")
      SET(svnRevFlag "-r")
      SET(svnRevNum  "188")
      SET(svnSite "https://www.kitware.com:8443/svn/KWPublic/trunk/libxml2")
      SET(svnDir "${ITK_BINARY_DIR}/Utilities")
      FILE(WRITE ${ITK_BINARY_DIR}/Utilities/t "t\n")
      EXECUTE_PROCESS(
            COMMAND "${SVNCOMMAND}" ${svnCmd} ${svnRevFlag} ${svnRevNum} ${svnSite} ${svnDir} 
            INPUT_FILE ${ITK_BINARY_DIR}/Utilities/t
            OUTPUT_VARIABLE svnOutput
            ERROR_VARIABLE svnError
            RESULT_VARIABLE svnResult
            )
      # Result should be 0 on success
      IF(NOT svnResult)
        ADD_SUBDIRECTORY( "${ITK_BINARY_DIR}/Utilities/itklibxml2" 
                          "${ITK_BINARY_DIR}/Utilities/itklibxml2" )
      ELSE(NOT svnResult)
        MESSAGE(FATAL_ERROR "SVN checkout of itklibxml2 failed with error = ${svnError}")
      ENDIF(NOT svnResult)
    ENDIF(EXISTS "${ITK_BINARY_DIR}/Utilities/itklibxml2/VERSION_2_6_27_1.txt")
  ELSE(NOT ITK_USE_SYSTEM_LIBXML2)
    MESSAGE(FATAL_ERROR "You cannot enable both ITK_USE_SYSTEM_LIBXML2 and ITK_USE_LIBXML2.  Enable the SYSTEM variable if libxml2 is already installed outside of the Insight build directory.")
  ENDIF(NOT ITK_USE_SYSTEM_LIBXML2)
ENDIF(ITK_USE_LIBXML2)

#------------------------------------------------------------------------------
# Other utilities libraries
ADD_SUBDIRECTORY(MetaIO)
ADD_SUBDIRECTORY(NrrdIO)
ADD_SUBDIRECTORY(nifti)
ADD_SUBDIRECTORY(itkExtHdrs)
ADD_SUBDIRECTORY(KWStyle)

#------------------------------------------------------------------------------
# Create the DICOMParse library for ITK
SET(DICOMPARSER_NAMESPACE itkdicomparser)
SET(DICOMPARSER_LIBRARY ITKDICOMParser)
SET(DICOMPARSER_STANDALONE 0)
ADD_SUBDIRECTORY(DICOMParser)

IF(ITK_DISABLE_CXX_TESTING)
  SET(BUILD_TESTING OFF)
ENDIF(ITK_DISABLE_CXX_TESTING)

#------------------------------------------------------------------------------
# Configure vxl/vnl as needed
IF(NOT ITK_USE_SYSTEM_VXL)
  ADD_SUBDIRECTORY(vxl)
ENDIF(NOT ITK_USE_SYSTEM_VXL)

#------------------------------------------------------------------------------
# This directory depends on VXL V3p
ADD_SUBDIRECTORY(netlib)

#------------------------------------------------------------------------------
# Build testing as needed
IF (BUILD_TESTING)
  ADD_SUBDIRECTORY(Dart)
ENDIF (BUILD_TESTING)

#-----------------------------------------------------------------------------
# ITK build configuration options.
IF(BUILD_DOXYGEN)
  ADD_SUBDIRECTORY(Doxygen)
ENDIF(BUILD_DOXYGEN)

#-----------------------------------------------------------------------------
# Install the UseITK.cmake file configured from the top-level.
IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  INSTALL(FILES ${ITK_BINARY_DIR}/Utilities/ITKConfig.cmake
    DESTINATION ${ITK_INSTALL_PACKAGE_DIR_CM24}
    COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)

#-----------------------------------------------------------------------------

IF(ITK_USE_FLAT_DIRECTORY_INSTALL)
  SET( ITK_INSTALL_INCLUDE_DIR_CM24_UTILITIES ${ITK_INSTALL_INCLUDE_DIR_CM24})
ELSE(ITK_USE_FLAT_DIRECTORY_INSTALL)
  SET( ITK_INSTALL_INCLUDE_DIR_CM24_UTILITIES ${ITK_INSTALL_INCLUDE_DIR_CM24}/Utilities)
ENDIF(ITK_USE_FLAT_DIRECTORY_INSTALL)


IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  INSTALL(FILES
    ${ITK_BINARY_DIR}/Utilities/itkThirdParty.h
    ${ITK_SOURCE_DIR}/Utilities/itk_zlib.h
    ${ITK_SOURCE_DIR}/Utilities/itk_tiff.h
    ${ITK_SOURCE_DIR}/Utilities/itk_jpeg.h
    ${ITK_SOURCE_DIR}/Utilities/itk_png.h
    ${ITK_SOURCE_DIR}/Utilities/metaIOConfig.h
    ${ITK_BINARY_DIR}/Utilities/NrrdIO/NrrdConfigure.h
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24_UTILITIES}
    COMPONENT Development
    )
  IF(ITK_USE_LIBXML2)
    INSTALL(FILES
      "${ITK_BINARY_DIR}/Utilities/itk_libxml2.h"
      DESTINATION "${ITK_INSTALL_INCLUDE_DIR_CM24_UTILITIES}"
      COMPONENT Development
      )
  ENDIF(ITK_USE_LIBXML2)
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)
