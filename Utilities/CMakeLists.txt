# If any utility projects have their own testing targets,
# tell Dart.cmake to generate the Experimental test target with the
# project name.  This avoids the conflict in msdev between the
# Experimental testing targets for main ITK and nested projects.
set(DART_EXPERIMENTAL_USE_PROJECT_NAME 1)

# Create a header to hold the options for use of third party
# utilities.
configure_file(${ITK_SOURCE_DIR}/Utilities/itkThirdParty.h.in
               ${ITK_BINARY_DIR}/Utilities/itkThirdParty.h @ONLY IMMEDIATE)

             #include_directories(BEFORE ${ITK_BINARY_DIR}/Utilities)
             #include_directories(BEFORE ${ITK_SOURCE_DIR}/Utilities)

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
set(OPENJPEG_INCLUDE_DIR ${ITK_SOURCE_DIR}/Utilities/openjpeg CACHE FILEPATH "" FORCE)
# ZLIB
set(GDCM_USE_SYSTEM_ZLIB ON CACHE BOOL "" FORCE)
if(NOT ITK_USE_SYSTEM_ZLIB)
  set(ZLIB_INCLUDE_DIR ${ITK_SOURCE_DIR}/Utilities/itkzlib CACHE FILEPATH "" FORCE)
  set(ZLIB_LIBRARY itkzlib CACHE FILEPATH "" FORCE)
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

#
# Handle the special case of building libxml2
#  Fetch libxml2 using subversion if requested
if(ITK_USE_LIBXML2)
  if(NOT ITK_USE_SYSTEM_LIBXML2)
    if(EXISTS "${ITK_BINARY_DIR}/Utilities/itklibxml2/VERSION_2_6_27_1.txt")
      add_subdirectory( "${ITK_BINARY_DIR}/Utilities/itklibxml2" )
    else(EXISTS "${ITK_BINARY_DIR}/Utilities/itklibxml2/VERSION_2_6_27_1.txt")
      if(NOT SVNCOMMAND)
        if(Subversion_SVN_EXECUTABLE)
          set(SVNCOMMAND ${Subversion_SVN_EXECUTABLE})
        else(Subversion_SVN_EXECUTABLE)
          find_package(Subversion REQUIRED)
          if(Subversion_SVN_FOUND)
            set(SVNCOMMAND ${Subversion_SVN_EXECUTABLE})
          else(Subversion_SVN_FOUND)
            message(FATAL_ERROR "Subversion is required to install itklibxml2.  Point SVNCOMMAND to your svn executable.")
          endif(Subversion_SVN_FOUND)
        endif(Subversion_SVN_EXECUTABLE)
      endif(NOT SVNCOMMAND)
      set(svnCmd "co")
      set(svnRevFlag "-r")
      set(svnRevNum  "188")
      set(svnSite "https://www.kitware.com:8443/svn/KWPublic/trunk/libxml2")
      set(svnDir "${ITK_BINARY_DIR}/Utilities")
      file(WRITE ${ITK_BINARY_DIR}/Utilities/t "t\n")
      execute_process(
            COMMAND "${SVNCOMMAND}" ${svnCmd} ${svnRevFlag} ${svnRevNum} ${svnSite} ${svnDir}
            INPUT_FILE ${ITK_BINARY_DIR}/Utilities/t
            OUTPUT_VARIABLE svnOutput
            ERROR_VARIABLE svnError
            RESULT_VARIABLE svnResult
            )
      # Result should be 0 on success
      if(NOT svnResult)
        add_subdirectory( "${ITK_BINARY_DIR}/Utilities/itklibxml2"
                          "${ITK_BINARY_DIR}/Utilities/itklibxml2" )
      else(NOT svnResult)
        message(FATAL_ERROR "SVN checkout of itklibxml2 failed with error = ${svnError}")
      endif(NOT svnResult)
    endif(EXISTS "${ITK_BINARY_DIR}/Utilities/itklibxml2/VERSION_2_6_27_1.txt")
  else(NOT ITK_USE_SYSTEM_LIBXML2)
    message(FATAL_ERROR "You cannot enable both ITK_USE_SYSTEM_LIBXML2 and ITK_USE_LIBXML2.  Enable the SYSTEM variable if libxml2 is already installed outside of the Insight build directory.")
  endif(NOT ITK_USE_SYSTEM_LIBXML2)
endif(ITK_USE_LIBXML2)

#------------------------------------------------------------------------------
# Other utilities libraries
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
  if(ITK_USE_LIBXML2)
    install(FILES
      "${ITK_BINARY_DIR}/Utilities/itk_libxml2.h"
      DESTINATION "${ITK_INSTALL_INCLUDE_DIR_CM24_UTILITIES}"
      COMPONENT Development
      )
  endif(ITK_USE_LIBXML2)
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)
