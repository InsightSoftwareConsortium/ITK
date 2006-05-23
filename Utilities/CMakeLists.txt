# If any utility projects have their own testing targets,
# tell Dart.cmake to generate the Experimental test target with the
# project name.  This avoids the conflict in msdev between the
# Experimental testing targets for main ITK and nested projects.
SET(DART_EXPERIMENTAL_USE_PROJECT_NAME 1)

# Build jpeg first because tiff needs it
SUBDIRS(itkjpeg)

#------------------------------------------------------------------------------
# Build third-party utilities.
ITK_THIRD_PARTY_SUBDIR(ZLIB zlib)
ITK_THIRD_PARTY_SUBDIR(PNG  png)
ITK_THIRD_PARTY_SUBDIR(TIFF tiff)
SET(OPJ_PREFIX "itk")
ITK_THIRD_PARTY_SUBDIR(OpenJPEG openjpeg)
ITK_THIRD_PARTY_SUBDIR(GDCM gdcm)
#------------------------------------------------------------------------------
# Other utilities libraries
SUBDIRS(MetaIO)
SUBDIRS(NrrdIO)
SUBDIRS(expat)
SUBDIRS(nifti)

#------------------------------------------------------------------------------
# Create the DICOMParse library for ITK
SET(DICOMPARSER_NAMESPACE itkdicomparser)
SET(DICOMPARSER_LIBRARY ITKDICOMParser)
SET(DICOMPARSER_STANDALONE 0)
SUBDIRS(DICOMParser)

IF(ITK_DISABLE_CXX_TESTING)
  SET(BUILD_TESTING OFF)
ENDIF(ITK_DISABLE_CXX_TESTING)

#------------------------------------------------------------------------------
# Configure vxl/vnl as needed
IF(NOT ITK_USE_SYSTEM_VXL)
  SUBDIRS(vxl)
ENDIF(NOT ITK_USE_SYSTEM_VXL)

#------------------------------------------------------------------------------
# Build testing as needed
IF (BUILD_TESTING)
  SUBDIRS(Dart)
ENDIF (BUILD_TESTING)

#-----------------------------------------------------------------------------
# Create the kwsys library for ITK.
SET(KWSYS_NAMESPACE itksys)
SET(KWSYS_USE_SystemTools 1)
SET(KWSYS_USE_RegularExpression 1)
SET(KWSYS_USE_Directory 1)
SET(KWSYS_USE_Base64 1)
SET(KWSYS_USE_FundamentalType 1)
SET(KWSYS_USE_CommandLineArguments 1)
SET(KWSYS_HEADER_ROOT ${ITK_BINARY_DIR}/Utilities)
SET(KWSYS_LIBRARY_INSTALL_DIR /lib/InsightToolkit)
SET(KWSYS_HEADER_INSTALL_DIR /include/InsightToolkit/Utilities)
SET(KWSYS_PROPERTIES_CXX ${ITK_LIBRARY_PROPERTIES})
SUBDIRS(kwsys)

#-----------------------------------------------------------------------------
# Build CableSwig if necessary.
IF(ITK_BUILD_CABLESWIG)
  SUBDIRS(CableSwig)
ENDIF(ITK_BUILD_CABLESWIG)


#-----------------------------------------------------------------------------
# Install the UseITK.cmake file configured from the top-level.
INSTALL_FILES(/lib/InsightToolkit ".cmake" "ITKConfig")

