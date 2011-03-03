#The DICOMParser Library is intended to be included in other
 # projects.  The library's namespace and library name can be
 # configured.

 # Typical usage is to import the DICOMParser directory tree into a
 # subdirectory under a parent project and enable the library be used.
 # The CMake listfile above this one configures the library as follows:
 #
 #  SET(DICOMPARSER_NAMESPACE foodicomparser)
 #  SET(DICOMPARSER_LIBRARY   foodicomparser)
 #  SUBDIRS(DICOMParser)
 #

 #-----------------------------------------------------------------------------
 # If a namespace is not specified, use "dicomparser". This should be
 # the case only when dicom is not included inside another project
 #
SET(DICOMPARSER_LIBRARY itkdicomparser)
SET(DICOMPARSER_STANDALONE 1)

#-----------------------------------------------------------------------------
# The project name is that of the specified namespace.

IF(WIN32)
  IF(BUILD_SHARED_LIBS)
    SET(DICOM_DLL 1)
  ELSE(BUILD_SHARED_LIBS)
    SET(DICOM_STATIC 1)
  ENDIF(BUILD_SHARED_LIBS)
ENDIF(WIN32)

#INCLUDE(${CMAKE_ROOT}/Modules/TestForSTDNamespace.cmake)
#SET(DICOM_NO_STD_NAMESPACE ${CMAKE_NO_STD_NAMESPACE})

# Inside ITK, use its setting.
SET(DICOM_ANSI_STDLIB 1)
configure_file(DICOMCMakeConfig.h.in ${ITK-DICOMParser_BINARY_DIR}/DICOMCMakeConfig.h)
set(ITK-DICOMParser_SRC
DICOMAppHelper.cxx
DICOMBuffer.cxx
DICOMFile.cxx
DICOMParser.cxx
DICOMSource.cxx
)

add_library(${DICOMPARSER_LIBRARY}  ${ITK-DICOMParser_SRC})
itk_module_target(${DICOMPARSER_LIBRARY})

install(FILES ${ITK-DICOMParser_BINARY_DIR}/DICOMCMakeConfig.h
DESTINATION ${ITK-DICOMParser_INSTALL_INCLUDE_DIR})
