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
SET(DICOMPARSER_NAMESPACE dicomparser)
SET(DICOMPARSER_LIBRARY dicomparser)
SET(DICOMPARSER_STANDALONE 1)


IF(WIN32)
  IF(BUILD_SHARED_LIBS)
    SET(DICOM_DLL 1)
  ELSE(BUILD_SHARED_LIBS)
    SET(DICOM_STATIC 1)
  ENDIF(BUILD_SHARED_LIBS)
ENDIF(WIN32)

IF(ITK_SOURCE_DIR)
   # Inside ITK, use its setting.
   SET(DICOM_ANSI_STDLIB 1)
ELSE(ITK_SOURCE_DIR)
   # Not inside ITK, use ANSI streams.
   SET(DICOM_ANSI_STDLIB 1)
ENDIF(ITK_SOURCE_DIR)


set(ITK-DICOMParser_SRC
DICOMAppHelper.cxx
DICOMBuffer.cxx
DICOMFile.cxx
DICOMParser.cxx
DICOMSource.cxx
)

add_library(ITK-DICOMParser ${ITK-DICOMParser_SRC})
target_link_libraries(ITK-DICOMParser )
itk_module_target(ITK-DICOMParser)
