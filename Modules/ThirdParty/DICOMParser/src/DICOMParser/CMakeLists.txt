# The DICOMParser Library is intended to be included in other
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
IF(NOT DICOMPARSER_NAMESPACE)
   SET(DICOMPARSER_NAMESPACE dicomparser) 
   SET(DICOMPARSER_LIBRARY dicomparser)
   SET(DICOMPARSER_STANDALONE 1)
ENDIF(NOT DICOMPARSER_NAMESPACE)


#-----------------------------------------------------------------------------
# The project name is that of the specified namespace.
PROJECT(${DICOMPARSER_NAMESPACE})

# Put the binary tree first in the include path to protect against
# when people accidently configure in their source tree
INCLUDE_DIRECTORIES(${PROJECT_BINARY_DIR})
INCLUDE_DIRECTORIES(${PROJECT_SOURCE_DIR})

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

INCLUDE(${CMAKE_ROOT}/Modules/TestForSTDNamespace.cmake)
SET(DICOM_NO_STD_NAMESPACE ${CMAKE_NO_STD_NAMESPACE})


CONFIGURE_FILE(${PROJECT_SOURCE_DIR}/DICOMCMakeConfig.h.in
               ${PROJECT_BINARY_DIR}/DICOMCMakeConfig.h)

ADD_LIBRARY(${DICOMPARSER_LIBRARY} DICOMSource.cxx DICOMBuffer.cxx DICOMFile.cxx DICOMParser.cxx DICOMAppHelper.cxx)

IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(${DICOMPARSER_LIBRARY} PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

INSTALL(TARGETS ${DICOMPARSER_LIBRARY}
  EXPORT ${ITK3P_INSTALL_EXPORT_NAME}
  RUNTIME DESTINATION ${ITK3P_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
  LIBRARY DESTINATION ${ITK3P_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
  ARCHIVE DESTINATION ${ITK3P_INSTALL_ARCHIVE_DIR} COMPONENT Development)

FILE(GLOB __files1 "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
FILE(GLOB __files2 "${CMAKE_CURRENT_SOURCE_DIR}/*.txx")
INSTALL(FILES ${__files1} ${__files2} ${PROJECT_BINARY_DIR}/DICOMCMakeConfig.h
  DESTINATION ${ITK3P_INSTALL_INCLUDE_DIR}
  COMPONENT Development)
