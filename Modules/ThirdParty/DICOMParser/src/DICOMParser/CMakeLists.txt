# The DICOMParser Library is intended to be included in other
# projects.  The library's namespace and library name can be
# configured.

# Typical usage is to import the DICOMParser directory tree into a
# subdirectory under a parent project and enable the library be used.
# The CMake listfile above this one configures the library as follows:
#
#  set(DICOMPARSER_NAMESPACE foodicomparser)
#  set(DICOMPARSER_LIBRARY   foodicomparser)
#  subdirs(DICOMParser)
#

#-----------------------------------------------------------------------------
# If a namespace is not specified, use "dicomparser". This should be
# the case only when dicom is not included inside another project
#
if(NOT DICOMPARSER_NAMESPACE)
   set(DICOMPARSER_NAMESPACE dicomparser)
   set(DICOMPARSER_LIBRARY dicomparser)
   set(DICOMPARSER_STANDALONE 1)
endif()


#-----------------------------------------------------------------------------
# The project name is that of the specified namespace.
project(${DICOMPARSER_NAMESPACE})

# Put the binary tree first in the include path to protect against
# when people accidently configure in their source tree
include_directories(${PROJECT_BINARY_DIR})
include_directories(${PROJECT_SOURCE_DIR})

if (WIN32)
  if (BUILD_SHARED_LIBS)
    set(DICOM_DLL 1)
  else ()
    set(DICOM_STATIC 1)
  endif ()
endif ()
set(DICOM_ANSI_STDLIB 1)

set(DICOM_NO_STD_NAMESPACE ${CMAKE_NO_STD_NAMESPACE})
include(${CMAKE_ROOT}/Modules/TestForSTDNamespace.cmake)

configure_file(${PROJECT_SOURCE_DIR}/DICOMCMakeConfig.h.in
               ${PROJECT_BINARY_DIR}/DICOMCMakeConfig.h)

add_library(${DICOMPARSER_LIBRARY} DICOMSource.cxx DICOMBuffer.cxx DICOMFile.cxx DICOMParser.cxx DICOMAppHelper.cxx)

if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(${DICOMPARSER_LIBRARY} PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif()

install(TARGETS ${DICOMPARSER_LIBRARY}
  EXPORT ${ITK3P_INSTALL_EXPORT_NAME}
  RUNTIME DESTINATION ${ITK3P_INSTALL_RUNTIME_DIR} COMPONENT RuntimeLibraries
  LIBRARY DESTINATION ${ITK3P_INSTALL_LIBRARY_DIR} COMPONENT RuntimeLibraries
  ARCHIVE DESTINATION ${ITK3P_INSTALL_ARCHIVE_DIR} COMPONENT Development)

file(GLOB __files1 "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
file(GLOB __files2 "${CMAKE_CURRENT_SOURCE_DIR}/*.txx")
install(FILES ${__files1} ${__files2} ${PROJECT_BINARY_DIR}/DICOMCMakeConfig.h
  DESTINATION ${ITK3P_INSTALL_INCLUDE_DIR}
  COMPONENT Development)
