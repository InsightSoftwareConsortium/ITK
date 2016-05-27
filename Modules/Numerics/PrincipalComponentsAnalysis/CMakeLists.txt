CMAKE_MINIMUM_REQUIRED(VERSION 2.9)
IF(COMMAND CMAKE_POLICY)
  CMAKE_POLICY(SET CMP0003 NEW)
ENDIF(COMMAND CMAKE_POLICY)

SET(kit PrincipalComponentsAnalysis)
PROJECT(${kit})

IF (NOT ITK_SOURCE_DIR)
  FIND_PACKAGE(ITK REQUIRED)
  LIST(APPEND CMAKE_MODULE_PATH ${ITK_CMAKE_DIR})
  INCLUDE(ITKModuleExternal)
ELSE()
  itk_module_impl()
ENDIF()

OPTION(${kit}_BUILD_EXAMPLES "Build the examples" OFF)
IF(${kit}_BUILD_EXAMPLES)
  ADD_SUBDIRECTORY( Examples )
ENDIF()


OPTION(${kit}_GENERATE_REPORT "Generate the PDF report from latex files, source code examples and result screenshots" OFF)
IF(${kit}_GENERATE_REPORT)
  ADD_SUBDIRECTORY( Documents )
ENDIF()
