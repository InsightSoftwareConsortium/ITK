cmake_minimum_required(VERSION 2.6)

PROJECT(AnalyzeObjectMapExamples)

FIND_PACKAGE(ITK REQUIRED)
IF(ITK_FOUND)
  INCLUDE(${ITK_USE_FILE})
ENDIF(ITK_FOUND)

#FIND_PACKAGE( VTK REQUIRED )
#IF ( VTK_FOUND )
#    INCLUDE( ${USE_VTK_FILE} )
#ENDIF( VTK_FOUND )


INCLUDE_DIRECTORIES(${AnalyzeObjectMap_SOURCE_DIR})
INCLUDE_DIRECTORIES(${AnalyzeObjectMapIO_SOURCE_DIR})

SET(TEST_DATA_ROOT     ${AnalyzeObjectMapExamples_SOURCE_DIR}/Data/Input)
SET(TESTING_OUTPUT_DIR ${AnalyzeObjectMapExamples_BINARY_DIR}/Temporary)

INCLUDE_DIRECTORIES( ${AnalyzeObjectMapExamples_SOURCE_DIR} )

ADD_EXECUTABLE( CreatingObjects CreatingObjects.cxx )
TARGET_LINK_LIBRARIES( CreatingObjects ITKIO AnalyzeObjectMapIO AnalyzeObjectMap)

ADD_EXECUTABLE( DisplayingObjectMaps DisplayingObjectMaps.cxx )
TARGET_LINK_LIBRARIES( DisplayingObjectMaps ITKIO AnalyzeObjectMapIO AnalyzeObjectMap) #vtkRendering vtkImaging vtkIO vtkCommon)

ADD_EXECUTABLE( PickOneObjectEntry PickOneObjectEntry.cxx )
TARGET_LINK_LIBRARIES( PickOneObjectEntry ITKIO AnalyzeObjectMapIO AnalyzeObjectMap)


