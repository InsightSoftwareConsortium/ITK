INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/BasicFilters)
INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/IO)
INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Utilities/zlib)
INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Utilities/png)

LINK_LIBRARIES(ITKIO ITKCommon itkpng itkzlib)

LINK_DIRECTORIES(${ITK_BINARY_DIR}/Utilities/png)
LINK_DIRECTORIES(${ITK_BINARY_DIR}/Utilities/zlib)
LINK_DIRECTORIES(${ITK_BINARY_DIR}/Code/IO)

ADD_EXECUTABLE(itkConvertBufferTest itkConvertBufferTest)
ADD_EXECUTABLE(itkIOHeaderTest itkIOHeaderTest)
ADD_EXECUTABLE(itkMetaImageIOTest itkMetaImageIOTest)
ADD_EXECUTABLE(itkPNGImageIOTest itkPNGImageIOTest)
ADD_EXECUTABLE(itkVOLImageIOTest itkVOLImageIOTest)
ADD_EXECUTABLE(itkVTKImageIOTest itkVTKImageIOTest)
ADD_EXECUTABLE(itkRawImageIOTest itkRawImageIOTest)

##Add tests that depend on finding data
FIND_PATH(VW_RAW_DATA_PATH
avfCROP.1023c.raw 
${ITK_SOURCE_DIR}/../InsightData/VisibleWomanHead/CROPraw)

IF(VW_RAW_DATA_PATH)
ADD_TEST(itkRawImageIOTest2 itkRawImageIOTest2 ${VW_RAW_DATA_PATH}/avfCROP.1023c.raw)
ADD_EXECUTABLE(itkRawImageIOTest2 itkRawImageIOTest2)
ENDIF(VW_RAW_DATA_PATH)

# Add VTK code if we found VTK
INCLUDE (${ITK_SOURCE_DIR}/Utilities/ITKFindVTK.cmake)
IF (USE_VTK_FILE)
  ADD_DEFINITIONS(-DITK_HAS_VTK)
  INCLUDE(${USE_VTK_FILE})
  LINK_LIBRARIES( 
      vtkRendering
      vtkGraphics	  
      vtkImaging
      vtkIO
      vtkFiltering
      vtkCommon
  )
IF (VTKTIFF_BINARY_DIR)
  LINK_LIBRARIES( 
        vtktiff
  )
ENDIF (VTKTIFF_BINARY_DIR)
ENDIF (USE_VTK_FILE)

IF (TCL_TCLSH)
ADD_TEST(PrintSelf-IO ${TCL_TCLSH}
         ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
         ${ITK_SOURCE_DIR}/Code/IO)
ENDIF (TCL_TCLSH)

