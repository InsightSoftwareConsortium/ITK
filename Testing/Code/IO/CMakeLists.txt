INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/BasicFilters)
INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/IO)
INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Utilities/zlib)
INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Utilities/png)

LINK_LIBRARIES(ITKIO ITKCommon itkpng itkzlib)

LINK_DIRECTORIES(${ITK_BINARY_DIR}/Utilities/png)
LINK_DIRECTORIES(${ITK_BINARY_DIR}/Utilities/zlib)
LINK_DIRECTORIES(${ITK_BINARY_DIR}/Code/IO)

ADD_EXECUTABLE(itkIOHeaderTest itkIOHeaderTest.cxx)

SOURCE_FILES(IOTest_SRCS
itkConvertBufferTest
itkDicomImageIOTest
itkMetaImageIOTest
itkPNGImageIOTest
itkVOLImageIOTest
itkVTKImageIOTest
itkRawImageIOTest
itkRawImageIOTest3
itkRawImageIOTest4)
 
ADD_EXECUTABLE(itkIOTests itkIOTests.cxx IOTest_SRCS)


##Add tests that depend on finding data
FIND_PATH(VW_RAW_DATA_PATH
avfCROP.1023c.raw 
${ITK_SOURCE_DIR}/../InsightData/VisibleWomanHead/CROPraw)

IF(VW_RAW_DATA_PATH)
ADD_TEST(itkRawImageIOTest2 itkIOTests itkRawImageIOTest2 ${VW_RAW_DATA_PATH}/avfCROP.1023c.raw)
ADD_EXECUTABLE(itkRawImageIOTest2 itkRawImageIOTest2.cxx)
ENDIF(VW_RAW_DATA_PATH)

# Add VTK code if we found VTK
IF (HAS_VTK)
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
ENDIF (HAS_VTK)

IF (TCL_TCLSH)
ADD_TEST(PrintSelf-IO ${TCL_TCLSH}
         ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
         ${ITK_SOURCE_DIR}/Code/IO)
ENDIF (TCL_TCLSH)

