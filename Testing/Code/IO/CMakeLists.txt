SET(KIT IO)
CREATE_TEST_SOURCELIST(Tests itk${KIT}Tests.cxx 
  itkConvertBufferTest.cxx
  itkDicomImageIOTest.cxx
  itkAnalyzeImageIOTest.cxx
  itkGiplImageIOTest.cxx
  itkMetaImageIOTest.cxx
  itkPNGImageIOTest.cxx
  itkVOLImageIOTest.cxx
  itkVTKImageIOTest.cxx
  itkRawImageIOTest.cxx
  itkRawImageIOTest2.cxx
  itkRawImageIOTest3.cxx
  itkRawImageIOTest4.cxx
)

ADD_EXECUTABLE(itk${KIT}Tests ${Tests})
TARGET_LINK_LIBRARIES(itk${KIT}Tests ITKIO)

SET (TestsToRun ${Tests})
REMOVE (TestsToRun itk${KIT}Tests.cxx)

FOREACH (test ${TestsToRun})
  GET_FILENAME_COMPONENT(TName ${test} NAME_WE)
  #ADD_TEST(${TName} ${CXX_TEST_PATH}/itk${KIT}Tests ${TName}) 
ENDFOREACH (test)

# Add PrintSelf test.
IF (TCL_TCLSH)
  ADD_TEST(PrintSelf-${KIT} ${TCL_TCLSH}
           ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
           ${ITK_SOURCE_DIR}/Code/${KIT})
ENDIF (TCL_TCLSH)

# Add header test.
ADD_EXECUTABLE(itk${KIT}HeaderTest itk${KIT}HeaderTest.cxx)
TARGET_LINK_LIBRARIES(itk${KIT}HeaderTest ITKIO)
ADD_TEST(itk${KIT}HeaderTest ${CXX_TEST_PATH}/itk${KIT}HeaderTest)

##Add tests that depend on finding data
FIND_PATH(VW_RAW_DATA_PATH avfCROP.1023c.raw 
          ${ITK_SOURCE_DIR}/../InsightData/VisibleWomanHead/CROPraw)
MARK_AS_ADVANCED(VW_RAW_DATA_PATH)

IF(VW_RAW_DATA_PATH)
  ADD_TEST(itkRawImageIOTest2 ${IO_TESTS} itkRawImageIOTest2
           ${VW_RAW_DATA_PATH}/avfCROP.1023c.raw)
ENDIF(VW_RAW_DATA_PATH)

# Add VTK code if we found VTK
IF (HAS_VTK)
  ADD_DEFINITIONS(-DITK_HAS_VTK)
  TARGET_LINK_LIBRARIES( itkIOTests
      vtkRendering
      vtkGraphics         
      vtkImaging
      vtkIO
      vtkFiltering
      vtkCommon
  )
IF (VTKTIFF_BINARY_DIR)
  TARGET_LINK_LIBRARIES( itkIOTests
        vtktiff
  )
ENDIF (VTKTIFF_BINARY_DIR)
ENDIF (HAS_VTK)
