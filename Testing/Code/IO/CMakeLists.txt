
ADD_EXECUTABLE(itkIOHeaderTest itkIOHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkIOHeaderTest ITKIO)

IF( CXX_TEST_PATH )
  SET(IO_TESTS ${CXX_TEST_PATH}/itkIOTests)
ELSE( CXX_TEST_PATH )
  SET(IO_TESTS itkIOTests)
ENDIF( CXX_TEST_PATH )

SET(IOTest_SRCS
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

ADD_EXECUTABLE(itkIOTests itkIOTests.cxx ${IOTest_SRCS})
TARGET_LINK_LIBRARIES(itkIOTests ITKIO)


##Add tests that depend on finding data
FIND_PATH(VW_RAW_DATA_PATH
avfCROP.1023c.raw 
${ITK_SOURCE_DIR}/../InsightData/VisibleWomanHead/CROPraw)

IF(VW_RAW_DATA_PATH)
ADD_TEST(itkRawImageIOTest2 ${IO_TESTS} itkRawImageIOTest2 ${VW_RAW_DATA_PATH}/avfCROP.1023c.raw)
ENDIF(VW_RAW_DATA_PATH)

IF (TCL_TCLSH)
ADD_TEST(PrintSelf-IO ${TCL_TCLSH}
         ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
         ${ITK_SOURCE_DIR}/Code/IO)
ENDIF (TCL_TCLSH)

