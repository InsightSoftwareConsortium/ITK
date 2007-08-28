IF( NOT ITK_DISABLE_CXX_TESTING )
IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-Patented ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/Patented)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)


SET(PATENTED_TESTS ${CXX_TEST_PATH}/itkPatentedTests)

SET(PATENTED_PRINT_TEST ${CXX_TEST_PATH}/itkPatentedPrintTest)

SET(PATENTED_HEADER_TESTS ${CXX_TEST_PATH}/itkPatentedHeaderTest)

ADD_TEST(itkPatentedPrintTest ${PATENTED_PRINT_TEST})

ADD_TEST(itkSimpleFuzzyConnectednessScalarImageFilterTest ${PATENTED_TESTS} itkSimpleFuzzyConnectednessScalarImageFilterTest)
ADD_TEST(itkVectorFuzzyConnectednessImageFilterTest ${PATENTED_TESTS} itkVectorFuzzyConnectednessImageFilterTest)

SET(PatentedTest_SRCS
itkActiveShapeModelCalculatorTest
itkSimpleFuzzyConnectednessScalarImageFilterTest.cxx
itkVectorFuzzyConnectednessImageFilterTest.cxx
)

ADD_EXECUTABLE(itkPatentedPrintTest itkPatentedPrintTest.cxx)
TARGET_LINK_LIBRARIES(itkPatentedPrintTest ITKAlgorithms ITKFEM)
ADD_EXECUTABLE(itkPatentedHeaderTest itkPatentedHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkPatentedHeaderTest ITKAlgorithms ITKFEM)

ADD_TEST(itkPatentedHeaderTest ${PATENTED_HEADER_TESTS})

IF(ITK_OBJCXX_COMPILER_WORKS)
  SET(PATENTED_OBJCXX_TEST ${CXX_TEST_PATH}/itkPatentedHeaderObjCxxTest)
  ADD_TEST(itkPatentedHeaderObjCxxTest ${PATENTED_OBJCXX_TEST})
  ADD_EXECUTABLE(itkPatentedHeaderObjCxxTest itkPatentedHeaderObjCxxTest.mm)
  TARGET_LINK_LIBRARIES(itkPatentedHeaderObjCxxTest ITKAlgorithms ITKFEM)
ENDIF(ITK_OBJCXX_COMPILER_WORKS)

ADD_EXECUTABLE(itkPatentedTests itkPatentedTests.cxx 
                                  ${PatentedTest_SRCS})
TARGET_LINK_LIBRARIES(itkPatentedTests ITKAlgorithms ITKFEM ITKStatistics)

IF(ITK_DATA_ROOT)
ADD_TEST(itkActiveShapeModelCalculatorTest ${PATENTED_TESTS}
  itkActiveShapeModelCalculatorTest
             ${ITK_DATA_ROOT}/Input/model.mha
             ${ITK_DATA_ROOT}/Input/foot.mha
             )
ENDIF(ITK_DATA_ROOT)

ENDIF( NOT ITK_DISABLE_CXX_TESTING )

