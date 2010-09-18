if( NOT ITK_DISABLE_CXX_TESTING )
if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-Patented ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/Patented)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)


set(PATENTED_TESTS ${CXX_TEST_PATH}/itkPatentedTests)

set(PATENTED_PRINT_TEST ${CXX_TEST_PATH}/itkPatentedPrintTest)

set(PATENTED_HEADER_TESTS ${CXX_TEST_PATH}/itkPatentedHeaderTest)

add_test(itkPatentedPrintTest ${PATENTED_PRINT_TEST})

add_test(itkSimpleFuzzyConnectednessScalarImageFilterTest ${PATENTED_TESTS} itkSimpleFuzzyConnectednessScalarImageFilterTest)
add_test(itkVectorFuzzyConnectednessImageFilterTest ${PATENTED_TESTS} itkVectorFuzzyConnectednessImageFilterTest)

set(PatentedTest_SRCS
itkActiveShapeModelCalculatorTest
itkSimpleFuzzyConnectednessScalarImageFilterTest.cxx
itkVectorFuzzyConnectednessImageFilterTest.cxx
)

add_executable(itkPatentedPrintTest itkPatentedPrintTest.cxx)
target_link_libraries(itkPatentedPrintTest ITKAlgorithms ITKFEM)
add_executable(itkPatentedHeaderTest itkPatentedHeaderTest.cxx)
target_link_libraries(itkPatentedHeaderTest ITKAlgorithms ITKFEM)

add_test(itkPatentedHeaderTest ${PATENTED_HEADER_TESTS})

if(ITK_OBJCXX_COMPILER_WORKS)
  set(PATENTED_OBJCXX_TEST ${CXX_TEST_PATH}/itkPatentedHeaderObjCxxTest)
  add_test(itkPatentedHeaderObjCxxTest ${PATENTED_OBJCXX_TEST})
  add_executable(itkPatentedHeaderObjCxxTest itkPatentedHeaderObjCxxTest.mm)
  target_link_libraries(itkPatentedHeaderObjCxxTest ITKAlgorithms ITKFEM)
endif(ITK_OBJCXX_COMPILER_WORKS)

add_executable(itkPatentedTests itkPatentedTests.cxx
                                  ${PatentedTest_SRCS})
target_link_libraries(itkPatentedTests ITKAlgorithms ITKFEM ITKStatistics)

if(ITK_DATA_ROOT)
add_test(itkActiveShapeModelCalculatorTest ${PATENTED_TESTS}
  itkActiveShapeModelCalculatorTest
             ${ITK_DATA_ROOT}/Input/model.mha
             ${ITK_DATA_ROOT}/Input/foot.mha
             )
endif(ITK_DATA_ROOT)

endif( NOT ITK_DISABLE_CXX_TESTING )

