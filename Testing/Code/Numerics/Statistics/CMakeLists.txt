IF (TCL_TCLSH)
ADD_TEST(PrintSelf-Statistics ${TCL_TCLSH}
         ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
         ${ITK_SOURCE_DIR}/Code/Numerics/Statistics)
ENDIF (TCL_TCLSH)

INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/doc
                    ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vnl
                    ${ITK_SOURCE_DIR}/Code/BasicFilters 
                    ${ITK_SOURCE_DIR}/Code/Numerics/Statistics)

LINK_DIRECTORIES(${ITK_BINARY_DIR}/Code/Numerics/Statistics)

IF( CXX_TEST_PATH )
  SET(STATISTICS_TESTS ${CXX_TEST_PATH}/itkStatisticsTests)
ELSE( CXX_TEST_PATH )
  SET(STATISTICS_TESTS itkStatisticsTests)
ENDIF( CXX_TEST_PATH )

ADD_TEST(itkStatisticsAlgorithmTest ${STATISTICS_TESTS} itkStatisticsAlgorithmTest)
ADD_TEST(itkCovarianceCalculatorTest ${STATISTICS_TESTS} itkCovarianceCalculatorTest)
ADD_TEST(itkMeanCalculatorTest ${STATISTICS_TESTS} itkMeanCalculatorTest)
ADD_TEST(itkSubsampleTest ${STATISTICS_TESTS} itkSubsampleTest)
ADD_TEST(itkMembershipSampleTest ${STATISTICS_TESTS} itkMembershipSampleTest)
ADD_TEST(itkImageToListAdaptorTest ${STATISTICS_TESTS} itkImageToListAdaptorTest)
ADD_TEST(itkHistogramTest ${STATISTICS_TESTS} itkHistogramTest)
ADD_TEST(itkDenseFrequencyContainerTest ${STATISTICS_TESTS} itkDenseFrequencyContainerTest)
ADD_TEST(itkNormalVariateGeneratorTest ${STATISTICS_TESTS} 
                    itkNormalVariateGeneratorTest)


SET(Statistics_SRCS
itkStatisticsAlgorithmTest.cxx
itkCovarianceCalculatorTest.cxx
itkMeanCalculatorTest.cxx
itkSubsampleTest.cxx
itkMembershipSampleTest.cxx
itkImageToListAdaptorTest.cxx
itkHistogramTest.cxx
itkDenseFrequencyContainerTest.cxx
itkNormalVariateGeneratorTest.cxx
)

ADD_EXECUTABLE(itkStatisticsTests itkStatisticsTests.cxx ${Statistics_SRCS})
TARGET_LINK_LIBRARIES(itkStatisticsTests ITKCommon ITKNumerics ITKStatistics)







