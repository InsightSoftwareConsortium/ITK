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

ADD_TEST(itkStatisticsAlgorithmTest itkStatisticsTests itkStatisticsAlgorithmTest)
ADD_TEST(itkCovarianceCalculatorTest itkStatisticsTests itkCovarianceCalculatorTest)
ADD_TEST(itkMeanCalculatorTest itkStatisticsTests itkMeanCalculatorTest)
ADD_TEST(itkSubsampleTest itkStatisticsTests itkSubsampleTest)
ADD_TEST(itkMembershipSampleTest itkStatisticsTests itkMembershipSampleTest)
ADD_TEST(itkImageToListAdaptorTest itkStatisticsTests itkImageToListAdaptorTest)
ADD_TEST(itkHistogramTest itkStatisticsTests itkHistogramTest)
ADD_TEST(itkDenseFrequencyContainerTest itkStatisticsTests itkDenseFrequencyContainerTest)
ADD_TEST(itkFastRandomUnitNormalVariateGeneratorTest itkStatisticsTests 
                    itkFastRandomUnitNormalVariateGeneratorTest)

LINK_LIBRARIES (
ITKCommon
ITKNumerics
ITKStatistics
)

SOURCE_FILES(Statistics_SRCS
itkStatisticsAlgorithmTest
itkCovarianceCalculatorTest
itkMeanCalculatorTest
itkSubsampleTest
itkMembershipSampleTest
itkImageToListAdaptorTest
itkHistogramTest 
itkDenseFrequencyContainerTest
itkFastRandomUnitNormalVariateGeneratorTest 
)

ADD_EXECUTABLE(itkStatisticsTests itkStatisticsTests.cxx Statistics_SRCS)






