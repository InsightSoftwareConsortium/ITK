IF (TCL_TCLSH)
ADD_TEST(PrintSelf-Statistics ${TCL_TCLSH}
         ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
         ${ITK_SOURCE_DIR}/Code/Numerics/Statistics)
ENDIF (TCL_TCLSH)

INCLUDE_DIRECTORIES(${ITK_SOURCE_DIR}/Code/Numerics/vxl/vcl/doc
                    ${ITK_SOURCE_DIR}/Code/Numerics/vxl/vnl 
                    ${ITK_SOURCE_DIR}/Code/Numerics/Statistics)

ADD_TEST(itkHistogramTest itkStatisticsTests itkHistogramTest)
ADD_TEST(itkDenseFrequencyContainerTest itkStatisticsTests itkDenseFrequencyContainerTest)

LINK_LIBRARIES (
ITKCommon
ITKNumerics
ITKStatistics
)

SOURCE_FILES(Statistics_SRCS
itkHistogramTest 
itkDenseFrequencyContainerTest
)

ADD_EXECUTABLE(itkStatisticsTests itkStatisticsTests.cxx Statistics_SRCS)


