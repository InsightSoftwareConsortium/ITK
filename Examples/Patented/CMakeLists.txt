PROJECT(PatentedExamples)
INCLUDE_REGULAR_EXPRESSION("^.*$")

ADD_EXECUTABLE(IterativeClosestPoint1   IterativeClosestPoint1.cxx )
TARGET_LINK_LIBRARIES(IterativeClosestPoint1 ITKAlgorithms ITKStatistics ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(IterativeClosestPoint2   IterativeClosestPoint2.cxx )
TARGET_LINK_LIBRARIES(IterativeClosestPoint2 ITKAlgorithms ITKStatistics ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(IterativeClosestPoint3   IterativeClosestPoint3.cxx )
TARGET_LINK_LIBRARIES(IterativeClosestPoint3 ITKAlgorithms ITKStatistics ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(FuzzyConnectednessImageFilter FuzzyConnectednessImageFilter.cxx )
TARGET_LINK_LIBRARIES(FuzzyConnectednessImageFilter ITKNumerics ITKIO)

ADD_EXECUTABLE(HybridSegmentationFuzzyVoronoi HybridSegmentationFuzzyVoronoi.cxx )
TARGET_LINK_LIBRARIES(HybridSegmentationFuzzyVoronoi ITKNumerics ITKIO)

SET(PATENTED_EXAMPLES ${CXX_TEST_PATH}/PatentedExamples)
SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Patented)
SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

IF( NOT ITK_DISABLE_CXX_TESTING )

ADD_EXECUTABLE(PatentedExamples PatentedExamples.cxx )
TARGET_LINK_LIBRARIES(PatentedExamples ITKAlgorithms ITKIO ITKBasicFilters ITKCommon)

ADD_TEST(FuzzyConnectednessImageFilter ${PATENTED_EXAMPLES}
  --compare ${BASELINE}/FuzzyConnectednessVWEyeTest.png
            ${TEMP}/FuzzyConnectednessVWEyeTest.png
  FuzzyConnectednessImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/FuzzyConnectednessVWEyeBinaryTest.png
        ${TEMP}/FuzzyConnectednessVWEyeTest.png
        64 66 3.0
)

ADD_TEST(IterativeClosestPoint1Test ${PATENTED_EXAMPLES}
  IterativeClosestPoint1Test
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointFixedPoints.txt
        ${ITK_SOURCE_DIR}/Examples/Data/IterativeClosestPointMovingPoints.txt
)



ENDIF( NOT ITK_DISABLE_CXX_TESTING )

