project(PatentedExamples)
include_regular_expression("^.*$")

add_executable(FuzzyConnectednessImageFilter FuzzyConnectednessImageFilter.cxx )
target_link_libraries(FuzzyConnectednessImageFilter ITKNumerics ITKIO)

add_executable(HybridSegmentationFuzzyVoronoi HybridSegmentationFuzzyVoronoi.cxx )
target_link_libraries(HybridSegmentationFuzzyVoronoi ITKNumerics ITKIO)

set(PATENTED_EXAMPLES ${CXX_TEST_PATH}/PatentedExamples)
set(BASELINE ${ITK_DATA_ROOT}/Baseline/Patented)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

if( NOT ITK_DISABLE_CXX_TESTING )

if(BUILD_TESTING)

add_executable(PatentedExamples PatentedExamples.cxx )
target_link_libraries(PatentedExamples ITKAlgorithms ITKIO ITKBasicFilters ITKNumerics)

add_test(FuzzyConnectednessImageFilter ${PATENTED_EXAMPLES}
  --compare ${BASELINE}/FuzzyConnectednessVWEyeTest.png
            ${TEMP}/FuzzyConnectednessVWEyeTest.png
  FuzzyConnectednessImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/FuzzyConnectednessVWEyeBinaryTest.png
        ${TEMP}/FuzzyConnectednessVWEyeTest.png
        64 66 3.0
)

endif(BUILD_TESTING)

endif( NOT ITK_DISABLE_CXX_TESTING )

