#
# Examples on the use of segmentation algorithms
#

PROJECT(ImageSegmentation)

ADD_EXECUTABLE(ConfidenceConnected ConfidenceConnected.cxx )
TARGET_LINK_LIBRARIES(ConfidenceConnected ITKNumerics ITKIO)

ADD_EXECUTABLE(NeighborhoodConnectedImageFilter NeighborhoodConnectedImageFilter.cxx )
TARGET_LINK_LIBRARIES(NeighborhoodConnectedImageFilter ITKNumerics ITKIO)

ADD_EXECUTABLE(IsolatedConnectedImageFilter IsolatedConnectedImageFilter.cxx )
TARGET_LINK_LIBRARIES(IsolatedConnectedImageFilter ITKNumerics ITKIO)

ADD_EXECUTABLE(ConnectedThresholdImageFilter ConnectedThresholdImageFilter.cxx )
TARGET_LINK_LIBRARIES(ConnectedThresholdImageFilter ITKNumerics ITKIO)

ADD_EXECUTABLE(HybridSegmentationFuzzyVoronoi HybridSegmentationFuzzyVoronoi.cxx )
TARGET_LINK_LIBRARIES(HybridSegmentationFuzzyVoronoi ITKNumerics ITKIO)

ADD_EXECUTABLE(FastMarchingImageFilter FastMarchingImageFilter.cxx )
TARGET_LINK_LIBRARIES(FastMarchingImageFilter ITKNumerics ITKIO)

ADD_EXECUTABLE(ShapeDetectionLevelSetFilter ShapeDetectionLevelSetFilter.cxx )
TARGET_LINK_LIBRARIES(ShapeDetectionLevelSetFilter ITKNumerics ITKIO)

ADD_EXECUTABLE(GeodesicActiveContourImageFilter GeodesicActiveContourImageFilter.cxx )
TARGET_LINK_LIBRARIES(GeodesicActiveContourImageFilter ITKNumerics ITKIO)

ADD_EXECUTABLE(WatershedSegmentation1 WatershedSegmentation1.cxx )
TARGET_LINK_LIBRARIES(WatershedSegmentation1 ITKAlgorithms ITKNumerics ITKIO)

ADD_EXECUTABLE(DeformableModel1 DeformableModel1.cxx )
TARGET_LINK_LIBRARIES(DeformableModel1 ITKAlgorithms ITKNumerics ITKIO)

ADD_EXECUTABLE(GibbsPriorImageFilter1 GibbsPriorImageFilter1.cxx )
TARGET_LINK_LIBRARIES(GibbsPriorImageFilter1 ITKAlgorithms ITKNumerics ITKIO)

SET(SEGMENTATION_EXAMPLES ${CXX_TEST_PATH}/SegmentationExamples)

SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Segmentation)
SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

ADD_TEST(ConfidenceConnectedWhiteTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/ConfidenceConnectedWhiteTest.png
            ${TEMP}/ConfidenceConnectedWhiteTest.png
  ConfidenceConnectedTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConfidenceConnectedWhiteTest.png
        60 116
)

ADD_TEST(ConfidenceConnectedVentricleTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/ConfidenceConnectedVentricleTest.png
            ${TEMP}/ConfidenceConnectedVentricleTest.png
  ConfidenceConnectedTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConfidenceConnectedVentricleTest.png
        81 112
)

ADD_TEST(ConfidenceConnectedGrayTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/ConfidenceConnectedGrayTest.png
            ${TEMP}/ConfidenceConnectedGrayTest.png
  ConfidenceConnectedTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConfidenceConnectedGrayTest.png
        107 69
)

ADD_TEST(ConnectedThresholdWhiteTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/ConnectedThresholdWhiteTest.png
            ${TEMP}/ConnectedThresholdWhiteTest.png
  ConnectedThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConnectedThresholdWhiteTest.png
        60 116 150 180
)

ADD_TEST(ConnectedThresholdVentricleTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/ConnectedThresholdVentricleTest.png
            ${TEMP}/ConnectedThresholdVentricleTest.png
  ConnectedThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConnectedThresholdVentricleTest.png
        81 112 210 250
)

ADD_TEST(ConnectedThresholdGrayTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/ConnectedThresholdGrayTest.png
            ${TEMP}/ConnectedThresholdGrayTest.png
  ConnectedThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConnectedThresholdGrayTest.png
        107 69 180 210
)

ADD_TEST(NeighborhoodConnectedWhiteTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/NeighborhoodConnectedWhiteTest.png
            ${TEMP}/NeighborhoodConnectedWhiteTest.png
  NeighborhoodConnectedImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/NeighborhoodConnectedWhiteTest.png
        60 116 150 180
)

ADD_TEST(NeighborhoodConnectedVentricleTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/NeighborhoodConnectedVentricleTest.png
            ${TEMP}/NeighborhoodConnectedVentricleTest.png
  NeighborhoodConnectedImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/NeighborhoodConnectedVentricleTest.png
        81 112 210 250
)

ADD_TEST(NeighborhoodConnectedGrayTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/NeighborhoodConnectedGrayTest.png
            ${TEMP}/NeighborhoodConnectedGrayTest.png
  NeighborhoodConnectedImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/NeighborhoodConnectedGrayTest.png
        107 69 180 210
)

ADD_TEST(IsolatedConnectedTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/IsolatedConnectedTest.png
            ${TEMP}/IsolatedConnectedTest.png
  IsolatedConnectedImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/IsolatedConnectedTest.png
        61 140 150 63 43
)

ADD_TEST(FastMarchingLeftVentricleTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/FastMarchingLeftVentricleTest.png
            ${TEMP}/FastMarchingLeftVentricleTest.png
  FastMarchingImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingLeftVentricleTest.png
        81 114 1.0 -0.5 3.0 100 150        
)

ADD_TEST(FastMarchingRightVentricleTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/FastMarchingRightVentricleTest.png
            ${TEMP}/FastMarchingRightVentricleTest.png
  FastMarchingImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingRightVentricleTest.png
        99 114 1.0 -0.5 3.0 100 150        
)

ADD_TEST(FastMarchingWhiteMatterTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/FastMarchingWhiteMatterTest.png
            ${TEMP}/FastMarchingWhiteMatterTest.png
  FastMarchingImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingWhiteMatterTest.png
        56 92 1.0 -0.3 2.0 200 250        
)

ADD_TEST(FastMarchingGrayMatterTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/FastMarchingGrayMatterTest.png
            ${TEMP}/FastMarchingGrayMatterTest.png
  FastMarchingImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingGrayMatterTest.png
        40 90 .5 -0.3 2.0 200 250        
)

ADD_EXECUTABLE(SegmentationExamples SegmentationExamples.cxx )
TARGET_LINK_LIBRARIES(SegmentationExamples ITKAlgorithms ITKIO ITKBasicFilters ITKCommon)
