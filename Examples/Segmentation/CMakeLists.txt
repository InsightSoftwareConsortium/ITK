#
# Examples on the use of segmentation algorithms
#

PROJECT(ImageSegmentation)
INCLUDE_REGULAR_EXPRESSION("^.*$")

ADD_EXECUTABLE(ConfidenceConnected ConfidenceConnected.cxx )
TARGET_LINK_LIBRARIES(ConfidenceConnected ITKNumerics ITKIO)

ADD_EXECUTABLE(NeighborhoodConnectedImageFilter NeighborhoodConnectedImageFilter.cxx )
TARGET_LINK_LIBRARIES(NeighborhoodConnectedImageFilter ITKNumerics ITKIO)

ADD_EXECUTABLE(IsolatedConnectedImageFilter IsolatedConnectedImageFilter.cxx )
TARGET_LINK_LIBRARIES(IsolatedConnectedImageFilter ITKNumerics ITKIO)

ADD_EXECUTABLE(CannySegmentationLevelSetImageFilter CannySegmentationLevelSetImageFilter.cxx )
TARGET_LINK_LIBRARIES(CannySegmentationLevelSetImageFilter ITKNumerics ITKIO)

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

ADD_EXECUTABLE(LaplacianSegmentationLevelSetImageFilter LaplacianSegmentationLevelSetImageFilter.cxx )
TARGET_LINK_LIBRARIES(LaplacianSegmentationLevelSetImageFilter ITKNumerics ITKIO)

ADD_EXECUTABLE(ThresholdSegmentationLevelSetImageFilter ThresholdSegmentationLevelSetImageFilter.cxx )
TARGET_LINK_LIBRARIES(ThresholdSegmentationLevelSetImageFilter ITKNumerics ITKIO)

ADD_EXECUTABLE(WatershedSegmentation1 WatershedSegmentation1.cxx )
TARGET_LINK_LIBRARIES(WatershedSegmentation1 ITKAlgorithms ITKNumerics ITKIO)

ADD_EXECUTABLE(DeformableModel1 DeformableModel1.cxx )
TARGET_LINK_LIBRARIES(DeformableModel1 ITKAlgorithms ITKNumerics ITKIO)

ADD_EXECUTABLE(GibbsPriorImageFilter1 GibbsPriorImageFilter1.cxx )
TARGET_LINK_LIBRARIES(GibbsPriorImageFilter1 ITKAlgorithms ITKNumerics ITKIO)

ADD_EXECUTABLE(HoughTransform2DLinesImageFilter HoughTransform2DLinesImageFilter.cxx )
TARGET_LINK_LIBRARIES(HoughTransform2DLinesImageFilter ITKBasicFilters ITKIO)

ADD_EXECUTABLE(VectorConfidenceConnected VectorConfidenceConnected.cxx )
TARGET_LINK_LIBRARIES(VectorConfidenceConnected ITKNumerics ITKIO)

SET(SEGMENTATION_EXAMPLES ${CXX_TEST_PATH}/SegmentationExamples)
SET(SEGMENTATION_EXAMPLES2 ${CXX_TEST_PATH}/SegmentationExamples2)

SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Segmentation)
SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

ADD_TEST(CannySegmentationLevelSetImageFilterTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/CannySegmentationLevelSetImageFilterTest.png
            ${TEMP}/CannySegmentationLevelSetImageFilterTest.png
  CannySegmentationLevelSetImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${BASELINE}/ThresholdSegmentationLevelSetVentricleTest.png
        ${TEMP}/CannySegmentationLevelSetImageFilterTest.png
        7 0.1 -10.0 127.5 10000
)

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

ADD_TEST(NeighborhoodConnectedWhiteTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/NeighborhoodConnectedWhiteTest.png
            ${TEMP}/NeighborhoodConnectedWhiteTest.png
  NeighborhoodConnectedImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/NeighborhoodConnectedWhiteTest.png
        60 116 150 180
)

ADD_TEST(NeighborhoodConnectedVentricleTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/NeighborhoodConnectedVentricleTest.png
            ${TEMP}/NeighborhoodConnectedVentricleTest.png
  NeighborhoodConnectedImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/NeighborhoodConnectedVentricleTest.png
        81 112 210 250
)

ADD_TEST(NeighborhoodConnectedGrayTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/NeighborhoodConnectedGrayTest.png
            ${TEMP}/NeighborhoodConnectedGrayTest.png
  NeighborhoodConnectedImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/NeighborhoodConnectedGrayTest.png
        107 69 180 210
)

ADD_TEST(IsolatedConnectedTest ${SEGMENTATION_EXAMPLES2}
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

ADD_TEST(GeodesicActiveContourLeftVentricleTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/GeodesicActiveContourLeftVentricleTest.png
            ${TEMP}/GeodesicActiveContourLeftVentricleTest.png
  GeodesicActiveContourImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourLeftVentricleTest.png
        81 114 5.0 1.0 -0.5 3.0 2.0
)

ADD_TEST(GeodesicActiveContourRightVentricleTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/GeodesicActiveContourRightVentricleTest.png
            ${TEMP}/GeodesicActiveContourRightVentricleTest.png
  GeodesicActiveContourImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourRightVentricleTest.png
        99 114 5.0 1.0 -0.5 3.0 2.0
)

ADD_TEST(GeodesicActiveContourWhiteMatterTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/GeodesicActiveContourWhiteMatterTest.png
            ${TEMP}/GeodesicActiveContourWhiteMatterTest.png
  GeodesicActiveContourImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourWhiteMatterTest.png
        56 92 5.0 1.0 -0.3 2.0 10.0
)

ADD_TEST(GeodesicActiveContourGrayMatterTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/GeodesicActiveContourGrayMatterTest.png
            ${TEMP}/GeodesicActiveContourGrayMatterTest.png
  GeodesicActiveContourImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourGrayMatterTest.png
        40 90 5.0 0.5 -0.3 2.0 10.0
)

ADD_TEST(HoughTransformTwo2DLinesTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/HoughTransformTwoLinesTest.png
            ${TEMP}/HoughTransformTwoLinesTest.png
  HoughTransform2DLinesImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/HoughTransformTwoLines.png
        ${TEMP}/HoughTransformTwoLinesTest.png
        2 5 10
)

ADD_TEST(ShapeDetectionLevelSetLeftVentricleTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/ShapeDetectionLevelSetLeftVentricleTest.png
            ${TEMP}/ShapeDetectionLevelSetLeftVentricleTest.png
  ShapeDetectionLevelSetFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetLeftVentricleTest.png
        81 114 5.0 1.0 -0.5 3.0 0.05 1.0
)

ADD_TEST(ShapeDetectionLevelSetRightVentricleTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/ShapeDetectionLevelSetRightVentricleTest.png
            ${TEMP}/ShapeDetectionLevelSetRightVentricleTest.png
  ShapeDetectionLevelSetFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetRightVentricleTest.png
        99 114 5.0 1.0 -0.5 3.0 0.05 1.0
)

ADD_TEST(ShapeDetectionLevelSetWhiteMatterTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/ShapeDetectionLevelSetWhiteMatterTest.png
            ${TEMP}/ShapeDetectionLevelSetWhiteMatterTest.png
  ShapeDetectionLevelSetFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetWhiteMatterTest.png
        56 92 5.0 1.0 -0.3 2.0 0.05 1.0
)

ADD_TEST(ShapeDetectionLevelSetGrayMatterTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/ShapeDetectionLevelSetGrayMatterTest.png
            ${TEMP}/ShapeDetectionLevelSetGrayMatterTest.png
  ShapeDetectionLevelSetFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetGrayMatterTest.png
        40 90 5.0 0.5 -0.3 2.0 0.05 1.0
)

ADD_TEST(ThresholdSegmentationLevelSetVentricleTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/ThresholdSegmentationLevelSetVentricleTest.png
            ${TEMP}/ThresholdSegmentationLevelSetVentricleTest.png
  ThresholdSegmentationLevelSetImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdSegmentationLevelSetVentricleTest.png
        81 112 5 210 250
)

ADD_TEST(ThresholdSegmentationLevelSetWhiteMatterTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/ThresholdSegmentationLevelSetWhiteMatterTest.png
            ${TEMP}/ThresholdSegmentationLevelSetWhiteMatterTest.png
  ThresholdSegmentationLevelSetImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdSegmentationLevelSetWhiteMatterTest.png
        60 116 5 150 180 10.0
)

ADD_TEST(ThresholdSegmentationLevelSetGrayMatterTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/ThresholdSegmentationLevelSetGrayMatterTest.png
            ${TEMP}/ThresholdSegmentationLevelSetGrayMatterTest.png
  ThresholdSegmentationLevelSetImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdSegmentationLevelSetGrayMatterTest.png
        107 69 5 180 210
)

ADD_TEST(WatershedSegmentation1Test1 ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/WatershedSegmentation1Test1.png
            ${TEMP}/WatershedSegmentation1Test1.png
  WatershedSegmentation1Test
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/WatershedSegmentation1Test1.png
        2.0 10 0.0 0.05 1
)

ADD_TEST(WatershedSegmentation1Test2 ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/WatershedSegmentation1Test2.png
            ${TEMP}/WatershedSegmentation1Test2.png
  WatershedSegmentation1Test
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/WatershedSegmentation1Test2.png
        2.0 10 0.001 0.15 0
)

ADD_EXECUTABLE(SegmentationExamples SegmentationExamples.cxx )
TARGET_LINK_LIBRARIES(SegmentationExamples ITKAlgorithms ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(SegmentationExamples2 SegmentationExamples2.cxx )
TARGET_LINK_LIBRARIES(SegmentationExamples2 ITKAlgorithms ITKIO ITKBasicFilters ITKCommon)
