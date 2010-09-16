#
# Examples on the use of segmentation algorithms
#

project(ImageSegmentation)
include_regular_expression("^.*$")

add_executable(ConfidenceConnected ConfidenceConnected.cxx )
target_link_libraries(ConfidenceConnected ITKNumerics ITKIO)

add_executable(ConfidenceConnected3D ConfidenceConnected3D.cxx )
target_link_libraries(ConfidenceConnected3D ITKNumerics ITKIO)

add_executable(NeighborhoodConnectedImageFilter NeighborhoodConnectedImageFilter.cxx )
target_link_libraries(NeighborhoodConnectedImageFilter ITKNumerics ITKIO)

add_executable(IsolatedConnectedImageFilter IsolatedConnectedImageFilter.cxx )
target_link_libraries(IsolatedConnectedImageFilter ITKNumerics ITKIO)

add_executable(ConnectedThresholdImageFilter ConnectedThresholdImageFilter.cxx )
target_link_libraries(ConnectedThresholdImageFilter ITKNumerics ITKIO)

add_executable(FastMarchingImageFilter FastMarchingImageFilter.cxx )
target_link_libraries(FastMarchingImageFilter ITKNumerics ITKIO)

add_executable(GeodesicActiveContourShapePriorLevelSetImageFilter
GeodesicActiveContourShapePriorLevelSetImageFilter.cxx )
target_link_libraries(GeodesicActiveContourShapePriorLevelSetImageFilter
ITKNumerics ITKIO ITKStatistics)

add_executable(DeformableModel2 DeformableModel2.cxx )
target_link_libraries(DeformableModel2 ITKAlgorithms ITKIO)

add_executable(GibbsPriorImageFilter1 GibbsPriorImageFilter1.cxx )
target_link_libraries(GibbsPriorImageFilter1 ITKAlgorithms ITKIO)

add_executable(HoughTransform2DCirclesImageFilter HoughTransform2DCirclesImageFilter.cxx )
target_link_libraries(HoughTransform2DCirclesImageFilter ITKBasicFilters ITKIO)

add_executable(HoughTransform2DLinesImageFilter HoughTransform2DLinesImageFilter.cxx )
target_link_libraries(HoughTransform2DLinesImageFilter ITKBasicFilters ITKIO)

add_executable(VectorConfidenceConnected VectorConfidenceConnected.cxx )
target_link_libraries(VectorConfidenceConnected ITKNumerics ITKIO)

add_executable(RelabelComponentImageFilter RelabelComponentImageFilter.cxx )
target_link_libraries(RelabelComponentImageFilter ITKNumerics ITKIO)

add_executable(CannySegmentationLevelSetImageFilter CannySegmentationLevelSetImageFilter.cxx )
target_link_libraries(CannySegmentationLevelSetImageFilter ITKNumerics ITKIO)

add_executable(GeodesicActiveContourImageFilter GeodesicActiveContourImageFilter.cxx )
target_link_libraries(GeodesicActiveContourImageFilter ITKNumerics ITKIO)

add_executable(ShapeDetectionLevelSetFilter ShapeDetectionLevelSetFilter.cxx )
target_link_libraries(ShapeDetectionLevelSetFilter ITKNumerics ITKIO)

add_executable(CurvesLevelSetImageFilter CurvesLevelSetImageFilter.cxx )
target_link_libraries(CurvesLevelSetImageFilter ITKNumerics ITKIO)

add_executable(WatershedSegmentation1 WatershedSegmentation1.cxx )
target_link_libraries(WatershedSegmentation1 ITKAlgorithms ITKIO)

add_executable(WatershedSegmentation2 WatershedSegmentation2.cxx )
target_link_libraries(WatershedSegmentation2 ITKAlgorithms ITKIO)

add_executable(CellularSegmentation1 CellularSegmentation1.cxx )
target_link_libraries(CellularSegmentation1 ITKAlgorithms ITKNumerics ITKIO)

add_executable(CellularSegmentation2 CellularSegmentation2.cxx )
target_link_libraries(CellularSegmentation2 ITKAlgorithms ITKNumerics ITKIO)

add_executable(DeformableModel1 DeformableModel1.cxx )
target_link_libraries(DeformableModel1 ITKAlgorithms ITKNumerics ITKIO)

add_executable(LaplacianSegmentationLevelSetImageFilter LaplacianSegmentationLevelSetImageFilter.cxx )
target_link_libraries(LaplacianSegmentationLevelSetImageFilter ITKNumerics ITKIO)

add_executable(ThresholdSegmentationLevelSetImageFilter ThresholdSegmentationLevelSetImageFilter.cxx )
target_link_libraries(ThresholdSegmentationLevelSetImageFilter ITKNumerics ITKIO)


set(BASELINE ${ITK_DATA_ROOT}/Baseline/Segmentation)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)


if( NOT ITK_DISABLE_CXX_TESTING )

if(BUILD_TESTING)

set(SEGMENTATION_EXAMPLES ${CXX_TEST_PATH}/SegmentationExamples)
set(SEGMENTATION_EXAMPLES2 ${CXX_TEST_PATH}/SegmentationExamples2)
set(SEGMENTATION_EXAMPLES3 ${CXX_TEST_PATH}/SegmentationExamples3)
set(SEGMENTATION_EXAMPLES4 ${CXX_TEST_PATH}/SegmentationExamples4)
set(SEGMENTATION_EXAMPLES5 ${CXX_TEST_PATH}/SegmentationExamples5)
set(SEGMENTATION_EXAMPLES6 ${CXX_TEST_PATH}/SegmentationExamples6)
set(SEGMENTATION_EXAMPLES7 ${CXX_TEST_PATH}/SegmentationExamples7)
set(SEGMENTATION_EXAMPLES8 ${CXX_TEST_PATH}/SegmentationExamples8)
set(SEGMENTATION_EXAMPLES9 ${CXX_TEST_PATH}/SegmentationExamples9)

add_executable(SegmentationExamples SegmentationExamples.cxx )
target_link_libraries(SegmentationExamples
ITKAlgorithms
ITKIO
ITKBasicFilters
)

add_executable(SegmentationExamples2 SegmentationExamples2.cxx )
target_link_libraries(SegmentationExamples2
ITKAlgorithms
ITKIO
ITKBasicFilters
)

add_executable(SegmentationExamples3 SegmentationExamples3.cxx )
target_link_libraries(SegmentationExamples3
ITKAlgorithms
ITKIO
ITKBasicFilters
ITKNumerics
)

add_executable(SegmentationExamples4 SegmentationExamples4.cxx )
target_link_libraries(SegmentationExamples4
ITKAlgorithms
ITKIO
ITKBasicFilters
ITKStatistics
)

add_executable(SegmentationExamples5 SegmentationExamples5.cxx )
target_link_libraries(SegmentationExamples5
ITKAlgorithms
ITKIO
ITKBasicFilters
ITKStatistics
)

add_executable(SegmentationExamples6 SegmentationExamples6.cxx )
target_link_libraries(SegmentationExamples6
ITKAlgorithms
ITKIO
ITKBasicFilters
ITKStatistics
)

add_executable(SegmentationExamples7 SegmentationExamples7.cxx )
target_link_libraries(SegmentationExamples7
ITKAlgorithms
ITKIO
ITKBasicFilters
ITKStatistics
)

add_executable(SegmentationExamples8 SegmentationExamples8.cxx )
target_link_libraries(SegmentationExamples8
ITKAlgorithms
ITKIO
ITKBasicFilters
ITKStatistics
)

add_executable(SegmentationExamples9 SegmentationExamples9.cxx )
target_link_libraries(SegmentationExamples9
ITKAlgorithms
ITKIO
ITKBasicFilters
ITKStatistics
)

add_test(ConfidenceConnectedWhiteTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/ConfidenceConnectedWhiteTest.png
            ${TEMP}/ConfidenceConnectedWhiteTest.png
  ConfidenceConnectedTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConfidenceConnectedWhiteTest.png
        60 116
)

add_test(ConfidenceConnectedVentricleTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/ConfidenceConnectedVentricleTest.png
            ${TEMP}/ConfidenceConnectedVentricleTest.png
  ConfidenceConnectedTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConfidenceConnectedVentricleTest.png
        81 112
)

add_test(ConfidenceConnectedGrayTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/ConfidenceConnectedGrayTest.png
            ${TEMP}/ConfidenceConnectedGrayTest.png
  ConfidenceConnectedTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConfidenceConnectedGrayTest.png
        107 69
)

add_test(ConnectedThresholdWhiteTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/ConnectedThresholdWhiteTest.png
            ${TEMP}/ConnectedThresholdWhiteTest.png
  ConnectedThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConnectedThresholdWhiteTest.png
        60 116 150 180
)

add_test(ConnectedThresholdVentricleTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/ConnectedThresholdVentricleTest.png
            ${TEMP}/ConnectedThresholdVentricleTest.png
  ConnectedThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConnectedThresholdVentricleTest.png
        81 112 210 250
)

add_test(ConnectedThresholdGrayTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/ConnectedThresholdGrayTest.png
            ${TEMP}/ConnectedThresholdGrayTest.png
  ConnectedThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConnectedThresholdGrayTest.png
        107 69 180 210
)

add_test(FastMarchingLeftVentricleTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/FastMarchingLeftVentricleTest.png
            ${TEMP}/FastMarchingLeftVentricleTest.png
  FastMarchingImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingLeftVentricleTest.png
        81 114 1.0 -0.5 3.0 100 100
)

add_test(FastMarchingRightVentricleTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/FastMarchingRightVentricleTest.png
            ${TEMP}/FastMarchingRightVentricleTest.png
  FastMarchingImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingRightVentricleTest.png
        99 114 1.0 -0.5 3.0 100 100
)

add_test(FastMarchingWhiteMatterTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/FastMarchingWhiteMatterTest.png
            ${TEMP}/FastMarchingWhiteMatterTest.png
  FastMarchingImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingWhiteMatterTest.png
        56 92 1.0 -0.3 2.0 200 100
)

add_test(FastMarchingGrayMatterTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/FastMarchingGrayMatterTest.png
            ${TEMP}/FastMarchingGrayMatterTest.png
  FastMarchingImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingGrayMatterTest.png
        40 90 .5 -0.3 2.0 200 100
)

add_test(GeodesicActiveContourLeftVentricleTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/GeodesicActiveContourLeftVentricleTest.png
            ${TEMP}/GeodesicActiveContourLeftVentricleTest.png
  GeodesicActiveContourImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourLeftVentricleTest.png
        81 114 5.0 1.0 -0.5 3.0 2.0
)

add_test(GeodesicActiveContourRightVentricleTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/GeodesicActiveContourRightVentricleTest.png
            ${TEMP}/GeodesicActiveContourRightVentricleTest.png
  GeodesicActiveContourImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourRightVentricleTest.png
        99 114 5.0 1.0 -0.5 3.0 2.0
)

add_test(GeodesicActiveContourWhiteMatterTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/GeodesicActiveContourWhiteMatterTest.png
            ${TEMP}/GeodesicActiveContourWhiteMatterTest.png
  GeodesicActiveContourImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourWhiteMatterTest.png
        56 92 5.0 1.0 -0.3 2.0 10.0
)

add_test(GeodesicActiveContourGrayMatterTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/GeodesicActiveContourGrayMatterTest.png
            ${TEMP}/GeodesicActiveContourGrayMatterTest.png
  GeodesicActiveContourImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourGrayMatterTest.png
        40 90 5.0 .5 -0.3 2.0 10.0
)

add_test(ShapeDetectionLevelSetLeftVentricleTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/ShapeDetectionLevelSetLeftVentricleTest.png
            ${TEMP}/ShapeDetectionLevelSetLeftVentricleTest.png
  ShapeDetectionLevelSetFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetLeftVentricleTest.png
        81 114 5.0 1.0 -0.5 3.0 0.05 1.0
)

add_test(ShapeDetectionLevelSetRightVentricleTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/ShapeDetectionLevelSetRightVentricleTest.png
            ${TEMP}/ShapeDetectionLevelSetRightVentricleTest.png
  ShapeDetectionLevelSetFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetRightVentricleTest.png
        99 114 5.0 1.0 -0.5 3.0 0.05 1.0
)

add_test(ShapeDetectionLevelSetWhiteMatterTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/ShapeDetectionLevelSetWhiteMatterTest.png
            ${TEMP}/ShapeDetectionLevelSetWhiteMatterTest.png
  ShapeDetectionLevelSetFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetWhiteMatterTest.png
        56 92 5.0 1.0 -0.3 2.0 0.05 1.0
)

add_test(ShapeDetectionLevelSetGrayMatterTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/ShapeDetectionLevelSetGrayMatterTest.png
            ${TEMP}/ShapeDetectionLevelSetGrayMatterTest.png
  ShapeDetectionLevelSetFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetGrayMatterTest.png
        40 90 5.0 1.0 -0.3 2.0 0.05 1.0
)

add_test(ThresholdSegmentationLevelSetVentricleTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/ThresholdSegmentationLevelSetVentricleTest.png
            ${TEMP}/ThresholdSegmentationLevelSetVentricleTest.png
  ThresholdSegmentationLevelSetImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdSegmentationLevelSetVentricleTest.png
        81 112 5 210 250
)

add_test(ThresholdSegmentationLevelSetWhiteMatterTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/ThresholdSegmentationLevelSetWhiteMatterTest.png
            ${TEMP}/ThresholdSegmentationLevelSetWhiteMatterTest.png
  ThresholdSegmentationLevelSetImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdSegmentationLevelSetWhiteMatterTest.png
        60 116 5 150 180
)

add_test(ThresholdSegmentationLevelSetGrayMatterTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/ThresholdSegmentationLevelSetGrayMatterTest.png
            ${TEMP}/ThresholdSegmentationLevelSetGrayMatterTest.png
  ThresholdSegmentationLevelSetImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdSegmentationLevelSetGrayMatterTest.png
        107 69 5 180 210
)

add_test(CellularSegmentation1Test ${SEGMENTATION_EXAMPLES9}
  CellularSegmentation1Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        100 115 200 255 3000
        ${TEMP}/CellularSegmentation1Test.vtk
)

if( ITK_BRAINWEB_DATA_ROOT )

add_test(CellularSegmentation2Test ${SEGMENTATION_EXAMPLES9}
  CellularSegmentation2Test
        ${ITK_BRAINWEB_DATA_ROOT}/brainweb1e1a10f20.mha
        81 113 96 220 255 2000
        ${TEMP}/CellularSegmentation2Test.vtk
)

endif( ITK_BRAINWEB_DATA_ROOT )

add_test(VectorConfidenceConnectedTest1 ${SEGMENTATION_EXAMPLES8}
  --compare ${BASELINE}/VectorConfidenceConnected1.png
            ${TEMP}/VectorConfidenceConnected1.png
  VectorConfidenceConnectedTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/VectorConfidenceConnected1.png
        70 120 7 1
)

add_test(VectorConfidenceConnectedTest2 ${SEGMENTATION_EXAMPLES8}
  --compare ${BASELINE}/VectorConfidenceConnected2.png
            ${TEMP}/VectorConfidenceConnected2.png
  VectorConfidenceConnectedTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/VectorConfidenceConnected2.png
        23 93 7 1
)

add_test(VectorConfidenceConnectedTest3 ${SEGMENTATION_EXAMPLES8}
  --compare ${BASELINE}/VectorConfidenceConnected3.png
            ${TEMP}/VectorConfidenceConnected3.png
  VectorConfidenceConnectedTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/VectorConfidenceConnected3.png
        66 66 3 1
)


add_test(CannySegmentationLevelSetImageFilterTest ${SEGMENTATION_EXAMPLES}
  --compare ${BASELINE}/CannySegmentationLevelSetImageFilterTest.png
            ${TEMP}/CannySegmentationLevelSetImageFilterTest.png
  CannySegmentationLevelSetImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${BASELINE}/ThresholdSegmentationLevelSetVentricle.png
        ${TEMP}/CannySegmentationLevelSetImageFilterTest.png
        7 0.1 -10.0 127.5 10000
)

add_test(GeodesicActiveContourShapePriorCallosumTest ${SEGMENTATION_EXAMPLES3}
  --compare ${BASELINE}/GeodesicActiveContourShapePriorCallosumTest.png
            ${TEMP}/GeodesicActiveContourShapePriorCallosumTest.png
  GeodesicActiveContourShapePriorLevelSetImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainMidSagittalSlice.png
        ${TEMP}/GeodesicActiveContourShapePriorCallosumTest.png
        120 85 60 102 88 83 6 1.0 0.5 0.02
        ${ITK_SOURCE_DIR}/Examples/Data/CorpusCallosumMeanShape.mha 3
        ${ITK_SOURCE_DIR}/Examples/Data/CorpusCallosumMode%d.mha 10 0
)

add_test(NeighborhoodConnectedWhiteTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/NeighborhoodConnectedWhiteTest.png
            ${TEMP}/NeighborhoodConnectedWhiteTest.png
  NeighborhoodConnectedImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/NeighborhoodConnectedWhiteTest.png
        60 116 150 180
)

add_test(NeighborhoodConnectedVentricleTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/NeighborhoodConnectedVentricleTest.png
            ${TEMP}/NeighborhoodConnectedVentricleTest.png
  NeighborhoodConnectedImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/NeighborhoodConnectedVentricleTest.png
        81 112 210 250
)

add_test(NeighborhoodConnectedGrayTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/NeighborhoodConnectedGrayTest.png
            ${TEMP}/NeighborhoodConnectedGrayTest.png
  NeighborhoodConnectedImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/NeighborhoodConnectedGrayTest.png
        107 69 180 210
)

add_test(IsolatedConnectedTest ${SEGMENTATION_EXAMPLES2}
  --compare ${BASELINE}/IsolatedConnectedTest.png
            ${TEMP}/IsolatedConnectedTest.png
  IsolatedConnectedImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/IsolatedConnectedTest.png
        61 140 150 63 43
)

add_test(RelabelComponentImageFilterTest ${SEGMENTATION_EXAMPLES5}
  --compare ${BASELINE}/RelabelComponentImageFilter.png
            ${TEMP}/RelabelComponentImageFilter.png
  RelabelComponentImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/WatershedSegmentation.png
        ${TEMP}/RelabelComponentImageFilter.png
)

add_test(WatershedSegmentation1Test1 ${SEGMENTATION_EXAMPLES5}
  --compare ${BASELINE}/WatershedSegmentation1Test1.png
            ${TEMP}/WatershedSegmentation1Test1.png
  WatershedSegmentation1Test
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/WatershedSegmentation1Test1.png
        2.0 10 0.0 0.05 1
)

add_test(WatershedSegmentation1Test2 ${SEGMENTATION_EXAMPLES5}
  --compare ${BASELINE}/WatershedSegmentation1Test2.png
            ${TEMP}/WatershedSegmentation1Test2.png
  WatershedSegmentation1Test
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/WatershedSegmentation1Test2.png
        2.0 10 0.001 0.15 0
)

add_test(CurvesLevelSetImageFilterTest ${SEGMENTATION_EXAMPLES4}
  --compare ${BASELINE}/CurvesLevelSetImageFilterTest.png
            ${TEMP}/CurvesLevelSetImageFilterTest.png
  CurvesLevelSetImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/CurvesLevelSetImageFilterTest.png
        81 114 5.0 1.0 -0.5 3.0 2.0
)

add_test(HoughTransformTwo2DLinesTest ${SEGMENTATION_EXAMPLES6}
  --compare ${BASELINE}/HoughTransformTwoLinesTest.png
            ${TEMP}/HoughTransformTwoLinesTest.png
  HoughTransform2DLinesImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/HoughTransformTwoLines.png
        ${TEMP}/HoughTransformTwoLinesTest.png
        2 5 10
)

add_test(HoughTransformTwo2DCirclesTest ${SEGMENTATION_EXAMPLES6}
  --compare ${BASELINE}/HoughTransformTwoCirclesTest.png
            ${TEMP}/HoughTransformTwoCirclesTest.png
  HoughTransform2DCirclesImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/HoughTransformTwoCircles.png
        ${TEMP}/HoughTransformTwoCirclesTest.png
        2 6 20 3.14 10 5 1.5
)

add_test(LaplacianSegmentationLevelSetImageFilterTest ${SEGMENTATION_EXAMPLES7}
  --compare ${BASELINE}/LaplacianSegmentationLevelSetImageFilter.png
            ${TEMP}/LaplacianSegmentationLevelSetImageFilter.png
  LaplacianSegmentationLevelSetImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${BASELINE}/ThresholdSegmentationLevelSetVentricle.png
        ${TEMP}/LaplacianSegmentationLevelSetImageFilter.png
        10 2.0 1 127.5 15
)

endif(BUILD_TESTING)

endif( NOT ITK_DISABLE_CXX_TESTING )


