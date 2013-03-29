set(BASELINE ${ITK_DATA_ROOT}/Baseline/Segmentation)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

itk_add_test(NAME ConfidenceConnectedWhiteTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ConfidenceConnectedWhiteTest.png}
            ${TEMP}/ConfidenceConnectedWhiteTest.png
  $<TARGET_FILE:ConfidenceConnected>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConfidenceConnectedWhiteTest.png
        60 116
)

itk_add_test(NAME ConfidenceConnectedVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ConfidenceConnectedVentricleTest.png}
            ${TEMP}/ConfidenceConnectedVentricleTest.png
  $<TARGET_FILE:ConfidenceConnected>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConfidenceConnectedVentricleTest.png
        81 112
)

itk_add_test(NAME ConfidenceConnectedGrayTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ConfidenceConnectedGrayTest.png}
            ${TEMP}/ConfidenceConnectedGrayTest.png
  $<TARGET_FILE:ConfidenceConnected>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConfidenceConnectedGrayTest.png
        107 69
)

itk_add_test(NAME ConnectedThresholdWhiteTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ConnectedThresholdWhiteTest.png}
            ${TEMP}/ConnectedThresholdWhiteTest.png
  $<TARGET_FILE:ConnectedThresholdImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConnectedThresholdWhiteTest.png
        60 116 150 180
)

itk_add_test(NAME ConnectedThresholdVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ConnectedThresholdVentricleTest.png}
            ${TEMP}/ConnectedThresholdVentricleTest.png
  $<TARGET_FILE:ConnectedThresholdImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConnectedThresholdVentricleTest.png
        81 112 210 250
)

itk_add_test(NAME ConnectedThresholdGrayTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ConnectedThresholdGrayTest.png}
            ${TEMP}/ConnectedThresholdGrayTest.png
  $<TARGET_FILE:ConnectedThresholdImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConnectedThresholdGrayTest.png
        107 69 180 210
)

itk_add_test(NAME FastMarchingLeftVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/FastMarchingLeftVentricleTest.png,:}
            ${TEMP}/FastMarchingLeftVentricleTest.png
  $<TARGET_FILE:FastMarchingImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingLeftVentricleTest.png
        81 114 1.0 -0.5 3.0 100 100 debug1.png debug2.png debug3.png
)

itk_add_test(NAME FastMarchingRightVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/FastMarchingRightVentricleTest.png,:}
            ${TEMP}/FastMarchingRightVentricleTest.png
  $<TARGET_FILE:FastMarchingImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingRightVentricleTest.png
        99 114 1.0 -0.5 3.0 100 100 debug1.png debug2.png debug3.png
)

itk_add_test(NAME FastMarchingWhiteMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/FastMarchingWhiteMatterTest.png,:}
            ${TEMP}/FastMarchingWhiteMatterTest.png
  $<TARGET_FILE:FastMarchingImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingWhiteMatterTest.png
        56 92 1.0 -0.3 2.0 200 100 debug1.png debug2.png debug3.png
)

itk_add_test(NAME FastMarchingGrayMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/FastMarchingGrayMatterTest.png,:}
            ${TEMP}/FastMarchingGrayMatterTest.png
  $<TARGET_FILE:FastMarchingImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingGrayMatterTest.png
        40 90 .5 -0.3 2.0 200 100 debug1.png debug2.png debug3.png
)

itk_add_test(NAME GeodesicActiveContourLeftVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/GeodesicActiveContourLeftVentricleTest.png,:}
            ${TEMP}/GeodesicActiveContourLeftVentricleTest.png
  $<TARGET_FILE:GeodesicActiveContourImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourLeftVentricleTest.png
        81 114 5.0 1.0 -0.5 3.0 2.0
)

itk_add_test(NAME GeodesicActiveContourRightVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/GeodesicActiveContourRightVentricleTest.png,:}
            ${TEMP}/GeodesicActiveContourRightVentricleTest.png
  $<TARGET_FILE:GeodesicActiveContourImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourRightVentricleTest.png
        99 114 5.0 1.0 -0.5 3.0 2.0
)

itk_add_test(NAME GeodesicActiveContourWhiteMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/GeodesicActiveContourWhiteMatterTest.png,:}
            ${TEMP}/GeodesicActiveContourWhiteMatterTest.png
  $<TARGET_FILE:GeodesicActiveContourImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourWhiteMatterTest.png
        56 92 5.0 1.0 -0.3 2.0 10.0
)

itk_add_test(NAME GeodesicActiveContourGrayMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/GeodesicActiveContourGrayMatterTest.png,:}
            ${TEMP}/GeodesicActiveContourGrayMatterTest.png
  $<TARGET_FILE:GeodesicActiveContourImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourGrayMatterTest.png
        40 90 5.0 .5 -0.3 2.0 10.0
)

itk_add_test(NAME ShapeDetectionLevelSetLeftVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ShapeDetectionLevelSetLeftVentricleTest.png,:}
            ${TEMP}/ShapeDetectionLevelSetLeftVentricleTest.png
  $<TARGET_FILE:ShapeDetectionLevelSetFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetLeftVentricleTest.png
        81 114 5.0 1.0 -0.5 3.0 0.05 1.0
)

itk_add_test(NAME ShapeDetectionLevelSetRightVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ShapeDetectionLevelSetRightVentricleTest.png,:}
            ${TEMP}/ShapeDetectionLevelSetRightVentricleTest.png
  $<TARGET_FILE:ShapeDetectionLevelSetFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetRightVentricleTest.png
        99 114 5.0 1.0 -0.5 3.0 0.05 1.0
)

itk_add_test(NAME ShapeDetectionLevelSetWhiteMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ShapeDetectionLevelSetWhiteMatterTest.png,:}
            ${TEMP}/ShapeDetectionLevelSetWhiteMatterTest.png
  $<TARGET_FILE:ShapeDetectionLevelSetFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetWhiteMatterTest.png
        56 92 5.0 1.0 -0.3 2.0 0.05 1.0
)

itk_add_test(NAME ShapeDetectionLevelSetGrayMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ShapeDetectionLevelSetGrayMatterTest.png,:}
            ${TEMP}/ShapeDetectionLevelSetGrayMatterTest.png
  $<TARGET_FILE:ShapeDetectionLevelSetFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetGrayMatterTest.png
        40 90 5.0 1.0 -0.3 2.0 0.05 1.0
)

itk_add_test(NAME ThresholdSegmentationLevelSetVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ThresholdSegmentationLevelSetVentricleTest.png,:}
            ${TEMP}/ThresholdSegmentationLevelSetVentricleTest.png
  $<TARGET_FILE:ThresholdSegmentationLevelSetImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdSegmentationLevelSetVentricleTest.png
        81 112 5 210 250
)

itk_add_test(NAME ThresholdSegmentationLevelSetWhiteMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ThresholdSegmentationLevelSetWhiteMatterTest.png,:}
            ${TEMP}/ThresholdSegmentationLevelSetWhiteMatterTest.png
  --compareNumberOfPixelsTolerance 40
  $<TARGET_FILE:ThresholdSegmentationLevelSetImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdSegmentationLevelSetWhiteMatterTest.png
        60 116 5 150 180
)

itk_add_test(NAME ThresholdSegmentationLevelSetGrayMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ThresholdSegmentationLevelSetGrayMatterTest.png,:}
            ${TEMP}/ThresholdSegmentationLevelSetGrayMatterTest.png
  $<TARGET_FILE:ThresholdSegmentationLevelSetImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdSegmentationLevelSetGrayMatterTest.png
        107 69 5 180 210
)

itk_add_test(NAME CellularSegmentation1Test COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:CellularSegmentation1>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        100 115 200 255 3000
        ${TEMP}/CellularSegmentation1Test.vtk
)

if( ITK_USE_BRAINWEB_DATA )

itk_add_test(NAME CellularSegmentation2Test COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:CellularSegmentation2>
        DATA{../../Data/BrainWeb/brainweb1e1a10f20.mha}
        81 113 96 220 255 2000
        ${TEMP}/CellularSegmentation2Test.vtk
)

endif()

itk_add_test(NAME VectorConfidenceConnectedTest1 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/VectorConfidenceConnected1.png}
            ${TEMP}/VectorConfidenceConnected1.png
  $<TARGET_FILE:VectorConfidenceConnected>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/VectorConfidenceConnected1.png
        70 120 7 1
)

itk_add_test(NAME VectorConfidenceConnectedTest2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/VectorConfidenceConnected2.png}
            ${TEMP}/VectorConfidenceConnected2.png
  $<TARGET_FILE:VectorConfidenceConnected>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/VectorConfidenceConnected2.png
        23 93 7 1
)

itk_add_test(NAME VectorConfidenceConnectedTest3 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/VectorConfidenceConnected3.png}
            ${TEMP}/VectorConfidenceConnected3.png
  $<TARGET_FILE:VectorConfidenceConnected>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/VectorConfidenceConnected3.png
        66 66 3 1
)


itk_add_test(NAME CannySegmentationLevelSetImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/CannySegmentationLevelSetImageFilterTest.png}
            ${TEMP}/CannySegmentationLevelSetImageFilterTest.png
  $<TARGET_FILE:CannySegmentationLevelSetImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        DATA{${BASELINE}/ThresholdSegmentationLevelSetVentricle.png}
        ${TEMP}/CannySegmentationLevelSetImageFilterTest.png
        7 0.1 -10.0 127.5 10000
)

itk_add_test(NAME GeodesicActiveContourShapePriorCallosumTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/GeodesicActiveContourShapePriorCallosumTest.png}
            ${TEMP}/GeodesicActiveContourShapePriorCallosumTest.png
  $<TARGET_FILE:GeodesicActiveContourShapePriorLevelSetImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainMidSagittalSlice.png
        ${TEMP}/GeodesicActiveContourShapePriorCallosumTest.png
        120 85 60 102 88 83 6 1.0 0.5 0.02
        ${ITK_SOURCE_DIR}/Examples/Data/CorpusCallosumMeanShape.mha 3
        ${ITK_SOURCE_DIR}/Examples/Data/CorpusCallosumMode%d.mha 10 0
)

itk_add_test(NAME NeighborhoodConnectedWhiteTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/NeighborhoodConnectedWhiteTest.png}
            ${TEMP}/NeighborhoodConnectedWhiteTest.png
  $<TARGET_FILE:NeighborhoodConnectedImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/NeighborhoodConnectedWhiteTest.png
        60 116 150 180
)

itk_add_test(NAME NeighborhoodConnectedVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/NeighborhoodConnectedVentricleTest.png}
            ${TEMP}/NeighborhoodConnectedVentricleTest.png
  $<TARGET_FILE:NeighborhoodConnectedImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/NeighborhoodConnectedVentricleTest.png
        81 112 210 250
)

itk_add_test(NAME NeighborhoodConnectedGrayTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/NeighborhoodConnectedGrayTest.png}
            ${TEMP}/NeighborhoodConnectedGrayTest.png
  $<TARGET_FILE:NeighborhoodConnectedImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/NeighborhoodConnectedGrayTest.png
        107 69 180 210
)

itk_add_test(NAME IsolatedConnectedTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/IsolatedConnectedTest.png}
            ${TEMP}/IsolatedConnectedTest.png
  $<TARGET_FILE:IsolatedConnectedImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/IsolatedConnectedTest.png
        61 140 150 63 43
)

itk_add_test(NAME RelabelComponentImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/RelabelComponentImageFilter.png}
            ${TEMP}/RelabelComponentImageFilter.png
  $<TARGET_FILE:RelabelComponentImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/WatershedSegmentation.png
        ${TEMP}/RelabelComponentImageFilter.png
)

itk_add_test(NAME WatershedSegmentation1Test1 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/WatershedSegmentation1Test1.png}
            ${TEMP}/WatershedSegmentation1Test1.png
  $<TARGET_FILE:WatershedSegmentation1>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/WatershedSegmentation1Test1.png
        2.0 10 0.0 0.05 1
)

itk_add_test(NAME WatershedSegmentation1Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/WatershedSegmentation1Test2.png}
            ${TEMP}/WatershedSegmentation1Test2.png
  $<TARGET_FILE:WatershedSegmentation1>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/WatershedSegmentation1Test2.png
        2.0 10 0.001 0.15 0
)

itk_add_test(NAME CurvesLevelSetImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/CurvesLevelSetImageFilterTest.png}
            ${TEMP}/CurvesLevelSetImageFilterTest.png
  $<TARGET_FILE:CurvesLevelSetImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/CurvesLevelSetImageFilterTest.png
        81 114 5.0 1.0 -0.5 3.0 2.0
)

itk_add_test(NAME HoughTransformTwo2DLinesTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/HoughTransformTwoLinesTest.png}
            ${TEMP}/HoughTransformTwoLinesTest.png
  $<TARGET_FILE:HoughTransform2DLinesImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/HoughTransformTwoLines.png
        ${TEMP}/HoughTransformTwoLinesTest.png
        2 5 10
)

itk_add_test(NAME HoughTransformTwo2DCirclesTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/HoughTransformTwoCirclesTest.png}
            ${TEMP}/HoughTransformTwoCirclesTest.png
  $<TARGET_FILE:HoughTransform2DCirclesImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/HoughTransformTwoCircles.png
        ${TEMP}/HoughTransformTwoCirclesTest.png
        2 6 20 3.14 10 5 1.5
)

itk_add_test(NAME LaplacianSegmentationLevelSetImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/LaplacianSegmentationLevelSetImageFilter.png}
            ${TEMP}/LaplacianSegmentationLevelSetImageFilter.png
  $<TARGET_FILE:LaplacianSegmentationLevelSetImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        DATA{${BASELINE}/ThresholdSegmentationLevelSetVentricle.png}
        ${TEMP}/LaplacianSegmentationLevelSetImageFilter.png
        10 2.0 1 127.5 15
)
