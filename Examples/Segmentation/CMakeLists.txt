#
# Examples on the use of segmentation algorithms
#

project(ImageSegmentation)
include_regular_expression("^.*$")

add_executable(ConfidenceConnected ConfidenceConnected.cxx )
target_link_libraries(ConfidenceConnected ${ITK_LIBRARIES})

add_executable(ConfidenceConnected3D ConfidenceConnected3D.cxx )
target_link_libraries(ConfidenceConnected3D ${ITK_LIBRARIES})

add_executable(NeighborhoodConnectedImageFilter NeighborhoodConnectedImageFilter.cxx )
target_link_libraries(NeighborhoodConnectedImageFilter ${ITK_LIBRARIES})

add_executable(IsolatedConnectedImageFilter IsolatedConnectedImageFilter.cxx )
target_link_libraries(IsolatedConnectedImageFilter ${ITK_LIBRARIES})

add_executable(ConnectedThresholdImageFilter ConnectedThresholdImageFilter.cxx )
target_link_libraries(ConnectedThresholdImageFilter ${ITK_LIBRARIES})

add_executable(FastMarchingImageFilter FastMarchingImageFilter.cxx )
target_link_libraries(FastMarchingImageFilter ${ITK_LIBRARIES})

add_executable(GeodesicActiveContourShapePriorLevelSetImageFilter
GeodesicActiveContourShapePriorLevelSetImageFilter.cxx )
target_link_libraries(GeodesicActiveContourShapePriorLevelSetImageFilter ${ITK_LIBRARIES})

add_executable(DeformableModel2 DeformableModel2.cxx )
target_link_libraries(DeformableModel2 ${ITK_LIBRARIES})

add_executable(GibbsPriorImageFilter1 GibbsPriorImageFilter1.cxx )
target_link_libraries(GibbsPriorImageFilter1 ${ITK_LIBRARIES})

add_executable(HoughTransform2DCirclesImageFilter HoughTransform2DCirclesImageFilter.cxx )
target_link_libraries(HoughTransform2DCirclesImageFilter ${ITK_LIBRARIES})

add_executable(HoughTransform2DLinesImageFilter HoughTransform2DLinesImageFilter.cxx )
target_link_libraries(HoughTransform2DLinesImageFilter ${ITK_LIBRARIES})

add_executable(VectorConfidenceConnected VectorConfidenceConnected.cxx )
target_link_libraries(VectorConfidenceConnected ${ITK_LIBRARIES})

add_executable(RelabelComponentImageFilter RelabelComponentImageFilter.cxx )
target_link_libraries(RelabelComponentImageFilter ${ITK_LIBRARIES})

add_executable(CannySegmentationLevelSetImageFilter CannySegmentationLevelSetImageFilter.cxx )
target_link_libraries(CannySegmentationLevelSetImageFilter ${ITK_LIBRARIES})

add_executable(GeodesicActiveContourImageFilter GeodesicActiveContourImageFilter.cxx )
target_link_libraries(GeodesicActiveContourImageFilter ${ITK_LIBRARIES})

add_executable(ShapeDetectionLevelSetFilter ShapeDetectionLevelSetFilter.cxx )
target_link_libraries(ShapeDetectionLevelSetFilter ${ITK_LIBRARIES})

add_executable(CurvesLevelSetImageFilter CurvesLevelSetImageFilter.cxx )
target_link_libraries(CurvesLevelSetImageFilter ${ITK_LIBRARIES})

add_executable(WatershedSegmentation1 WatershedSegmentation1.cxx )
target_link_libraries(WatershedSegmentation1 ${ITK_LIBRARIES})

add_executable(WatershedSegmentation2 WatershedSegmentation2.cxx )
target_link_libraries(WatershedSegmentation2 ${ITK_LIBRARIES})

add_executable(CellularSegmentation1 CellularSegmentation1.cxx )
target_link_libraries(CellularSegmentation1 ${ITK_LIBRARIES})

add_executable(CellularSegmentation2 CellularSegmentation2.cxx )
target_link_libraries(CellularSegmentation2 ${ITK_LIBRARIES})

add_executable(DeformableModel1 DeformableModel1.cxx )
target_link_libraries(DeformableModel1 ${ITK_LIBRARIES})

add_executable(LaplacianSegmentationLevelSetImageFilter LaplacianSegmentationLevelSetImageFilter.cxx )
target_link_libraries(LaplacianSegmentationLevelSetImageFilter ${ITK_LIBRARIES})

add_executable(ThresholdSegmentationLevelSetImageFilter ThresholdSegmentationLevelSetImageFilter.cxx )
target_link_libraries(ThresholdSegmentationLevelSetImageFilter ${ITK_LIBRARIES})


set(BASELINE ${ITK_DATA_ROOT}/Baseline/Segmentation)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)


if( NOT ITK_DISABLE_CXX_TESTING )

if(BUILD_TESTING)

itk_add_test(NAME ConfidenceConnectedWhiteTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ConfidenceConnectedWhiteTest.png
            ${TEMP}/ConfidenceConnectedWhiteTest.png
  $<TARGET_FILE:ConfidenceConnected>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConfidenceConnectedWhiteTest.png
        60 116
)

itk_add_test(NAME ConfidenceConnectedVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ConfidenceConnectedVentricleTest.png
            ${TEMP}/ConfidenceConnectedVentricleTest.png
  $<TARGET_FILE:ConfidenceConnected>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConfidenceConnectedVentricleTest.png
        81 112
)

itk_add_test(NAME ConfidenceConnectedGrayTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ConfidenceConnectedGrayTest.png
            ${TEMP}/ConfidenceConnectedGrayTest.png
  $<TARGET_FILE:ConfidenceConnected>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConfidenceConnectedGrayTest.png
        107 69
)

itk_add_test(NAME ConnectedThresholdWhiteTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ConnectedThresholdWhiteTest.png
            ${TEMP}/ConnectedThresholdWhiteTest.png
  $<TARGET_FILE:ConnectedThresholdImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConnectedThresholdWhiteTest.png
        60 116 150 180
)

itk_add_test(NAME ConnectedThresholdVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ConnectedThresholdVentricleTest.png
            ${TEMP}/ConnectedThresholdVentricleTest.png
  $<TARGET_FILE:ConnectedThresholdImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConnectedThresholdVentricleTest.png
        81 112 210 250
)

itk_add_test(NAME ConnectedThresholdGrayTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ConnectedThresholdGrayTest.png
            ${TEMP}/ConnectedThresholdGrayTest.png
  $<TARGET_FILE:ConnectedThresholdImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ConnectedThresholdGrayTest.png
        107 69 180 210
)

itk_add_test(NAME FastMarchingLeftVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/FastMarchingLeftVentricleTest.png
            ${TEMP}/FastMarchingLeftVentricleTest.png
  $<TARGET_FILE:FastMarchingImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingLeftVentricleTest.png
        81 114 1.0 -0.5 3.0 100 100
)

itk_add_test(NAME FastMarchingRightVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/FastMarchingRightVentricleTest.png
            ${TEMP}/FastMarchingRightVentricleTest.png
  $<TARGET_FILE:FastMarchingImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingRightVentricleTest.png
        99 114 1.0 -0.5 3.0 100 100
)

itk_add_test(NAME FastMarchingWhiteMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/FastMarchingWhiteMatterTest.png
            ${TEMP}/FastMarchingWhiteMatterTest.png
  $<TARGET_FILE:FastMarchingImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingWhiteMatterTest.png
        56 92 1.0 -0.3 2.0 200 100
)

itk_add_test(NAME FastMarchingGrayMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/FastMarchingGrayMatterTest.png
            ${TEMP}/FastMarchingGrayMatterTest.png
  $<TARGET_FILE:FastMarchingImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/FastMarchingGrayMatterTest.png
        40 90 .5 -0.3 2.0 200 100
)

itk_add_test(NAME GeodesicActiveContourLeftVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/GeodesicActiveContourLeftVentricleTest.png
            ${TEMP}/GeodesicActiveContourLeftVentricleTest.png
  $<TARGET_FILE:GeodesicActiveContourImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourLeftVentricleTest.png
        81 114 5.0 1.0 -0.5 3.0 2.0
)

itk_add_test(NAME GeodesicActiveContourRightVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/GeodesicActiveContourRightVentricleTest.png
            ${TEMP}/GeodesicActiveContourRightVentricleTest.png
  $<TARGET_FILE:GeodesicActiveContourImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourRightVentricleTest.png
        99 114 5.0 1.0 -0.5 3.0 2.0
)

itk_add_test(NAME GeodesicActiveContourWhiteMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/GeodesicActiveContourWhiteMatterTest.png
            ${TEMP}/GeodesicActiveContourWhiteMatterTest.png
  $<TARGET_FILE:GeodesicActiveContourImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourWhiteMatterTest.png
        56 92 5.0 1.0 -0.3 2.0 10.0
)

itk_add_test(NAME GeodesicActiveContourGrayMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/GeodesicActiveContourGrayMatterTest.png
            ${TEMP}/GeodesicActiveContourGrayMatterTest.png
  $<TARGET_FILE:GeodesicActiveContourImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GeodesicActiveContourGrayMatterTest.png
        40 90 5.0 .5 -0.3 2.0 10.0
)

itk_add_test(NAME ShapeDetectionLevelSetLeftVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ShapeDetectionLevelSetLeftVentricleTest.png
            ${TEMP}/ShapeDetectionLevelSetLeftVentricleTest.png
  $<TARGET_FILE:ShapeDetectionLevelSetFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetLeftVentricleTest.png
        81 114 5.0 1.0 -0.5 3.0 0.05 1.0
)

itk_add_test(NAME ShapeDetectionLevelSetRightVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ShapeDetectionLevelSetRightVentricleTest.png
            ${TEMP}/ShapeDetectionLevelSetRightVentricleTest.png
  $<TARGET_FILE:ShapeDetectionLevelSetFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetRightVentricleTest.png
        99 114 5.0 1.0 -0.5 3.0 0.05 1.0
)

itk_add_test(NAME ShapeDetectionLevelSetWhiteMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ShapeDetectionLevelSetWhiteMatterTest.png
            ${TEMP}/ShapeDetectionLevelSetWhiteMatterTest.png
  $<TARGET_FILE:ShapeDetectionLevelSetFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetWhiteMatterTest.png
        56 92 5.0 1.0 -0.3 2.0 0.05 1.0
)

itk_add_test(NAME ShapeDetectionLevelSetGrayMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ShapeDetectionLevelSetGrayMatterTest.png
            ${TEMP}/ShapeDetectionLevelSetGrayMatterTest.png
  $<TARGET_FILE:ShapeDetectionLevelSetFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ShapeDetectionLevelSetGrayMatterTest.png
        40 90 5.0 1.0 -0.3 2.0 0.05 1.0
)

itk_add_test(NAME ThresholdSegmentationLevelSetVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ThresholdSegmentationLevelSetVentricleTest.png
            ${TEMP}/ThresholdSegmentationLevelSetVentricleTest.png
  $<TARGET_FILE:ThresholdSegmentationLevelSetImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdSegmentationLevelSetVentricleTest.png
        81 112 5 210 250
)

itk_add_test(NAME ThresholdSegmentationLevelSetWhiteMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ThresholdSegmentationLevelSetWhiteMatterTest.png
            ${TEMP}/ThresholdSegmentationLevelSetWhiteMatterTest.png
  $<TARGET_FILE:ThresholdSegmentationLevelSetImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdSegmentationLevelSetWhiteMatterTest.png
        60 116 5 150 180
)

itk_add_test(NAME ThresholdSegmentationLevelSetGrayMatterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ThresholdSegmentationLevelSetGrayMatterTest.png
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
        DATA{../Data/BrainWeb/brainweb1e1a10f20.mha}
        81 113 96 220 255 2000
        ${TEMP}/CellularSegmentation2Test.vtk
)

endif( ITK_USE_BRAINWEB_DATA )

itk_add_test(NAME VectorConfidenceConnectedTest1 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/VectorConfidenceConnected1.png
            ${TEMP}/VectorConfidenceConnected1.png
  $<TARGET_FILE:VectorConfidenceConnected>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/VectorConfidenceConnected1.png
        70 120 7 1
)

itk_add_test(NAME VectorConfidenceConnectedTest2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/VectorConfidenceConnected2.png
            ${TEMP}/VectorConfidenceConnected2.png
  $<TARGET_FILE:VectorConfidenceConnected>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/VectorConfidenceConnected2.png
        23 93 7 1
)

itk_add_test(NAME VectorConfidenceConnectedTest3 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/VectorConfidenceConnected3.png
            ${TEMP}/VectorConfidenceConnected3.png
  $<TARGET_FILE:VectorConfidenceConnected>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/VectorConfidenceConnected3.png
        66 66 3 1
)


itk_add_test(NAME CannySegmentationLevelSetImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/CannySegmentationLevelSetImageFilterTest.png
            ${TEMP}/CannySegmentationLevelSetImageFilterTest.png
  $<TARGET_FILE:CannySegmentationLevelSetImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${BASELINE}/ThresholdSegmentationLevelSetVentricle.png
        ${TEMP}/CannySegmentationLevelSetImageFilterTest.png
        7 0.1 -10.0 127.5 10000
)

itk_add_test(NAME GeodesicActiveContourShapePriorCallosumTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/GeodesicActiveContourShapePriorCallosumTest.png
            ${TEMP}/GeodesicActiveContourShapePriorCallosumTest.png
  $<TARGET_FILE:GeodesicActiveContourShapePriorLevelSetImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainMidSagittalSlice.png
        ${TEMP}/GeodesicActiveContourShapePriorCallosumTest.png
        120 85 60 102 88 83 6 1.0 0.5 0.02
        ${ITK_SOURCE_DIR}/Examples/Data/CorpusCallosumMeanShape.mha 3
        ${ITK_SOURCE_DIR}/Examples/Data/CorpusCallosumMode%d.mha 10 0
)

itk_add_test(NAME NeighborhoodConnectedWhiteTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/NeighborhoodConnectedWhiteTest.png
            ${TEMP}/NeighborhoodConnectedWhiteTest.png
  $<TARGET_FILE:NeighborhoodConnectedImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/NeighborhoodConnectedWhiteTest.png
        60 116 150 180
)

itk_add_test(NAME NeighborhoodConnectedVentricleTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/NeighborhoodConnectedVentricleTest.png
            ${TEMP}/NeighborhoodConnectedVentricleTest.png
  $<TARGET_FILE:NeighborhoodConnectedImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/NeighborhoodConnectedVentricleTest.png
        81 112 210 250
)

itk_add_test(NAME NeighborhoodConnectedGrayTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/NeighborhoodConnectedGrayTest.png
            ${TEMP}/NeighborhoodConnectedGrayTest.png
  $<TARGET_FILE:NeighborhoodConnectedImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/NeighborhoodConnectedGrayTest.png
        107 69 180 210
)

itk_add_test(NAME IsolatedConnectedTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/IsolatedConnectedTest.png
            ${TEMP}/IsolatedConnectedTest.png
  $<TARGET_FILE:IsolatedConnectedImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/IsolatedConnectedTest.png
        61 140 150 63 43
)

itk_add_test(NAME RelabelComponentImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/RelabelComponentImageFilter.png
            ${TEMP}/RelabelComponentImageFilter.png
  $<TARGET_FILE:RelabelComponentImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/WatershedSegmentation.png
        ${TEMP}/RelabelComponentImageFilter.png
)

itk_add_test(NAME WatershedSegmentation1Test1 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/WatershedSegmentation1Test1.png
            ${TEMP}/WatershedSegmentation1Test1.png
  $<TARGET_FILE:WatershedSegmentation1>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/WatershedSegmentation1Test1.png
        2.0 10 0.0 0.05 1
)

itk_add_test(NAME WatershedSegmentation1Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/WatershedSegmentation1Test2.png
            ${TEMP}/WatershedSegmentation1Test2.png
  $<TARGET_FILE:WatershedSegmentation1>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/WatershedSegmentation1Test2.png
        2.0 10 0.001 0.15 0
)

itk_add_test(NAME CurvesLevelSetImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/CurvesLevelSetImageFilterTest.png
            ${TEMP}/CurvesLevelSetImageFilterTest.png
  $<TARGET_FILE:CurvesLevelSetImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/CurvesLevelSetImageFilterTest.png
        81 114 5.0 1.0 -0.5 3.0 2.0
)

itk_add_test(NAME HoughTransformTwo2DLinesTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/HoughTransformTwoLinesTest.png
            ${TEMP}/HoughTransformTwoLinesTest.png
  $<TARGET_FILE:HoughTransform2DLinesImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/HoughTransformTwoLines.png
        ${TEMP}/HoughTransformTwoLinesTest.png
        2 5 10
)

itk_add_test(NAME HoughTransformTwo2DCirclesTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/HoughTransformTwoCirclesTest.png
            ${TEMP}/HoughTransformTwoCirclesTest.png
  $<TARGET_FILE:HoughTransform2DCirclesImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/HoughTransformTwoCircles.png
        ${TEMP}/HoughTransformTwoCirclesTest.png
        2 6 20 3.14 10 5 1.5
)

itk_add_test(NAME LaplacianSegmentationLevelSetImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/LaplacianSegmentationLevelSetImageFilter.png
            ${TEMP}/LaplacianSegmentationLevelSetImageFilter.png
  $<TARGET_FILE:LaplacianSegmentationLevelSetImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${BASELINE}/ThresholdSegmentationLevelSetVentricle.png
        ${TEMP}/LaplacianSegmentationLevelSetImageFilter.png
        10 2.0 1 127.5 15
)

endif(BUILD_TESTING)

endif( NOT ITK_DISABLE_CXX_TESTING )


