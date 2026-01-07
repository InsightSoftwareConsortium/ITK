project(ImageSegmentation)

add_executable(ConfidenceConnected ConfidenceConnected.cxx)
target_link_libraries(
  ConfidenceConnected
  PRIVATE
    ITKImageIO
    ITKCurvatureFlowModule
    ITKImageFilterBaseModule
    ITKRegionGrowingModule
)

add_executable(ConfidenceConnected3D ConfidenceConnected3D.cxx)
target_link_libraries(
  ConfidenceConnected3D
  PRIVATE
    ITKImageIO
    ITKCurvatureFlowModule
    ITKImageFilterBaseModule
    ITKRegionGrowingModule
)

add_executable(
  NeighborhoodConnectedImageFilter
  NeighborhoodConnectedImageFilter.cxx
)
target_link_libraries(
  NeighborhoodConnectedImageFilter
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKCurvatureFlowModule
    ITKImageFilterBaseModule
    ITKRegionGrowingModule
)

add_executable(IsolatedConnectedImageFilter IsolatedConnectedImageFilter.cxx)
target_link_libraries(
  IsolatedConnectedImageFilter
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKCurvatureFlowModule
    ITKImageFilterBaseModule
    ITKRegionGrowingModule
)

add_executable(ConnectedThresholdImageFilter ConnectedThresholdImageFilter.cxx)
target_link_libraries(
  ConnectedThresholdImageFilter
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKCurvatureFlowModule
    ITKImageFilterBaseModule
    ITKRegionGrowingModule
)

add_executable(FastMarchingImageFilter FastMarchingImageFilter.cxx)
target_link_libraries(
  FastMarchingImageFilter
  PRIVATE
    ITKImageIO
    ITKAnisotropicSmoothingModule
    ITKFastMarchingModule
    ITKImageGradientModule
    ITKImageIntensityModule
    ITKThresholdingModule
)

add_executable(
  GeodesicActiveContourShapePriorLevelSetImageFilter
  GeodesicActiveContourShapePriorLevelSetImageFilter.cxx
)
target_link_libraries(
  GeodesicActiveContourShapePriorLevelSetImageFilter
  PRIVATE
    ITKImageIO
    ITKAnisotropicSmoothingModule
    ITKCommonModule
    ITKFastMarchingModule
    ITKImageGradientModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKLevelSetsModule
    ITKSpatialFunctionModule
    ITKSpatialObjectsModule
    ITKThresholdingModule
)

add_executable(GibbsPriorImageFilter1 GibbsPriorImageFilter1.cxx)
target_link_libraries(
  GibbsPriorImageFilter1
  PRIVATE
    ITKImageIO
    ITKClassifiersModule
    ITKCommonModule
    ITKMarkovRandomFieldsClassifiersModule
    ITKStatisticsModule
)

add_executable(
  HoughTransform2DCirclesImageFilter
  HoughTransform2DCirclesImageFilter.cxx
)
target_link_libraries(
  HoughTransform2DCirclesImageFilter
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFeatureModule
    ITKImageFilterBaseModule
    ITKImageGradientModule
    ITKSmoothingModule
    ITKThresholdingModule
)

add_executable(
  HoughTransform2DLinesImageFilter
  HoughTransform2DLinesImageFilter.cxx
)
target_link_libraries(
  HoughTransform2DLinesImageFilter
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFeatureModule
    ITKImageFilterBaseModule
    ITKImageGradientModule
    ITKSmoothingModule
    ITKThresholdingModule
)

add_executable(VectorConfidenceConnected VectorConfidenceConnected.cxx)
target_link_libraries(
  VectorConfidenceConnected
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKRegionGrowingModule
)

add_executable(RelabelComponentImageFilter RelabelComponentImageFilter.cxx)
target_link_libraries(
  RelabelComponentImageFilter
  PRIVATE
    ITKImageIO
    ITKConnectedComponentsModule
)

add_executable(
  CannySegmentationLevelSetImageFilter
  CannySegmentationLevelSetImageFilter.cxx
)
target_link_libraries(
  CannySegmentationLevelSetImageFilter
  PRIVATE
    ITKImageIO
    ITKAnisotropicSmoothingModule
    ITKCommonModule
    ITKFastMarchingModule
    ITKImageFeatureModule
    ITKLevelSetsModule
    ITKThresholdingModule
)

add_executable(
  GeodesicActiveContourImageFilter
  GeodesicActiveContourImageFilter.cxx
)
target_link_libraries(
  GeodesicActiveContourImageFilter
  PRIVATE
    ITKImageIO
    ITKAnisotropicSmoothingModule
    ITKFastMarchingModule
    ITKImageGradientModule
    ITKImageIntensityModule
    ITKLevelSetsModule
    ITKThresholdingModule
)

add_executable(ShapeDetectionLevelSetFilter ShapeDetectionLevelSetFilter.cxx)
target_link_libraries(
  ShapeDetectionLevelSetFilter
  PRIVATE
    ITKImageIO
    ITKAnisotropicSmoothingModule
    ITKFastMarchingModule
    ITKImageGradientModule
    ITKImageIntensityModule
    ITKLevelSetsModule
    ITKThresholdingModule
)

add_executable(CurvesLevelSetImageFilter CurvesLevelSetImageFilter.cxx)
target_link_libraries(
  CurvesLevelSetImageFilter
  PRIVATE
    ITKImageIO
    ITKAnisotropicSmoothingModule
    ITKFastMarchingModule
    ITKImageGradientModule
    ITKImageIntensityModule
    ITKLevelSetsModule
    ITKThresholdingModule
)

add_executable(WatershedSegmentation1 WatershedSegmentation1.cxx)
target_link_libraries(
  WatershedSegmentation1
  PRIVATE
    ITKImageIO
    ITKAnisotropicSmoothingModule
    ITKImageFilterBaseModule
    ITKImageFusionModule
    ITKImageGradientModule
    ITKWatershedsModule
)

add_executable(WatershedSegmentation2 WatershedSegmentation2.cxx)
target_link_libraries(
  WatershedSegmentation2
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFusionModule
    ITKImageGradientModule
    ITKWatershedsModule
)

add_executable(
  LaplacianSegmentationLevelSetImageFilter
  LaplacianSegmentationLevelSetImageFilter.cxx
)
target_link_libraries(
  LaplacianSegmentationLevelSetImageFilter
  PRIVATE
    ITKImageIO
    ITKAnisotropicSmoothingModule
    ITKCommonModule
    ITKFastMarchingModule
    ITKImageFeatureModule
    ITKLevelSetsModule
    ITKThresholdingModule
)

add_executable(
  ThresholdSegmentationLevelSetImageFilter
  ThresholdSegmentationLevelSetImageFilter.cxx
)
target_link_libraries(
  ThresholdSegmentationLevelSetImageFilter
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKFastMarchingModule
    ITKImageFeatureModule
    ITKLevelSetsModule
    ITKThresholdingModule
)

if(BUILD_TESTING)
  add_subdirectory(test)
endif()
