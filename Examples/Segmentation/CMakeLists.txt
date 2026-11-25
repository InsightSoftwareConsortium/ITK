project(ImageSegmentation)

add_executable(ConfidenceConnected ConfidenceConnected.cxx)
target_link_libraries(
  ConfidenceConnected
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCurvatureFlowModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKRegionGrowingModule
)

add_executable(ConfidenceConnected3D ConfidenceConnected3D.cxx)
target_link_libraries(
  ConfidenceConnected3D
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCurvatureFlowModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKRegionGrowingModule
)

add_executable(
  NeighborhoodConnectedImageFilter
  NeighborhoodConnectedImageFilter.cxx
)
target_link_libraries(
  NeighborhoodConnectedImageFilter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKCurvatureFlowModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKRegionGrowingModule
)

add_executable(IsolatedConnectedImageFilter IsolatedConnectedImageFilter.cxx)
target_link_libraries(
  IsolatedConnectedImageFilter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKCurvatureFlowModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKRegionGrowingModule
)

add_executable(ConnectedThresholdImageFilter ConnectedThresholdImageFilter.cxx)
target_link_libraries(
  ConnectedThresholdImageFilter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKCurvatureFlowModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKRegionGrowingModule
)

add_executable(FastMarchingImageFilter FastMarchingImageFilter.cxx)
target_link_libraries(
  FastMarchingImageFilter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKAnisotropicSmoothingModule
    ITK::ITKFastMarchingModule
    ITK::ITKImageGradientModule
    ITK::ITKImageIntensityModule
    ITK::ITKThresholdingModule
)

add_executable(
  GeodesicActiveContourShapePriorLevelSetImageFilter
  GeodesicActiveContourShapePriorLevelSetImageFilter.cxx
)
target_link_libraries(
  GeodesicActiveContourShapePriorLevelSetImageFilter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKAnisotropicSmoothingModule
    ITK::ITKCommonModule
    ITK::ITKFastMarchingModule
    ITK::ITKImageGradientModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKLevelSetsModule
    ITK::ITKSpatialFunctionModule
    ITK::ITKSpatialObjectsModule
    ITK::ITKThresholdingModule
)

add_executable(GibbsPriorImageFilter1 GibbsPriorImageFilter1.cxx)
target_link_libraries(
  GibbsPriorImageFilter1
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKClassifiersModule
    ITK::ITKCommonModule
    ITK::ITKMarkovRandomFieldsClassifiersModule
    ITK::ITKStatisticsModule
)

add_executable(
  HoughTransform2DCirclesImageFilter
  HoughTransform2DCirclesImageFilter.cxx
)
target_link_libraries(
  HoughTransform2DCirclesImageFilter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFeatureModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGradientModule
    ITK::ITKSmoothingModule
    ITK::ITKThresholdingModule
)

add_executable(
  HoughTransform2DLinesImageFilter
  HoughTransform2DLinesImageFilter.cxx
)
target_link_libraries(
  HoughTransform2DLinesImageFilter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFeatureModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGradientModule
    ITK::ITKSmoothingModule
    ITK::ITKThresholdingModule
)

add_executable(VectorConfidenceConnected VectorConfidenceConnected.cxx)
target_link_libraries(
  VectorConfidenceConnected
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKRegionGrowingModule
)

add_executable(RelabelComponentImageFilter RelabelComponentImageFilter.cxx)
target_link_libraries(
  RelabelComponentImageFilter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKConnectedComponentsModule
)

add_executable(
  CannySegmentationLevelSetImageFilter
  CannySegmentationLevelSetImageFilter.cxx
)
target_link_libraries(
  CannySegmentationLevelSetImageFilter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKAnisotropicSmoothingModule
    ITK::ITKCommonModule
    ITK::ITKFastMarchingModule
    ITK::ITKImageFeatureModule
    ITK::ITKLevelSetsModule
    ITK::ITKThresholdingModule
)

add_executable(
  GeodesicActiveContourImageFilter
  GeodesicActiveContourImageFilter.cxx
)
target_link_libraries(
  GeodesicActiveContourImageFilter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKAnisotropicSmoothingModule
    ITK::ITKFastMarchingModule
    ITK::ITKImageGradientModule
    ITK::ITKImageIntensityModule
    ITK::ITKLevelSetsModule
    ITK::ITKThresholdingModule
)

add_executable(ShapeDetectionLevelSetFilter ShapeDetectionLevelSetFilter.cxx)
target_link_libraries(
  ShapeDetectionLevelSetFilter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKAnisotropicSmoothingModule
    ITK::ITKFastMarchingModule
    ITK::ITKImageGradientModule
    ITK::ITKImageIntensityModule
    ITK::ITKLevelSetsModule
    ITK::ITKThresholdingModule
)

add_executable(CurvesLevelSetImageFilter CurvesLevelSetImageFilter.cxx)
target_link_libraries(
  CurvesLevelSetImageFilter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKAnisotropicSmoothingModule
    ITK::ITKFastMarchingModule
    ITK::ITKImageGradientModule
    ITK::ITKImageIntensityModule
    ITK::ITKLevelSetsModule
    ITK::ITKThresholdingModule
)

add_executable(WatershedSegmentation1 WatershedSegmentation1.cxx)
target_link_libraries(
  WatershedSegmentation1
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKAnisotropicSmoothingModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageFusionModule
    ITK::ITKImageGradientModule
    ITK::ITKWatershedsModule
)

add_executable(WatershedSegmentation2 WatershedSegmentation2.cxx)
target_link_libraries(
  WatershedSegmentation2
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFusionModule
    ITK::ITKImageGradientModule
    ITK::ITKWatershedsModule
)

add_executable(
  LaplacianSegmentationLevelSetImageFilter
  LaplacianSegmentationLevelSetImageFilter.cxx
)
target_link_libraries(
  LaplacianSegmentationLevelSetImageFilter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKAnisotropicSmoothingModule
    ITK::ITKCommonModule
    ITK::ITKFastMarchingModule
    ITK::ITKImageFeatureModule
    ITK::ITKLevelSetsModule
    ITK::ITKThresholdingModule
)

add_executable(
  ThresholdSegmentationLevelSetImageFilter
  ThresholdSegmentationLevelSetImageFilter.cxx
)
target_link_libraries(
  ThresholdSegmentationLevelSetImageFilter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKFastMarchingModule
    ITK::ITKImageFeatureModule
    ITK::ITKLevelSetsModule
    ITK::ITKThresholdingModule
)

if(BUILD_TESTING)
  add_subdirectory(test)
endif()
