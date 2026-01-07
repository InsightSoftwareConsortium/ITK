project(ITKv4ImageRegistration)

add_executable(ImageRegistration3 ImageRegistration3.cxx)
target_link_libraries(
  ImageRegistration3
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

add_executable(ImageRegistration5 ImageRegistration5.cxx)
target_link_libraries(
  ImageRegistration5
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

add_executable(ImageRegistration6 ImageRegistration6.cxx)
target_link_libraries(
  ImageRegistration6
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationCommonModule
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

add_executable(ImageRegistration8 ImageRegistration8.cxx)
target_link_libraries(
  ImageRegistration8
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationCommonModule
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

add_executable(ImageRegistration9 ImageRegistration9.cxx)
target_link_libraries(
  ImageRegistration9
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationCommonModule
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

add_executable(ImageRegistration10 ImageRegistration10.cxx)
target_link_libraries(
  ImageRegistration10
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageFunctionModule
    ITKImageGridModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKTransformModule
)

add_executable(DeformableRegistration4 DeformableRegistration4.cxx)
target_link_libraries(
  DeformableRegistration4
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKDisplacementFieldModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

add_executable(ImageRegistration4 ImageRegistration4.cxx)
target_link_libraries(
  ImageRegistration4
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

add_executable(ImageRegistration7 ImageRegistration7.cxx)
target_link_libraries(
  ImageRegistration7
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationCommonModule
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

add_executable(ImageRegistration11 ImageRegistration11.cxx)
target_link_libraries(
  ImageRegistration11
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationMethodsv4Module
    ITKStatisticsModule
    ITKTransformModule
)

add_executable(ImageRegistration12 ImageRegistration12.cxx)
target_link_libraries(
  ImageRegistration12
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationCommonModule
    ITKRegistrationMethodsv4Module
    ITKSpatialObjectsModule
    ITKTransformModule
)

add_executable(ImageRegistration13 ImageRegistration13.cxx)
target_link_libraries(
  ImageRegistration13
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationCommonModule
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

add_executable(ImageRegistration14 ImageRegistration14.cxx)
target_link_libraries(
  ImageRegistration14
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKStatisticsModule
    ITKTransformModule
)

add_executable(ImageRegistration15 ImageRegistration15.cxx)
target_link_libraries(
  ImageRegistration15
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKStatisticsModule
    ITKTransformModule
)

add_executable(ImageRegistration16 ImageRegistration16.cxx)
target_link_libraries(
  ImageRegistration16
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKTransformModule
)

add_executable(ImageRegistration18 ImageRegistration18.cxx)
target_link_libraries(
  ImageRegistration18
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKTransformModule
)

add_executable(ImageRegistration19 ImageRegistration19.cxx)
target_link_libraries(
  ImageRegistration19
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageFunctionModule
    ITKImageGridModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKTransformModule
)

add_executable(ImageRegistration20 ImageRegistration20.cxx)
target_link_libraries(
  ImageRegistration20
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKTransformModule
)

add_executable(DeformableRegistration6 DeformableRegistration6.cxx)
target_link_libraries(
  DeformableRegistration6
  PRIVATE
    ITKImageIO
    ITKDisplacementFieldModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationCommonModule
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

add_executable(DeformableRegistration7 DeformableRegistration7.cxx)
target_link_libraries(
  DeformableRegistration7
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKDisplacementFieldModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

add_executable(DeformableRegistration8 DeformableRegistration8.cxx)
target_link_libraries(
  DeformableRegistration8
  PRIVATE
    ITKImageIO
    ITKTransformIO
    ITKCommonModule
    ITKDisplacementFieldModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

if(ITK_USE_FFTWD AND NOT ITK_USE_CUFFTW)
  add_executable(DeformableRegistration9 DeformableRegistration9.cxx)
  target_link_libraries(DeformableRegistration9 ${ITK_LIBRARIES})

  add_executable(DeformableRegistration10 DeformableRegistration10.cxx)
  target_link_libraries(DeformableRegistration10 ${ITK_LIBRARIES})
endif()

add_executable(MultiResImageRegistration1 MultiResImageRegistration1.cxx)
target_link_libraries(
  MultiResImageRegistration1
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

add_executable(MultiResImageRegistration2 MultiResImageRegistration2.cxx)
target_link_libraries(
  MultiResImageRegistration2
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKTransformModule
)

add_executable(MultiResImageRegistration3 MultiResImageRegistration3.cxx)
target_link_libraries(
  MultiResImageRegistration3
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKTransformModule
)

add_executable(MultiStageImageRegistration1 MultiStageImageRegistration1.cxx)
target_link_libraries(
  MultiStageImageRegistration1
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

add_executable(MultiStageImageRegistration2 MultiStageImageRegistration2.cxx)
target_link_libraries(
  MultiStageImageRegistration2
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKImageStatisticsModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

if(ITKFEMRegistration_ENABLED)
  add_executable(DeformableRegistration1 DeformableRegistration1.cxx)
  target_link_libraries(DeformableRegistration1 ${ITK_LIBRARIES})

  add_executable(DeformableRegistration11 DeformableRegistration11.cxx)
  target_link_libraries(DeformableRegistration11 ${ITK_LIBRARIES})
endif()

add_executable(DeformableRegistration2 DeformableRegistration2.cxx)
target_link_libraries(
  DeformableRegistration2
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKDisplacementFieldModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKPDEDeformableRegistrationModule
)

add_executable(DeformableRegistration3 DeformableRegistration3.cxx)
target_link_libraries(
  DeformableRegistration3
  PRIVATE
    ITKImageIO
    ITKDisplacementFieldModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKPDEDeformableRegistrationModule
)

add_executable(DeformableRegistration5 DeformableRegistration5.cxx)
target_link_libraries(
  DeformableRegistration5
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKDisplacementFieldModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKPDEDeformableRegistrationModule
)

add_executable(DeformableRegistration12 DeformableRegistration12.cxx)
target_link_libraries(
  DeformableRegistration12
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

add_executable(DeformableRegistration13 DeformableRegistration13.cxx)
target_link_libraries(
  DeformableRegistration13
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKTransformModule
)

add_executable(DeformableRegistration14 DeformableRegistration14.cxx)
target_link_libraries(
  DeformableRegistration14
  PRIVATE
    ITKImageIO
    ITKTransformIO
    ITKCommonModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKTransformModule
)

add_executable(ChangeInformationImageFilter ChangeInformationImageFilter.cxx)
target_link_libraries(
  ChangeInformationImageFilter
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageGridModule
)

add_executable(DeformableRegistration15 DeformableRegistration15.cxx)
target_link_libraries(
  DeformableRegistration15
  PRIVATE
    ITKImageIO
    ITKTransformIO
    ITKCommonModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageFunctionModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKTransformModule
)

add_executable(DeformableRegistration16 DeformableRegistration16.cxx)
target_link_libraries(
  DeformableRegistration16
  PRIVATE
    ITKImageIO
    ITKDisplacementFieldModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKPDEDeformableRegistrationModule
    ITKRegistrationCommonModule
)

add_executable(DeformableRegistration17 DeformableRegistration17.cxx)
target_link_libraries(
  DeformableRegistration17
  PRIVATE
    ITKImageIO
    ITKDisplacementFieldModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKPDEDeformableRegistrationModule
    ITKRegistrationCommonModule
)

add_executable(
  DisplacementFieldInitialization
  DisplacementFieldInitialization.cxx
)
target_link_libraries(
  DisplacementFieldInitialization
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKDisplacementFieldModule
)

add_executable(LandmarkWarping2 LandmarkWarping2.cxx)
target_link_libraries(
  LandmarkWarping2
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKDisplacementFieldModule
    ITKImageGridModule
)

add_executable(BSplineWarping1 BSplineWarping1.cxx)
target_link_libraries(
  BSplineWarping1
  PRIVATE
    ITKImageIO
    ITKTransformIO
    ITKCommonModule
    ITKImageGridModule
    ITKTransformModule
)

add_executable(BSplineWarping2 BSplineWarping2.cxx)
target_link_libraries(
  BSplineWarping2
  PRIVATE
    ITKImageIO
    ITKTransformIO
    ITKCommonModule
    ITKImageGridModule
    ITKTransformModule
)

add_executable(ThinPlateSplineWarp ThinPlateSplineWarp.cxx)
target_link_libraries(
  ThinPlateSplineWarp
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageGridModule
    ITKTransformModule
)

add_executable(ModelToImageRegistration1 ModelToImageRegistration1.cxx)
target_link_libraries(
  ModelToImageRegistration1
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFilterBaseModule
    ITKImageFunctionModule
    ITKImageIntensityModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKSmoothingModule
    ITKSpatialObjectsModule
    ITKStatisticsModule
    ITKTransformModule
)

add_executable(ModelToImageRegistration2 ModelToImageRegistration2.cxx)
target_link_libraries(
  ModelToImageRegistration2
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFunctionModule
    ITKImageGridModule
    ITKLevelSetsModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKSpatialObjectsModule
    ITKTransformModule
)

add_executable(ImageRegistration1 ImageRegistration1.cxx)
target_link_libraries(
  ImageRegistration1
  PRIVATE
    ITKImageIO
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKMetricsv4Module
    ITKOptimizersv4Module
    ITKRegistrationMethodsv4Module
    ITKTransformModule
)

add_executable(ImageRegistration2 ImageRegistration2.cxx)
target_link_libraries(
  ImageRegistration2
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageCompareModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKSmoothingModule
    ITKTransformModule
)

add_executable(ImageRegistration17 ImageRegistration17.cxx)
target_link_libraries(
  ImageRegistration17
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKTransformModule
)

add_executable(
  ImageRegistrationHistogramPlotter
  ImageRegistrationHistogramPlotter.cxx
)
target_link_libraries(
  ImageRegistrationHistogramPlotter
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFilterBaseModule
    ITKImageGridModule
    ITKImageIntensityModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKSmoothingModule
    ITKStatisticsModule
    ITKTransformModule
)

add_executable(MeanSquaresImageMetric1 MeanSquaresImageMetric1.cxx)
target_link_libraries(
  MeanSquaresImageMetric1
  PRIVATE
    ITKImageIO
    ITKCommonModule
    ITKImageFunctionModule
    ITKMetricsv4Module
    ITKTransformModule
)

add_executable(IterativeClosestPoint1 IterativeClosestPoint1.cxx)
target_link_libraries(
  IterativeClosestPoint1
  PRIVATE
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKTransformModule
)

add_executable(IterativeClosestPoint2 IterativeClosestPoint2.cxx)
target_link_libraries(
  IterativeClosestPoint2
  PRIVATE
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKTransformModule
)

add_executable(IterativeClosestPoint3 IterativeClosestPoint3.cxx)
target_link_libraries(
  IterativeClosestPoint3
  PRIVATE
    ITKCommonModule
    ITKDistanceMapModule
    ITKOptimizersModule
    ITKRegistrationCommonModule
    ITKTransformModule
)

if(BUILD_TESTING)
  add_subdirectory(test)
endif()
