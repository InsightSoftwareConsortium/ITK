project(ITKv4ImageRegistration)

add_executable(ImageRegistration3 ImageRegistration3.cxx)
target_link_libraries(
  ImageRegistration3
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

add_executable(ImageRegistration5 ImageRegistration5.cxx)
target_link_libraries(
  ImageRegistration5
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

add_executable(ImageRegistration6 ImageRegistration6.cxx)
target_link_libraries(
  ImageRegistration6
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationCommonModule
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

add_executable(ImageRegistration8 ImageRegistration8.cxx)
target_link_libraries(
  ImageRegistration8
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationCommonModule
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

add_executable(ImageRegistration9 ImageRegistration9.cxx)
target_link_libraries(
  ImageRegistration9
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationCommonModule
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

add_executable(ImageRegistration10 ImageRegistration10.cxx)
target_link_libraries(
  ImageRegistration10
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageFunctionModule
    ITK::ITKImageGridModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKTransformModule
)

add_executable(DeformableRegistration4 DeformableRegistration4.cxx)
target_link_libraries(
  DeformableRegistration4
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKDisplacementFieldModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

add_executable(ImageRegistration4 ImageRegistration4.cxx)
target_link_libraries(
  ImageRegistration4
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

add_executable(ImageRegistration7 ImageRegistration7.cxx)
target_link_libraries(
  ImageRegistration7
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationCommonModule
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

add_executable(ImageRegistration11 ImageRegistration11.cxx)
target_link_libraries(
  ImageRegistration11
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKStatisticsModule
    ITK::ITKTransformModule
)

add_executable(ImageRegistration12 ImageRegistration12.cxx)
target_link_libraries(
  ImageRegistration12
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationCommonModule
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKSpatialObjectsModule
    ITK::ITKTransformModule
)

add_executable(ImageRegistration13 ImageRegistration13.cxx)
target_link_libraries(
  ImageRegistration13
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationCommonModule
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

add_executable(ImageRegistration14 ImageRegistration14.cxx)
target_link_libraries(
  ImageRegistration14
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKStatisticsModule
    ITK::ITKTransformModule
)

add_executable(ImageRegistration15 ImageRegistration15.cxx)
target_link_libraries(
  ImageRegistration15
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKStatisticsModule
    ITK::ITKTransformModule
)

add_executable(ImageRegistration16 ImageRegistration16.cxx)
target_link_libraries(
  ImageRegistration16
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKTransformModule
)

add_executable(ImageRegistration18 ImageRegistration18.cxx)
target_link_libraries(
  ImageRegistration18
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKTransformModule
)

add_executable(ImageRegistration19 ImageRegistration19.cxx)
target_link_libraries(
  ImageRegistration19
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageFunctionModule
    ITK::ITKImageGridModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKTransformModule
)

add_executable(ImageRegistration20 ImageRegistration20.cxx)
target_link_libraries(
  ImageRegistration20
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKTransformModule
)

add_executable(DeformableRegistration6 DeformableRegistration6.cxx)
target_link_libraries(
  DeformableRegistration6
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKDisplacementFieldModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationCommonModule
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

add_executable(DeformableRegistration7 DeformableRegistration7.cxx)
target_link_libraries(
  DeformableRegistration7
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKDisplacementFieldModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

add_executable(DeformableRegistration8 DeformableRegistration8.cxx)
target_link_libraries(
  DeformableRegistration8
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKTransformIO
    ITK::ITKCommonModule
    ITK::ITKDisplacementFieldModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

if(ITK_USE_FFTWD AND NOT ITK_USE_CUFFTW)
  add_executable(DeformableRegistration9 DeformableRegistration9.cxx)
  target_link_libraries(
    DeformableRegistration9
    PRIVATE
      ITK::ITKImageIO
      ITK::ITKDisplacementFieldModule
      ITK::ITKImageFilterBaseModule
      ITK::ITKImageFunctionModule
      ITK::ITKImageGridModule
      ITK::ITKImageIntensityModule
      ITK::ITKPDEDeformableRegistrationModule
  )

  add_executable(DeformableRegistration10 DeformableRegistration10.cxx)
  target_link_libraries(
    DeformableRegistration10
    PRIVATE
      ITK::ITKImageIO
      ITK::ITKDisplacementFieldModule
      ITK::ITKImageFilterBaseModule
      ITK::ITKImageFunctionModule
      ITK::ITKImageIntensityModule
      ITK::ITKPDEDeformableRegistrationModule
  )
endif()

add_executable(MultiResImageRegistration1 MultiResImageRegistration1.cxx)
target_link_libraries(
  MultiResImageRegistration1
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

add_executable(MultiResImageRegistration2 MultiResImageRegistration2.cxx)
target_link_libraries(
  MultiResImageRegistration2
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKTransformModule
)

add_executable(MultiResImageRegistration3 MultiResImageRegistration3.cxx)
target_link_libraries(
  MultiResImageRegistration3
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKTransformModule
)

add_executable(MultiStageImageRegistration1 MultiStageImageRegistration1.cxx)
target_link_libraries(
  MultiStageImageRegistration1
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

add_executable(MultiStageImageRegistration2 MultiStageImageRegistration2.cxx)
target_link_libraries(
  MultiStageImageRegistration2
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKImageStatisticsModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

if(ITKFEMRegistration_ENABLED)
  add_executable(DeformableRegistration1 DeformableRegistration1.cxx)
  target_link_libraries(
    DeformableRegistration1
    PRIVATE
      ITK::ITKImageIO
      ITK::ITKFEMRegistrationModule
      ITK::ITKImageIntensityModule
  )

  add_executable(DeformableRegistration11 DeformableRegistration11.cxx)
  target_link_libraries(
    DeformableRegistration11
    PRIVATE
      ITK::ITKImageIO
      ITK::ITKFEMRegistrationModule
      ITK::ITKImageIntensityModule
  )
endif()

add_executable(DeformableRegistration2 DeformableRegistration2.cxx)
target_link_libraries(
  DeformableRegistration2
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKDisplacementFieldModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKPDEDeformableRegistrationModule
)

add_executable(DeformableRegistration3 DeformableRegistration3.cxx)
target_link_libraries(
  DeformableRegistration3
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKDisplacementFieldModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKPDEDeformableRegistrationModule
)

add_executable(DeformableRegistration5 DeformableRegistration5.cxx)
target_link_libraries(
  DeformableRegistration5
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKDisplacementFieldModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKPDEDeformableRegistrationModule
)

add_executable(DeformableRegistration12 DeformableRegistration12.cxx)
target_link_libraries(
  DeformableRegistration12
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

add_executable(DeformableRegistration13 DeformableRegistration13.cxx)
target_link_libraries(
  DeformableRegistration13
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKTransformModule
)

add_executable(DeformableRegistration14 DeformableRegistration14.cxx)
target_link_libraries(
  DeformableRegistration14
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKTransformIO
    ITK::ITKCommonModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKTransformModule
)

add_executable(ChangeInformationImageFilter ChangeInformationImageFilter.cxx)
target_link_libraries(
  ChangeInformationImageFilter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageGridModule
)

add_executable(DeformableRegistration15 DeformableRegistration15.cxx)
target_link_libraries(
  DeformableRegistration15
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKTransformIO
    ITK::ITKCommonModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageFunctionModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKTransformModule
)

add_executable(DeformableRegistration16 DeformableRegistration16.cxx)
target_link_libraries(
  DeformableRegistration16
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKDisplacementFieldModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKPDEDeformableRegistrationModule
    ITK::ITKRegistrationCommonModule
)

add_executable(DeformableRegistration17 DeformableRegistration17.cxx)
target_link_libraries(
  DeformableRegistration17
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKDisplacementFieldModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKPDEDeformableRegistrationModule
    ITK::ITKRegistrationCommonModule
)

add_executable(
  DisplacementFieldInitialization
  DisplacementFieldInitialization.cxx
)
target_link_libraries(
  DisplacementFieldInitialization
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKDisplacementFieldModule
)

add_executable(LandmarkWarping2 LandmarkWarping2.cxx)
target_link_libraries(
  LandmarkWarping2
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKDisplacementFieldModule
    ITK::ITKImageGridModule
)

add_executable(BSplineWarping1 BSplineWarping1.cxx)
target_link_libraries(
  BSplineWarping1
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKTransformIO
    ITK::ITKCommonModule
    ITK::ITKImageGridModule
    ITK::ITKTransformModule
)

add_executable(BSplineWarping2 BSplineWarping2.cxx)
target_link_libraries(
  BSplineWarping2
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKTransformIO
    ITK::ITKCommonModule
    ITK::ITKImageGridModule
    ITK::ITKTransformModule
)

add_executable(ThinPlateSplineWarp ThinPlateSplineWarp.cxx)
target_link_libraries(
  ThinPlateSplineWarp
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageGridModule
    ITK::ITKTransformModule
)

add_executable(ModelToImageRegistration1 ModelToImageRegistration1.cxx)
target_link_libraries(
  ModelToImageRegistration1
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageFunctionModule
    ITK::ITKImageIntensityModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKSmoothingModule
    ITK::ITKSpatialObjectsModule
    ITK::ITKStatisticsModule
    ITK::ITKTransformModule
)

add_executable(ModelToImageRegistration2 ModelToImageRegistration2.cxx)
target_link_libraries(
  ModelToImageRegistration2
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFunctionModule
    ITK::ITKImageGridModule
    ITK::ITKLevelSetsModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKSpatialObjectsModule
    ITK::ITKTransformModule
)

add_executable(ImageRegistration1 ImageRegistration1.cxx)
target_link_libraries(
  ImageRegistration1
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKMetricsv4Module
    ITK::ITKOptimizersv4Module
    ITK::ITKRegistrationMethodsv4Module
    ITK::ITKTransformModule
)

add_executable(ImageRegistration2 ImageRegistration2.cxx)
target_link_libraries(
  ImageRegistration2
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageCompareModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKSmoothingModule
    ITK::ITKTransformModule
)

add_executable(ImageRegistration17 ImageRegistration17.cxx)
target_link_libraries(
  ImageRegistration17
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKTransformModule
)

add_executable(
  ImageRegistrationHistogramPlotter
  ImageRegistrationHistogramPlotter.cxx
)
target_link_libraries(
  ImageRegistrationHistogramPlotter
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFilterBaseModule
    ITK::ITKImageGridModule
    ITK::ITKImageIntensityModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKSmoothingModule
    ITK::ITKStatisticsModule
    ITK::ITKTransformModule
)

add_executable(MeanSquaresImageMetric1 MeanSquaresImageMetric1.cxx)
target_link_libraries(
  MeanSquaresImageMetric1
  PRIVATE
    ITK::ITKImageIO
    ITK::ITKCommonModule
    ITK::ITKImageFunctionModule
    ITK::ITKMetricsv4Module
    ITK::ITKTransformModule
)

add_executable(IterativeClosestPoint1 IterativeClosestPoint1.cxx)
target_link_libraries(
  IterativeClosestPoint1
  PRIVATE
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKTransformModule
)

add_executable(IterativeClosestPoint2 IterativeClosestPoint2.cxx)
target_link_libraries(
  IterativeClosestPoint2
  PRIVATE
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKTransformModule
)

add_executable(IterativeClosestPoint3 IterativeClosestPoint3.cxx)
target_link_libraries(
  IterativeClosestPoint3
  PRIVATE
    ITK::ITKCommonModule
    ITK::ITKDistanceMapModule
    ITK::ITKOptimizersModule
    ITK::ITKRegistrationCommonModule
    ITK::ITKTransformModule
)

if(BUILD_TESTING)
  add_subdirectory(test)
endif()
