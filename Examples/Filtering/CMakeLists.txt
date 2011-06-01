project(FilteringExamples)
include_regular_expression("^.*$")
add_executable(CompositeFilterExample CompositeFilterExample.cxx )
target_link_libraries(CompositeFilterExample  ${ITK_LIBRARIES})
add_executable(FlipImageFilter FlipImageFilter.cxx )
target_link_libraries(FlipImageFilter  ${ITK_LIBRARIES})
add_executable(FFTImageFilter FFTImageFilter.cxx )
target_link_libraries(FFTImageFilter  ${ITK_LIBRARIES})
if( USE_FFTWF )
  add_executable(FFTDirectInverse2 FFTDirectInverse2.cxx )
  target_link_libraries(FFTDirectInverse2  ${ITK_LIBRARIES})
endif( USE_FFTWF )
add_executable(GradientMagnitudeImageFilter GradientMagnitudeImageFilter.cxx )
target_link_libraries(GradientMagnitudeImageFilter  ${ITK_LIBRARIES})
add_executable(GradientMagnitudeRecursiveGaussianImageFilter GradientMagnitudeRecursiveGaussianImageFilter.cxx )
target_link_libraries(GradientMagnitudeRecursiveGaussianImageFilter  ${ITK_LIBRARIES})
add_executable(GradientRecursiveGaussianImageFilter GradientRecursiveGaussianImageFilter.cxx )
target_link_libraries(GradientRecursiveGaussianImageFilter  ${ITK_LIBRARIES})
add_executable(GrayscaleFunctionDilateImageFilter GrayscaleFunctionDilateImageFilter.cxx )
target_link_libraries(GrayscaleFunctionDilateImageFilter  ${ITK_LIBRARIES})
add_executable(BinaryThresholdImageFilter BinaryThresholdImageFilter.cxx )
target_link_libraries(BinaryThresholdImageFilter  ${ITK_LIBRARIES})
add_executable(BinaryMedianImageFilter BinaryMedianImageFilter.cxx )
target_link_libraries(BinaryMedianImageFilter  ${ITK_LIBRARIES})
add_executable(OtsuThresholdImageFilter OtsuThresholdImageFilter.cxx )
target_link_libraries(OtsuThresholdImageFilter  ${ITK_LIBRARIES})
add_executable(OtsuMultipleThresholdImageFilter OtsuMultipleThresholdImageFilter.cxx )
target_link_libraries(OtsuMultipleThresholdImageFilter ${ITK_LIBRARIES})
add_executable(ThresholdImageFilter ThresholdImageFilter.cxx )
target_link_libraries(ThresholdImageFilter  ${ITK_LIBRARIES})
add_executable(CastingImageFilters CastingImageFilters.cxx )
target_link_libraries(CastingImageFilters  ${ITK_LIBRARIES})
add_executable(CannyEdgeDetectionImageFilter CannyEdgeDetectionImageFilter.cxx )
target_link_libraries(CannyEdgeDetectionImageFilter  ${ITK_LIBRARIES})
add_executable(MedianImageFilter MedianImageFilter.cxx )
target_link_libraries(MedianImageFilter  ${ITK_LIBRARIES})
add_executable(MeanImageFilter MeanImageFilter.cxx )
target_link_libraries(MeanImageFilter  ${ITK_LIBRARIES})
add_executable(MathematicalMorphologyBinaryFilters MathematicalMorphologyBinaryFilters.cxx )
target_link_libraries(MathematicalMorphologyBinaryFilters  ${ITK_LIBRARIES})
add_executable(MathematicalMorphologyGrayscaleFilters MathematicalMorphologyGrayscaleFilters.cxx )
target_link_libraries(MathematicalMorphologyGrayscaleFilters  ${ITK_LIBRARIES})
add_executable(BinaryMinMaxCurvatureFlowImageFilter BinaryMinMaxCurvatureFlowImageFilter.cxx )
target_link_libraries(BinaryMinMaxCurvatureFlowImageFilter  ${ITK_LIBRARIES})
add_executable(DiscreteGaussianImageFilter DiscreteGaussianImageFilter.cxx )
target_link_libraries(DiscreteGaussianImageFilter  ${ITK_LIBRARIES})
add_executable(GaussianBlurImageFunction GaussianBlurImageFunction.cxx )
target_link_libraries(GaussianBlurImageFunction  ${ITK_LIBRARIES})
add_executable(LaplacianRecursiveGaussianImageFilter1 LaplacianRecursiveGaussianImageFilter1.cxx )
target_link_libraries(LaplacianRecursiveGaussianImageFilter1  ${ITK_LIBRARIES})
add_executable(ScaleSpaceGenerator2D ScaleSpaceGenerator2D.cxx )
target_link_libraries(ScaleSpaceGenerator2D  ${ITK_LIBRARIES})
add_executable(BinomialBlurImageFilter BinomialBlurImageFilter.cxx )
target_link_libraries(BinomialBlurImageFilter  ${ITK_LIBRARIES})
add_executable(CurvatureAnisotropicDiffusionImageFilter CurvatureAnisotropicDiffusionImageFilter.cxx )
target_link_libraries(CurvatureAnisotropicDiffusionImageFilter  ${ITK_LIBRARIES})
add_executable(VectorGradientAnisotropicDiffusionImageFilter VectorGradientAnisotropicDiffusionImageFilter.cxx )
target_link_libraries(VectorGradientAnisotropicDiffusionImageFilter  ${ITK_LIBRARIES})
add_executable(VectorCurvatureAnisotropicDiffusionImageFilter VectorCurvatureAnisotropicDiffusionImageFilter.cxx )
target_link_libraries(VectorCurvatureAnisotropicDiffusionImageFilter  ${ITK_LIBRARIES})
add_executable(RGBToGrayscale RGBToGrayscale.cxx )
target_link_libraries(RGBToGrayscale  ${ITK_LIBRARIES})
add_executable(BilateralImageFilter BilateralImageFilter.cxx )
target_link_libraries(BilateralImageFilter  ${ITK_LIBRARIES})
add_executable(ResampleOrientedImageFilter ResampleOrientedImageFilter.cxx )
target_link_libraries(ResampleOrientedImageFilter  ${ITK_LIBRARIES})
add_executable(ResampleImageFilter2 ResampleImageFilter2.cxx )
target_link_libraries(ResampleImageFilter2  ${ITK_LIBRARIES})
add_executable(GradientAnisotropicDiffusionImageFilter GradientAnisotropicDiffusionImageFilter.cxx )
target_link_libraries(GradientAnisotropicDiffusionImageFilter  ${ITK_LIBRARIES})
add_executable(CurvatureFlowImageFilter CurvatureFlowImageFilter.cxx )
target_link_libraries(CurvatureFlowImageFilter  ${ITK_LIBRARIES})
add_executable(GradientVectorFlowImageFilter GradientVectorFlowImageFilter.cxx )
target_link_libraries(GradientVectorFlowImageFilter  ${ITK_LIBRARIES})
add_executable(SigmoidImageFilter SigmoidImageFilter.cxx )
target_link_libraries(SigmoidImageFilter  ${ITK_LIBRARIES})
add_executable(RGBCurvatureAnisotropicDiffusionImageFilter RGBCurvatureAnisotropicDiffusionImageFilter.cxx )
target_link_libraries(RGBCurvatureAnisotropicDiffusionImageFilter  ${ITK_LIBRARIES})
add_executable(RGBGradientAnisotropicDiffusionImageFilter RGBGradientAnisotropicDiffusionImageFilter.cxx )
target_link_libraries(RGBGradientAnisotropicDiffusionImageFilter  ${ITK_LIBRARIES})
add_executable(SignedDanielssonDistanceMapImageFilter SignedDanielssonDistanceMapImageFilter.cxx )
target_link_libraries(SignedDanielssonDistanceMapImageFilter  ${ITK_LIBRARIES})
add_executable(SecondDerivativeRecursiveGaussianImageFilter SecondDerivativeRecursiveGaussianImageFilter.cxx )
target_link_libraries(SecondDerivativeRecursiveGaussianImageFilter  ${ITK_LIBRARIES})
add_executable(DerivativeImageFilter DerivativeImageFilter.cxx )
target_link_libraries(DerivativeImageFilter  ${ITK_LIBRARIES})
add_executable(DanielssonDistanceMapImageFilter DanielssonDistanceMapImageFilter.cxx )
target_link_libraries(DanielssonDistanceMapImageFilter  ${ITK_LIBRARIES})
add_executable(AntiAliasBinaryImageFilter AntiAliasBinaryImageFilter.cxx )
target_link_libraries(AntiAliasBinaryImageFilter  ${ITK_LIBRARIES})
add_executable(ResampleImageFilter ResampleImageFilter.cxx )
target_link_libraries(ResampleImageFilter  ${ITK_LIBRARIES})
add_executable(SmoothingRecursiveGaussianImageFilter SmoothingRecursiveGaussianImageFilter.cxx )
target_link_libraries(SmoothingRecursiveGaussianImageFilter  ${ITK_LIBRARIES})
add_executable(SmoothingRecursiveGaussianImageFilter2 SmoothingRecursiveGaussianImageFilter2.cxx )
target_link_libraries(SmoothingRecursiveGaussianImageFilter2  ${ITK_LIBRARIES})
add_executable(LaplacianRecursiveGaussianImageFilter2 LaplacianRecursiveGaussianImageFilter2.cxx )
target_link_libraries(LaplacianRecursiveGaussianImageFilter2  ${ITK_LIBRARIES})
add_executable(MorphologicalImageEnhancement MorphologicalImageEnhancement.cxx )
target_link_libraries(MorphologicalImageEnhancement  ${ITK_LIBRARIES})
add_executable(MinMaxCurvatureFlowImageFilter MinMaxCurvatureFlowImageFilter.cxx )
target_link_libraries(MinMaxCurvatureFlowImageFilter  ${ITK_LIBRARIES})
add_executable(FFTImageFilterFourierDomainFiltering FFTImageFilterFourierDomainFiltering.cxx )
target_link_libraries(FFTImageFilterFourierDomainFiltering  ${ITK_LIBRARIES})
add_executable(LaplacianImageFilter LaplacianImageFilter.cxx )
target_link_libraries(LaplacianImageFilter  ${ITK_LIBRARIES})
add_executable(ResampleImageFilter3 ResampleImageFilter3.cxx )
target_link_libraries(ResampleImageFilter3  ${ITK_LIBRARIES})
add_executable(ResampleImageFilter4 ResampleImageFilter4.cxx )
target_link_libraries(ResampleImageFilter4  ${ITK_LIBRARIES})
add_executable(VotingBinaryHoleFillingImageFilter VotingBinaryHoleFillingImageFilter.cxx )
target_link_libraries(VotingBinaryHoleFillingImageFilter  ${ITK_LIBRARIES})
add_executable(VotingBinaryIterativeHoleFillingImageFilter VotingBinaryIterativeHoleFillingImageFilter.cxx )
target_link_libraries(VotingBinaryIterativeHoleFillingImageFilter  ${ITK_LIBRARIES})
add_executable(ResampleImageFilter5 ResampleImageFilter5.cxx )
target_link_libraries(ResampleImageFilter5  ${ITK_LIBRARIES})
add_executable(ResampleImageFilter6 ResampleImageFilter6.cxx )
target_link_libraries(ResampleImageFilter6  ${ITK_LIBRARIES})
add_executable(ResampleImageFilter8 ResampleImageFilter8.cxx )
target_link_libraries(ResampleImageFilter8  ${ITK_LIBRARIES})
add_executable(ResampleImageFilter9 ResampleImageFilter9.cxx )
target_link_libraries(ResampleImageFilter9  ${ITK_LIBRARIES})
add_executable(ResampleVolumesToBeIsotropic ResampleVolumesToBeIsotropic.cxx )
target_link_libraries(ResampleVolumesToBeIsotropic  ${ITK_LIBRARIES})
add_executable(SubsampleVolume SubsampleVolume.cxx )
target_link_libraries(SubsampleVolume  ${ITK_LIBRARIES})
add_executable(SpatialObjectToImage1 SpatialObjectToImage1.cxx )
target_link_libraries(SpatialObjectToImage1  ${ITK_LIBRARIES})
add_executable(SpatialObjectToImage2 SpatialObjectToImage2.cxx )
target_link_libraries(SpatialObjectToImage2  ${ITK_LIBRARIES})
add_executable(SpatialObjectToImage3 SpatialObjectToImage3.cxx )
target_link_libraries(SpatialObjectToImage3  ${ITK_LIBRARIES})
add_executable(LaplacianSharpeningImageFilter LaplacianSharpeningImageFilter.cxx )
target_link_libraries(LaplacianSharpeningImageFilter  ${ITK_LIBRARIES})
add_executable(VectorIndexSelection VectorIndexSelection.cxx )
target_link_libraries(VectorIndexSelection  ${ITK_LIBRARIES})
add_executable(VesselnessMeasureImageFilter VesselnessMeasureImageFilter.cxx )
target_link_libraries(VesselnessMeasureImageFilter  ${ITK_LIBRARIES})

set(BASELINE ${ITK_DATA_ROOT}/Baseline/Filtering)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)
if( NOT ITK_DISABLE_CXX_TESTING )
if(BUILD_TESTING)
itk_add_test(NAME BilateralImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/BilateralImageFilter.png
            ${TEMP}/BilateralImageFilter.png
  $<TARGET_FILE:BilateralImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BilateralImageFilter.png
        6.0 5.0
)
itk_add_test(NAME  BinaryMinMaxCurvatureFlowImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/BinaryMinMaxCurvatureFlowImageFilter.png
            ${TEMP}/BinaryMinMaxCurvatureFlowImageFilter.png
  $<TARGET_FILE:BinaryMinMaxCurvatureFlowImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BinaryMinMaxCurvatureFlowImageFilter.png
        10 .125 1 128
)
itk_add_test(NAME  BinaryThresholdImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/BinaryThresholdImageFilter.png
            ${TEMP}/BinaryThresholdImageFilter.png
  $<TARGET_FILE:BinaryThresholdImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BinaryThresholdImageFilter.png
        150 180 0 255
)
itk_add_test(NAME  BinaryMedianImageFilterTest1 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/BinaryMedianImageFilter1.png
            ${TEMP}/BinaryMedianImageFilter1.png
  $<TARGET_FILE:BinaryMedianImageFilter>
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/BinaryMedianImageFilter1.png
        1 1
)
itk_add_test(NAME  BinaryMedianImageFilterTest2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/BinaryMedianImageFilter2.png
            ${TEMP}/BinaryMedianImageFilter2.png
  $<TARGET_FILE:BinaryMedianImageFilter>
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/BinaryMedianImageFilter2.png
        2 2
)
itk_add_test(NAME  BinaryMedianImageFilterTest3 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/BinaryMedianImageFilter3.png
            ${TEMP}/BinaryMedianImageFilter3.png
  $<TARGET_FILE:BinaryMedianImageFilter>
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/BinaryMedianImageFilter3.png
        3 3
)
itk_add_test(NAME  VotingBinaryHoleFillingImageFilterTest1 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/VotingBinaryHoleFillingImageFilter1.png
            ${TEMP}/VotingBinaryHoleFillingImageFilter1.png
  $<TARGET_FILE:VotingBinaryHoleFillingImageFilter>
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryHoleFillingImageFilter1.png
        1 1
)
itk_add_test(NAME  VotingBinaryHoleFillingImageFilterTest2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/VotingBinaryHoleFillingImageFilter2.png
            ${TEMP}/VotingBinaryHoleFillingImageFilter2.png
  $<TARGET_FILE:VotingBinaryHoleFillingImageFilter>
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryHoleFillingImageFilter2.png
        2 2
)
itk_add_test(NAME  VotingBinaryHoleFillingImageFilterTest3 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/VotingBinaryHoleFillingImageFilter3.png
            ${TEMP}/VotingBinaryHoleFillingImageFilter3.png
  $<TARGET_FILE:VotingBinaryHoleFillingImageFilter>
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryHoleFillingImageFilter3.png
        3 3
)
itk_add_test(NAME  VotingBinaryIterativeHoleFillingImageFilterTest1 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/VotingBinaryIterativeHoleFillingImageFilter1.png
            ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter1.png
  $<TARGET_FILE:VotingBinaryIterativeHoleFillingImageFilter>
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter1.png
        1 1 10
)
itk_add_test(NAME  VotingBinaryIterativeHoleFillingImageFilterTest2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/VotingBinaryIterativeHoleFillingImageFilter2.png
            ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter2.png
  $<TARGET_FILE:VotingBinaryIterativeHoleFillingImageFilter>
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter2.png
        2 2 10
)
itk_add_test(NAME  VotingBinaryIterativeHoleFillingImageFilterTest3 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/VotingBinaryIterativeHoleFillingImageFilter3.png
            ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter3.png
  $<TARGET_FILE:VotingBinaryIterativeHoleFillingImageFilter>
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter3.png
        3 3 10
)
itk_add_test(NAME  BinomialBlurImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/BinomialBlurImageFilter.png
            ${TEMP}/BinomialBlurImageFilter.png
  $<TARGET_FILE:BinomialBlurImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BinomialBlurImageFilter.png
        5
)
itk_add_test(NAME  CurvatureAnisotropicDiffusionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/CurvatureAnisotropicDiffusionImageFilter.png
            ${TEMP}/CurvatureAnisotropicDiffusionImageFilter.png
  $<TARGET_FILE:CurvatureAnisotropicDiffusionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/CurvatureAnisotropicDiffusionImageFilter.png
        5 0.125 3
)
itk_add_test(NAME  CurvatureFlowImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/CurvatureFlowImageFilter.png
            ${TEMP}/CurvatureFlowImageFilter.png
  $<TARGET_FILE:CurvatureFlowImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/CurvatureFlowImageFilter.png
        5 0.125 1.0
)
itk_add_test(NAME  DanielssonDistanceMapImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DanielssonDistanceMapImageFilter.png
            ${TEMP}/DanielssonDistanceMapImageFilter.png
  $<TARGET_FILE:DanielssonDistanceMapImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/FivePoints.png
        ${TEMP}/DanielssonDistanceMapImageFilter.png
        ${TEMP}/DanielssonDistanceMapImageFilter2.png
        ${TEMP}/DanielssonDistanceMapImageFilter.mha
)
itk_add_test(NAME  DiscreteGaussianImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DiscreteGaussianImageFilter.png
            ${TEMP}/DiscreteGaussianImageFilter.png
  $<TARGET_FILE:DiscreteGaussianImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/DiscreteGaussianImageFilter.png
        1.73 10
)
itk_add_test(NAME  GradientAnisotropicDiffusionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/GradientAnisotropicDiffusionImageFilter.png
            ${TEMP}/GradientAnisotropicDiffusionImageFilter.png
  $<TARGET_FILE:GradientAnisotropicDiffusionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GradientAnisotropicDiffusionImageFilter.png
        5 0.125 1.0
)
itk_add_test(NAME  GradientMagnitudeImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/GradientMagnitudeImageFilter.png
            ${TEMP}/GradientMagnitudeImageFilter.png
  $<TARGET_FILE:GradientMagnitudeImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GradientMagnitudeImageFilter.png
)
itk_add_test(NAME  GradientMagnitudeRecursiveGaussianImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/GradientMagnitudeRecursiveGaussianImageFilter.png
            ${TEMP}/GradientMagnitudeRecursiveGaussianImageFilter.png
  $<TARGET_FILE:GradientMagnitudeRecursiveGaussianImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GradientMagnitudeRecursiveGaussianImageFilter.png
        5
)
itk_add_test(NAME  GradientRecursiveGaussianImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:GradientRecursiveGaussianImageFilter>
        ${ITK_SOURCE_DIR}/Testing/Data/Input/HeadMRVolume.mhd
        ${TEMP}/GradientRecursiveGaussianImageFilterTest.mha
        1.5  # Sigma
)
itk_add_test(NAME  GradientVectorFlowImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:GradientVectorFlowImageFilter>
        ${ITK_SOURCE_DIR}/Testing/Data/Input/GradientRecursiveGaussianImageFilterTest.mha
        ${TEMP}/GradientVectorFlowImageFilterTest.mha
        5        # number of iterations
        2000.0   # noise level
)
set_tests_properties( GradientVectorFlowImageFilterTest
  PROPERTIES DEPENDS GradientRecursiveGaussianImageFilterTest)
itk_add_test(NAME  DerivativeImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DerivativeImageFilter.mha
            ${TEMP}/DerivativeImageFilter.mha
  $<TARGET_FILE:DerivativeImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/DerivativeImageFilter.mha
        ${TEMP}/DerivativeImageFilter.png
        1 1
)
itk_add_test(NAME  LaplacianImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/LaplacianImageFilter.png
            ${TEMP}/LaplacianImageFilter.png
  $<TARGET_FILE:LaplacianImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/LaplacianImageFilter.png
)
itk_add_test(NAME  LaplacianSharpeningImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/LaplacianSharpeningImageFilter.png
            ${TEMP}/LaplacianSharpeningImageFilter.png
  $<TARGET_FILE:LaplacianSharpeningImageFilter>
        ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
        ${TEMP}/LaplacianSharpeningImageFilter.png
)
itk_add_test(NAME  MathematicalMorphologyBinaryFiltersTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/MathematicalMorphologyBinaryFiltersErode.png
            ${TEMP}/MathematicalMorphologyBinaryFiltersErode.png
  $<TARGET_FILE:MathematicalMorphologyBinaryFilters>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MathematicalMorphologyBinaryFiltersErode.png
        ${TEMP}/MathematicalMorphologyBinaryFiltersDilate.png
        150 180
)
itk_add_test(NAME  MathematicalMorphologyGrayscaleFiltersTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/MathematicalMorphologyGrayscaleFiltersErode.png
            ${TEMP}/MathematicalMorphologyGrayscaleFiltersErode.png
  $<TARGET_FILE:MathematicalMorphologyGrayscaleFilters>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MathematicalMorphologyGrayscaleFiltersErode.png
        ${TEMP}/MathematicalMorphologyGrayscaleFiltersDilate.png
)
itk_add_test(NAME  MeanImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/MeanImageFilter.png
            ${TEMP}/MeanImageFilter.png
  $<TARGET_FILE:MeanImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MeanImageFilter.png
)
itk_add_test(NAME  MedianImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/MedianImageFilter.png
            ${TEMP}/MedianImageFilter.png
  $<TARGET_FILE:MedianImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MedianImageFilter.png
        1
)
itk_add_test(NAME  ResampleImageFilterTest1 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilterTest1.png
            ${TEMP}/ResampleImageFilterTest1.png
  $<TARGET_FILE:ResampleImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest1.png
        0
)
itk_add_test(NAME  ResampleOrientedImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleOrientedImageFilterTest.png
            ${TEMP}/ResampleOrientedImageFilterTest.png
  $<TARGET_FILE:ResampleOrientedImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanHeadSlice.png
        ${TEMP}/ResampleOrientedImageFilterTest.png
        0 1 1 0
)
itk_add_test(NAME  ResampleImageFilterTest2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilterTest2.png
            ${TEMP}/ResampleImageFilterTest2.png
  $<TARGET_FILE:ResampleImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest2.png
        1
)
itk_add_test(NAME  ResampleImageFilterTest3 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilterTest3.png
            ${TEMP}/ResampleImageFilterTest3.png
  $<TARGET_FILE:ResampleImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest3.png
        2
)
itk_add_test(NAME  ResampleImageFilterTest4 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilterTest4.png
            ${TEMP}/ResampleImageFilterTest4.png
  $<TARGET_FILE:ResampleImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest4.png
        3
)
itk_add_test(NAME  ResampleImageFilter2Test1 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilter2Test1.png
            ${TEMP}/ResampleImageFilter2Test1.png
  $<TARGET_FILE:ResampleImageFilter2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test1.png
        0
)
itk_add_test(NAME  ResampleImageFilter2Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilter2Test2.png
            ${TEMP}/ResampleImageFilter2Test2.png
  $<TARGET_FILE:ResampleImageFilter2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test2.png
        1
)
itk_add_test(NAME  ResampleImageFilter2Test3 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilter2Test3.png
            ${TEMP}/ResampleImageFilter2Test3.png
  $<TARGET_FILE:ResampleImageFilter2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test3.png
        2
)
itk_add_test(NAME  ResampleImageFilter2Test4 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilter2Test4.png
            ${TEMP}/ResampleImageFilter2Test4.png
  $<TARGET_FILE:ResampleImageFilter2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test4.png
        3
)
itk_add_test(NAME  ResampleImageFilter2Test5 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilter2Test5.png
            ${TEMP}/ResampleImageFilter2Test5.png
  $<TARGET_FILE:ResampleImageFilter2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test5.png
        3
)
itk_add_test(NAME  ResampleImageFilter3Test1 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilter3Test1PixelCentered.png
            ${TEMP}/ResampleImageFilter3Test1PixelCentered.png
  $<TARGET_FILE:ResampleImageFilter3>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter3Test1PixelCentered.png
        0
)

itk_add_test(NAME  ResampleImageFilter4Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilter4TestPixelCentered.png
            ${TEMP}/ResampleImageFilter4TestPixelCentered.png
  $<TARGET_FILE:ResampleImageFilter4>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter4TestPixelCentered.png
        20
)
itk_add_test(NAME  ResampleImageFilter5Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilter5TestPixelCentered.png
            ${TEMP}/ResampleImageFilter5TestPixelCentered.png
  $<TARGET_FILE:ResampleImageFilter5>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter5TestPixelCentered.png
        10 1.2
)
itk_add_test(NAME  ResampleImageFilter8Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilter8TestPixelCentered.png
            ${TEMP}/ResampleImageFilter8TestPixelCentered.png
  $<TARGET_FILE:ResampleImageFilter8>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter8TestPixelCentered.png
        45
)
itk_add_test(NAME  ResampleImageFilter9Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilter9TestPixelCentered.png
            ${TEMP}/ResampleImageFilter9TestNearestPixelCentered.png
  $<TARGET_FILE:ResampleImageFilter9>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/ResampleImageFilter9TestNearestPixelCentered.png
        ${TEMP}/ResampleImageFilter9TestLinearPixelCentered.png
)
itk_add_test(NAME  SigmoidImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/SigmoidImageFilter.png
            ${TEMP}/SigmoidImageFilter.png
  $<TARGET_FILE:SigmoidImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/SigmoidImageFilter.png
        10 240 10 170
)
itk_add_test(NAME  SmoothingRecursiveGaussianImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/SmoothingRecursiveGaussianImageFilter.png
            ${TEMP}/SmoothingRecursiveGaussianImageFilter.png
  $<TARGET_FILE:SmoothingRecursiveGaussianImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/SmoothingRecursiveGaussianImageFilter.png
        3
)
itk_add_test(NAME  SmoothingRecursiveGaussianImageFilter2Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/SmoothingRecursiveGaussianImageFilter2.png
            ${TEMP}/SmoothingRecursiveGaussianImageFilter2.png
  $<TARGET_FILE:SmoothingRecursiveGaussianImageFilter2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/SmoothingRecursiveGaussianImageFilter2.png
        3
)
itk_add_test(NAME  ThresholdAboveBelowOutsideImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ThresholdAboveImageFilter.png
            ${TEMP}/ThresholdAboveImageFilter.png
  --compare ${BASELINE}/ThresholdBelowImageFilter.png
            ${TEMP}/ThresholdBelowImageFilter.png
  --compare ${BASELINE}/ThresholdOutsideImageFilter.png
            ${TEMP}/ThresholdOutsideImageFilter.png
  $<TARGET_FILE:ThresholdImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdBelowImageFilter.png
        ${TEMP}/ThresholdAboveImageFilter.png
        ${TEMP}/ThresholdOutsideImageFilter.png
)
itk_add_test(NAME  VectorGradientAnisotropicDiffusionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/VectorGradientAnisotropicDiffusionImageFilter.png
            ${TEMP}/VectorGradientAnisotropicDiffusionImageFilter.png
  $<TARGET_FILE:VectorGradientAnisotropicDiffusionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/VectorGradientAnisotropicDiffusionImageFilterOriginal.png
        ${TEMP}/VectorGradientAnisotropicDiffusionImageFilter.png
        .25 5
)
itk_add_test(NAME  VectorCurvatureAnisotropicDiffusionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/VectorCurvatureAnisotropicDiffusionImageFilter.png
            ${TEMP}/VectorCurvatureAnisotropicDiffusionImageFilter.png
  $<TARGET_FILE:VectorCurvatureAnisotropicDiffusionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/VectorCurvatureAnisotropicDiffusionImageFilterOriginal.png
        ${TEMP}/VectorCurvatureAnisotropicDiffusionImageFilter.png
        0.125 5
)
itk_add_test(NAME  VectorIndexSelectionTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/VectorIndexSelection.png
            ${TEMP}/VectorIndexSelection.png
  $<TARGET_FILE:VectorIndexSelection>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/VectorIndexSelection.png
        1
)
itk_add_test(NAME  ZeroCrossingBasedEdgeDetectionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ZeroCrossingBasedEdgeDetectionImageFilter1.png
            ${TEMP}/ZeroCrossingBasedEdgeDetectionImageFilter1.png
  $<TARGET_FILE:ZeroCrossingBasedEdgeDetectionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ZeroCrossingBasedEdgeDetectionImageFilter1.png
        5.0
        0.03
)
itk_add_test(NAME  RGBGradientAnisotropicDiffusionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/RGBGradientAnisotropicDiffusionImageFilter.png
            ${TEMP}/RGBGradientAnisotropicDiffusionImageFilter.png
  $<TARGET_FILE:RGBGradientAnisotropicDiffusionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/RGBGradientAnisotropicDiffusionImageFilter.png
        0.125 20
)
itk_add_test(NAME  RGBCurvatureAnisotropicDiffusionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/RGBCurvatureAnisotropicDiffusionImageFilter.png
            ${TEMP}/RGBCurvatureAnisotropicDiffusionImageFilter.png
  $<TARGET_FILE:RGBCurvatureAnisotropicDiffusionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/RGBCurvatureAnisotropicDiffusionImageFilter.png
        0.125 20
)
itk_add_test(NAME  MorphologicalImageEnhancementTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/MorphologicalImageEnhancement.png
            ${TEMP}/MorphologicalImageEnhancement.png
  $<TARGET_FILE:MorphologicalImageEnhancement>
        ${ITK_SOURCE_DIR}/Testing/Data/Input/peppers.png
        ${TEMP}/MorphologicalImageEnhancement.png
        3
)
itk_add_test(NAME  OtsuThresholdImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/OtsuThresholdImageFilter.png
            ${TEMP}/OtsuThresholdImageFilter.png
  $<TARGET_FILE:OtsuThresholdImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/OtsuThresholdImageFilter.png
        255 0
)
itk_add_test(NAME  OtsuMultipleThresholdImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/OtsuMultipleThresholdOutput002.png
            ${TEMP}/OtsuMultipleThresholdOutput002.png
  $<TARGET_FILE:OtsuMultipleThresholdImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/OtsuMultipleThresholdOutput
        png 3
)
itk_add_test(NAME  DigitallyReconstructedRadiograph1Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/DigitallyReconstructedRadiograph1.png
            ${TEMP}/DigitallyReconstructedRadiograph1.png
  $<TARGET_FILE:DigitallyReconstructedRadiograph1>
        -o ${TEMP}/DigitallyReconstructedRadiograph1.png
)
itk_add_test(NAME  AntiAliasBinaryImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/AntiAliasBinaryImageFilter.png
            ${TEMP}/AntiAliasBinaryImageFilter.png
  $<TARGET_FILE:AntiAliasBinaryImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VentricleModel.png
        ${TEMP}/AntiAliasBinaryImageFilterDoublePixelType.mha
        ${TEMP}/AntiAliasBinaryImageFilter.png
        .001 50
)
itk_add_test(NAME  CannyEdgeDetectionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/CannyEdgeDetectionImageFilter.png
            ${TEMP}/CannyEdgeDetectionImageFilter.png
  $<TARGET_FILE:CannyEdgeDetectionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/CannyEdgeDetectionImageFilter.png
        9     # Sigma
        0     # Upper Threshold
        0     # Lower Threshold
)
itk_add_test(NAME  CannyEdgeDetectionImageFilterTest2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/CannyEdgeDetectionImageFilter2.png
            ${TEMP}/CannyEdgeDetectionImageFilter2.png
  $<TARGET_FILE:CannyEdgeDetectionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/CannyEdgeDetectionImageFilter2.png
        9     # Sigma
        5     # Upper Threshold
        1     # Lower Threshold
)
itk_add_test(NAME  FlipImageFilterTest1 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/FlipImageFilterX.png
            ${TEMP}/FlipImageFilterX.png
  $<TARGET_FILE:FlipImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/FlipImageFilterX.png
        1 0
)
itk_add_test(NAME  FlipImageFilterTest2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/FlipImageFilterY.png
            ${TEMP}/FlipImageFilterY.png
  $<TARGET_FILE:FlipImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/FlipImageFilterY.png
        0 1
)
itk_add_test(NAME  FlipImageFilterTest3 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/FlipImageFilterXY.png
            ${TEMP}/FlipImageFilterXY.png
  $<TARGET_FILE:FlipImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/FlipImageFilterXY.png
        1 1
)
itk_add_test(NAME  GaussianBlurImageFunctionTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/GaussianBlurImageFunction.mha
            ${TEMP}/GaussianBlurImageFunction.mha
  $<TARGET_FILE:GaussianBlurImageFunction>
        ${ITK_SOURCE_DIR}/Examples/Data/VentricleModel.png
        ${TEMP}/GaussianBlurImageFunction.mha
        2 3
)
itk_add_test(NAME  FFTImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/FFTImageFilterReal.png
            ${TEMP}/FFTImageFilterReal.png
  $<TARGET_FILE:FFTImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/FatMRISlice.png
        ${TEMP}/FFTImageFilterReal.png
        ${TEMP}/FFTImageFilterImaginary.png
        ${TEMP}/FFTImageFilterComplex.mhd
)
itk_add_test(NAME  FFTDirectInverseTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${ITK_SOURCE_DIR}/Examples/Data/Point.png
            ${TEMP}/FFTDirectInversePoint.png
  $<TARGET_FILE:FFTDirectInverse>
        ${ITK_SOURCE_DIR}/Examples/Data/Point.png
        ${TEMP}/FFTDirectInversePoint.png
)
if( USE_FFTWF )
#${BASELINE}/FFTDirectInverse2Point.png
  itk_add_test(NAME  FFTDirectInverse2Test COMMAND ${ITK_TEST_DRIVER}
    --compare ${ITK_SOURCE_DIR}/Examples/Data/Point.png
              ${TEMP}/FFTDirectInverse2Point.png
    $<TARGET_FILE:FFTDirectInverse2>
          ${ITK_SOURCE_DIR}/Examples/Data/Point.png
          ${TEMP}/FFTDirectInverse2Point.png
  )
endif( USE_FFTWF )
itk_add_test(NAME  FFTImageFilterTest2 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:FFTImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/FFTcenteredDC.png
        ${TEMP}/FFTImageFilterCenteredDCReal.png
        ${TEMP}/FFTImageFilterCenteredDCImaginary.png
        ${TEMP}/FFTImageFilterComplex2.mhd
)
itk_add_test(NAME  FFTImageFilterTest3 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:FFTImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/FFTcorneredDC.png
        ${TEMP}/FFTImageFilterCorneredDCReal.png
        ${TEMP}/FFTImageFilterCorneredDCImaginary.png
        ${TEMP}/FFTImageFilterComplex3.mhd
)
itk_add_test(NAME  RGBToGrayscaleTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/RGBToGrayscale.png
            ${TEMP}/RGBToGrayscale.png
  $<TARGET_FILE:RGBToGrayscale>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanHeadSlice.png
        ${TEMP}/RGBToGrayscale.png
)
itk_add_test(NAME  LaplacianRecursiveGaussianImageFilter1Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/LaplacianRecursiveGaussianImageFilter1.mha
            ${TEMP}/LaplacianRecursiveGaussianImageFilter1.mha
  $<TARGET_FILE:LaplacianRecursiveGaussianImageFilter1>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/LaplacianRecursiveGaussianImageFilter1.mha
        3
)
itk_add_test(NAME  LaplacianRecursiveGaussianImageFilter2Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/LaplacianRecursiveGaussianImageFilter2.mha
            ${TEMP}/LaplacianRecursiveGaussianImageFilter2.mha
  $<TARGET_FILE:LaplacianRecursiveGaussianImageFilter2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/LaplacianRecursiveGaussianImageFilter2.mha
        3
)
itk_add_test(NAME  ResampleImageFilter6Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilter6PixelCentered.png
            ${TEMP}/ResampleImageFilter6PixelCentered.png
  $<TARGET_FILE:ResampleImageFilter6>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanHeadSlice.png
        ${TEMP}/ResampleImageFilter6PixelCentered.png
        3
)
itk_add_test(NAME  ResampleImageFilter7Test COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleImageFilter7PixelCentered.png
            ${TEMP}/ResampleImageFilter7PixelCentered.png
  --compareNumberOfPixelsTolerance 1
  $<TARGET_FILE:ResampleImageFilter7>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanHeadSlice.png
        ${TEMP}/ResampleImageFilter7PixelCentered.png
        33
)
itk_add_test(NAME  ResampleVolumesToBeIsotropicTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ResampleVolumesToBeIsotropic.mha
            ${TEMP}/ResampleVolumesToBeIsotropic.mha
  $<TARGET_FILE:ResampleVolumesToBeIsotropic>
        ${ITK_SOURCE_DIR}/Testing/Data/Input/mri3D.mhd
        ${TEMP}/ResampleVolumesToBeIsotropic.mha
        0 255
)
itk_add_test(NAME  ScaleSpaceGenerator2DTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/ScaleSpaceGenerator2D005.mhd
            ${TEMP}/ScaleSpaceGenerator2D005.mhd
  $<TARGET_FILE:ScaleSpaceGenerator2D>
        ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
        ${TEMP}/ScaleSpaceGenerator2D
        10
)
itk_add_test(NAME  SubsampleVolumeTest COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/SubsampleVolume.mha
            ${TEMP}/SubsampleVolume.mha
  $<TARGET_FILE:SubsampleVolume>
        ${ITK_SOURCE_DIR}/Testing/Data/Input/HeadMRVolume.mhd
        ${TEMP}/SubsampleVolume.mha
        2 2 2
)
itk_add_test(NAME  SpatialObjectToImage1 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:SpatialObjectToImage1>
        ${TEMP}/SpatialObjectToImage1.mha
)
itk_add_test(NAME  SpatialObjectToImage2 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:SpatialObjectToImage2>
        ${TEMP}/SpatialObjectToImage2.mha
)
itk_add_test(NAME  SpatialObjectToImage3 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:SpatialObjectToImage3>
        ${TEMP}/SpatialObjectToImage3.mha
)
# The test tests 3 filters.
#   DiffusionTensor3DReconstructionImageFilter
#   TensorFractionalAnisotropyImageFilter
#   TensorRelativeAnisotropyImageFilter
#
itk_add_test(NAME  DiffusionTensor3DReconstructionImageFilterTest1 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/FACorpusCallosum.mhd
            ${TEMP}/FACorpusCallosum.mhd
  $<TARGET_FILE:DiffusionTensor3DReconstructionImageFilter>
        ${ITK_SOURCE_DIR}/Testing/Data/Input/DwiCorpusCallosum.nhdr
        80 ${TEMP}/Tensors.mhd ${TEMP}/FACorpusCallosum.mhd
        ${TEMP}/RACorpusCallosum.mhd 0
)
itk_add_test(NAME  DiffusionTensor3DReconstructionImageFilterTest2 COMMAND ${ITK_TEST_DRIVER}
  --compare ${BASELINE}/RACorpusCallosum.mhd
            ${TEMP}/RACorpusCallosum.mhd
  $<TARGET_FILE:DiffusionTensor3DReconstructionImageFilter>
        ${ITK_SOURCE_DIR}/Testing/Data/Input/DwiCorpusCallosum.nhdr
        80 ${TEMP}/Tensors.mhd ${TEMP}/FACorpusCallosum.mhd
        ${TEMP}/RACorpusCallosum.mhd 0
)
endif(BUILD_TESTING)
endif( NOT ITK_DISABLE_CXX_TESTING )
add_executable(DiffusionTensor3DReconstructionImageFilter DiffusionTensor3DReconstructionImageFilter.cxx)
target_link_libraries(DiffusionTensor3DReconstructionImageFilter ${ITK_LIBRARIES})
add_executable(FFTDirectInverse FFTDirectInverse.cxx )
target_link_libraries(FFTDirectInverse  ${ITK_LIBRARIES})
add_executable(ZeroCrossingBasedEdgeDetectionImageFilter ZeroCrossingBasedEdgeDetectionImageFilter.cxx )
target_link_libraries(ZeroCrossingBasedEdgeDetectionImageFilter  ${ITK_LIBRARIES})
add_executable(WarpImageFilter1 WarpImageFilter1.cxx )
target_link_libraries(WarpImageFilter1  ${ITK_LIBRARIES})
add_executable(SurfaceExtraction SurfaceExtraction.cxx )
target_link_libraries(SurfaceExtraction  ${ITK_LIBRARIES})
add_executable(DigitallyReconstructedRadiograph1 DigitallyReconstructedRadiograph1.cxx )
target_link_libraries(DigitallyReconstructedRadiograph1  ${ITK_LIBRARIES})
add_executable(ResampleImageFilter7 ResampleImageFilter7.cxx )
target_link_libraries(ResampleImageFilter7  ${ITK_LIBRARIES})
