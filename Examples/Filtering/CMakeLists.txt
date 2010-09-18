project(FilteringExamples)
include_regular_expression("^.*$")
add_executable(CompositeFilterExample CompositeFilterExample.cxx )
target_link_libraries(CompositeFilterExample  ITKIO ITKBasicFilters)
add_executable(FlipImageFilter FlipImageFilter.cxx )
target_link_libraries(FlipImageFilter  ITKIO)
add_executable(FFTImageFilter FFTImageFilter.cxx )
target_link_libraries(FFTImageFilter  ITKIO)
if( USE_FFTWF )
  add_executable(FFTDirectInverse2 FFTDirectInverse2.cxx )
  target_link_libraries(FFTDirectInverse2  ITKIO ITKAlgorithms )
endif( USE_FFTWF )
add_executable(GradientMagnitudeImageFilter GradientMagnitudeImageFilter.cxx )
target_link_libraries(GradientMagnitudeImageFilter  ITKIO)
add_executable(GradientMagnitudeRecursiveGaussianImageFilter GradientMagnitudeRecursiveGaussianImageFilter.cxx )
target_link_libraries(GradientMagnitudeRecursiveGaussianImageFilter  ITKIO)
add_executable(GradientRecursiveGaussianImageFilter GradientRecursiveGaussianImageFilter.cxx )
target_link_libraries(GradientRecursiveGaussianImageFilter  ITKIO)
add_executable(GrayscaleFunctionDilateImageFilter GrayscaleFunctionDilateImageFilter.cxx )
target_link_libraries(GrayscaleFunctionDilateImageFilter  ITKIO)
add_executable(BinaryThresholdImageFilter BinaryThresholdImageFilter.cxx )
target_link_libraries(BinaryThresholdImageFilter  ITKIO)
add_executable(BinaryMedianImageFilter BinaryMedianImageFilter.cxx )
target_link_libraries(BinaryMedianImageFilter  ITKIO)
add_executable(OtsuThresholdImageFilter OtsuThresholdImageFilter.cxx )
target_link_libraries(OtsuThresholdImageFilter  ITKIO)
add_executable(OtsuMultipleThresholdImageFilter OtsuMultipleThresholdImageFilter.cxx )
target_link_libraries(OtsuMultipleThresholdImageFilter
                       ITKIO ITKStatistics)
add_executable(ThresholdImageFilter ThresholdImageFilter.cxx )
target_link_libraries(ThresholdImageFilter  ITKIO)
add_executable(CastingImageFilters CastingImageFilters.cxx )
target_link_libraries(CastingImageFilters  ITKIO)
add_executable(CannyEdgeDetectionImageFilter CannyEdgeDetectionImageFilter.cxx )
target_link_libraries(CannyEdgeDetectionImageFilter  ITKIO)
add_executable(MedianImageFilter MedianImageFilter.cxx )
target_link_libraries(MedianImageFilter  ITKIO)
add_executable(MeanImageFilter MeanImageFilter.cxx )
target_link_libraries(MeanImageFilter  ITKIO)
add_executable(MathematicalMorphologyBinaryFilters MathematicalMorphologyBinaryFilters.cxx )
target_link_libraries(MathematicalMorphologyBinaryFilters  ITKIO)
add_executable(MathematicalMorphologyGrayscaleFilters MathematicalMorphologyGrayscaleFilters.cxx )
target_link_libraries(MathematicalMorphologyGrayscaleFilters  ITKIO)
add_executable(BinaryMinMaxCurvatureFlowImageFilter BinaryMinMaxCurvatureFlowImageFilter.cxx )
target_link_libraries(BinaryMinMaxCurvatureFlowImageFilter  ITKBasicFilters ITKIO)
add_executable(DiscreteGaussianImageFilter DiscreteGaussianImageFilter.cxx )
target_link_libraries(DiscreteGaussianImageFilter  ITKBasicFilters ITKIO)
add_executable(GaussianBlurImageFunction GaussianBlurImageFunction.cxx )
target_link_libraries(GaussianBlurImageFunction  ITKBasicFilters ITKIO)
add_executable(LaplacianRecursiveGaussianImageFilter1 LaplacianRecursiveGaussianImageFilter1.cxx )
target_link_libraries(LaplacianRecursiveGaussianImageFilter1  ITKBasicFilters ITKIO)
add_executable(ScaleSpaceGenerator2D ScaleSpaceGenerator2D.cxx )
target_link_libraries(ScaleSpaceGenerator2D  ITKBasicFilters ITKIO)
add_executable(BinomialBlurImageFilter BinomialBlurImageFilter.cxx )
target_link_libraries(BinomialBlurImageFilter  ITKBasicFilters ITKIO)
add_executable(CurvatureAnisotropicDiffusionImageFilter CurvatureAnisotropicDiffusionImageFilter.cxx )
target_link_libraries(CurvatureAnisotropicDiffusionImageFilter  ITKBasicFilters ITKIO)
add_executable(VectorGradientAnisotropicDiffusionImageFilter VectorGradientAnisotropicDiffusionImageFilter.cxx )
target_link_libraries(VectorGradientAnisotropicDiffusionImageFilter  ITKBasicFilters ITKIO)
add_executable(VectorCurvatureAnisotropicDiffusionImageFilter VectorCurvatureAnisotropicDiffusionImageFilter.cxx )
target_link_libraries(VectorCurvatureAnisotropicDiffusionImageFilter  ITKBasicFilters ITKIO)
add_executable(RGBToGrayscale RGBToGrayscale.cxx )
target_link_libraries(RGBToGrayscale  ITKBasicFilters ITKIO)
add_executable(BilateralImageFilter BilateralImageFilter.cxx )
target_link_libraries(BilateralImageFilter  ITKBasicFilters ITKIO)
add_executable(ResampleOrientedImageFilter ResampleOrientedImageFilter.cxx )
target_link_libraries(ResampleOrientedImageFilter  ITKBasicFilters ITKIO)
add_executable(ResampleImageFilter2 ResampleImageFilter2.cxx )
target_link_libraries(ResampleImageFilter2  ITKBasicFilters ITKIO)
add_executable(GradientAnisotropicDiffusionImageFilter GradientAnisotropicDiffusionImageFilter.cxx )
target_link_libraries(GradientAnisotropicDiffusionImageFilter  ITKBasicFilters ITKIO)
add_executable(CurvatureFlowImageFilter CurvatureFlowImageFilter.cxx )
target_link_libraries(CurvatureFlowImageFilter  ITKBasicFilters ITKIO)
add_executable(GradientVectorFlowImageFilter GradientVectorFlowImageFilter.cxx )
target_link_libraries(GradientVectorFlowImageFilter  ITKBasicFilters ITKIO)
add_executable(SigmoidImageFilter SigmoidImageFilter.cxx )
target_link_libraries(SigmoidImageFilter  ITKIO)
add_executable(RGBCurvatureAnisotropicDiffusionImageFilter RGBCurvatureAnisotropicDiffusionImageFilter.cxx )
target_link_libraries(RGBCurvatureAnisotropicDiffusionImageFilter  ITKBasicFilters ITKIO)
add_executable(RGBGradientAnisotropicDiffusionImageFilter RGBGradientAnisotropicDiffusionImageFilter.cxx )
target_link_libraries(RGBGradientAnisotropicDiffusionImageFilter  ITKBasicFilters ITKIO)
add_executable(SignedDanielssonDistanceMapImageFilter SignedDanielssonDistanceMapImageFilter.cxx )
target_link_libraries(SignedDanielssonDistanceMapImageFilter  ITKBasicFilters ITKIO)
add_executable(SecondDerivativeRecursiveGaussianImageFilter SecondDerivativeRecursiveGaussianImageFilter.cxx )
target_link_libraries(SecondDerivativeRecursiveGaussianImageFilter  ITKIO)
add_executable(DerivativeImageFilter DerivativeImageFilter.cxx )
target_link_libraries(DerivativeImageFilter  ITKIO)
add_executable(DanielssonDistanceMapImageFilter DanielssonDistanceMapImageFilter.cxx )
target_link_libraries(DanielssonDistanceMapImageFilter  ITKBasicFilters ITKIO)
add_executable(AntiAliasBinaryImageFilter AntiAliasBinaryImageFilter.cxx )
target_link_libraries(AntiAliasBinaryImageFilter  ITKIO)
add_executable(ResampleImageFilter ResampleImageFilter.cxx )
target_link_libraries(ResampleImageFilter  ITKBasicFilters ITKIO)
add_executable(SmoothingRecursiveGaussianImageFilter SmoothingRecursiveGaussianImageFilter.cxx )
target_link_libraries(SmoothingRecursiveGaussianImageFilter  ITKBasicFilters ITKIO)
add_executable(SmoothingRecursiveGaussianImageFilter2 SmoothingRecursiveGaussianImageFilter2.cxx )
target_link_libraries(SmoothingRecursiveGaussianImageFilter2  ITKBasicFilters ITKIO)
add_executable(LaplacianRecursiveGaussianImageFilter2 LaplacianRecursiveGaussianImageFilter2.cxx )
target_link_libraries(LaplacianRecursiveGaussianImageFilter2  ITKBasicFilters ITKIO)
add_executable(MorphologicalImageEnhancement MorphologicalImageEnhancement.cxx )
target_link_libraries(MorphologicalImageEnhancement  ITKIO)
add_executable(MinMaxCurvatureFlowImageFilter MinMaxCurvatureFlowImageFilter.cxx )
target_link_libraries(MinMaxCurvatureFlowImageFilter  ITKBasicFilters ITKIO)
add_executable(FFTImageFilterFourierDomainFiltering FFTImageFilterFourierDomainFiltering.cxx )
target_link_libraries(FFTImageFilterFourierDomainFiltering  ITKIO)
add_executable(LaplacianImageFilter LaplacianImageFilter.cxx )
target_link_libraries(LaplacianImageFilter  ITKBasicFilters ITKIO)
add_executable(ResampleImageFilter3 ResampleImageFilter3.cxx )
target_link_libraries(ResampleImageFilter3  ITKBasicFilters ITKIO)
add_executable(ResampleImageFilter4 ResampleImageFilter4.cxx )
target_link_libraries(ResampleImageFilter4  ITKBasicFilters ITKIO)
add_executable(VotingBinaryHoleFillingImageFilter VotingBinaryHoleFillingImageFilter.cxx )
target_link_libraries(VotingBinaryHoleFillingImageFilter  ITKIO)
add_executable(VotingBinaryIterativeHoleFillingImageFilter VotingBinaryIterativeHoleFillingImageFilter.cxx )
target_link_libraries(VotingBinaryIterativeHoleFillingImageFilter  ITKIO)
add_executable(ResampleImageFilter5 ResampleImageFilter5.cxx )
target_link_libraries(ResampleImageFilter5  ITKBasicFilters ITKIO)
add_executable(ResampleImageFilter6 ResampleImageFilter6.cxx )
target_link_libraries(ResampleImageFilter6  ITKBasicFilters ITKIO)
add_executable(ResampleImageFilter8 ResampleImageFilter8.cxx )
target_link_libraries(ResampleImageFilter8  ITKBasicFilters ITKIO)
add_executable(ResampleImageFilter9 ResampleImageFilter9.cxx )
target_link_libraries(ResampleImageFilter9  ITKBasicFilters ITKIO)
add_executable(ResampleVolumesToBeIsotropic ResampleVolumesToBeIsotropic.cxx )
target_link_libraries(ResampleVolumesToBeIsotropic  ITKBasicFilters ITKIO)
add_executable(SubsampleVolume SubsampleVolume.cxx )
target_link_libraries(SubsampleVolume  ITKBasicFilters ITKIO)
add_executable(SpatialObjectToImage1 SpatialObjectToImage1.cxx )
target_link_libraries(SpatialObjectToImage1  ITKBasicFilters ITKIO)
add_executable(SpatialObjectToImage2 SpatialObjectToImage2.cxx )
target_link_libraries(SpatialObjectToImage2  ITKBasicFilters ITKIO)
add_executable(SpatialObjectToImage3 SpatialObjectToImage3.cxx )
target_link_libraries(SpatialObjectToImage3  ITKBasicFilters ITKIO)
add_executable(LaplacianSharpeningImageFilter LaplacianSharpeningImageFilter.cxx )
target_link_libraries(LaplacianSharpeningImageFilter  ITKBasicFilters ITKIO)
add_executable(VectorIndexSelection VectorIndexSelection.cxx )
target_link_libraries(VectorIndexSelection  ITKBasicFilters ITKIO)
add_executable(VesselnessMeasureImageFilter VesselnessMeasureImageFilter.cxx )
target_link_libraries(VesselnessMeasureImageFilter  ITKBasicFilters ITKIO)
set(FILTERING_EXAMPLES ${CXX_TEST_PATH}/FilteringExamples)
set(FILTERING_EXAMPLES2 ${CXX_TEST_PATH}/FilteringExamples2)
set(FILTERING_EXAMPLES3 ${CXX_TEST_PATH}/FilteringExamples3)
set(FILTERING_EXAMPLES4 ${CXX_TEST_PATH}/FilteringExamples4)
set(FILTERING_EXAMPLES5 ${CXX_TEST_PATH}/FilteringExamples5)
set(FILTERING_EXAMPLES6 ${CXX_TEST_PATH}/FilteringExamples6)
set(FILTERING_EXAMPLES7 ${CXX_TEST_PATH}/FilteringExamples7)
set(FILTERING_EXAMPLES8 ${CXX_TEST_PATH}/FilteringExamples8)
set(FILTERING_EXAMPLES9 ${CXX_TEST_PATH}/FilteringExamples9)
set(BASELINE ${ITK_DATA_ROOT}/Baseline/Filtering)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)
if( NOT ITK_DISABLE_CXX_TESTING )
if(BUILD_TESTING)
add_test(BilateralImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/BilateralImageFilter.png
            ${TEMP}/BilateralImageFilter.png
  BilateralImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BilateralImageFilter.png
        6.0 5.0
)
add_test( BinaryMinMaxCurvatureFlowImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/BinaryMinMaxCurvatureFlowImageFilter.png
            ${TEMP}/BinaryMinMaxCurvatureFlowImageFilter.png
  BinaryMinMaxCurvatureFlowImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BinaryMinMaxCurvatureFlowImageFilter.png
        10 .125 1 128
)
add_test( BinaryThresholdImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/BinaryThresholdImageFilter.png
            ${TEMP}/BinaryThresholdImageFilter.png
  BinaryThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BinaryThresholdImageFilter.png
        150 180 0 255
)
add_test( BinaryMedianImageFilterTest1 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/BinaryMedianImageFilter1.png
            ${TEMP}/BinaryMedianImageFilter1.png
  BinaryMedianImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/BinaryMedianImageFilter1.png
        1 1
)
add_test( BinaryMedianImageFilterTest2 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/BinaryMedianImageFilter2.png
            ${TEMP}/BinaryMedianImageFilter2.png
  BinaryMedianImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/BinaryMedianImageFilter2.png
        2 2
)
add_test( BinaryMedianImageFilterTest3 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/BinaryMedianImageFilter3.png
            ${TEMP}/BinaryMedianImageFilter3.png
  BinaryMedianImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/BinaryMedianImageFilter3.png
        3 3
)
add_test( VotingBinaryHoleFillingImageFilterTest1 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VotingBinaryHoleFillingImageFilter1.png
            ${TEMP}/VotingBinaryHoleFillingImageFilter1.png
  VotingBinaryHoleFillingImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryHoleFillingImageFilter1.png
        1 1
)
add_test( VotingBinaryHoleFillingImageFilterTest2 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VotingBinaryHoleFillingImageFilter2.png
            ${TEMP}/VotingBinaryHoleFillingImageFilter2.png
  VotingBinaryHoleFillingImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryHoleFillingImageFilter2.png
        2 2
)
add_test( VotingBinaryHoleFillingImageFilterTest3 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VotingBinaryHoleFillingImageFilter3.png
            ${TEMP}/VotingBinaryHoleFillingImageFilter3.png
  VotingBinaryHoleFillingImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryHoleFillingImageFilter3.png
        3 3
)
add_test( VotingBinaryIterativeHoleFillingImageFilterTest1 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VotingBinaryIterativeHoleFillingImageFilter1.png
            ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter1.png
  VotingBinaryIterativeHoleFillingImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter1.png
        1 1 10
)
add_test( VotingBinaryIterativeHoleFillingImageFilterTest2 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VotingBinaryIterativeHoleFillingImageFilter2.png
            ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter2.png
  VotingBinaryIterativeHoleFillingImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter2.png
        2 2 10
)
add_test( VotingBinaryIterativeHoleFillingImageFilterTest3 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VotingBinaryIterativeHoleFillingImageFilter3.png
            ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter3.png
  VotingBinaryIterativeHoleFillingImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter3.png
        3 3 10
)
add_test( BinomialBlurImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/BinomialBlurImageFilter.png
            ${TEMP}/BinomialBlurImageFilter.png
  BinomialBlurImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BinomialBlurImageFilter.png
        5
)
add_test( CurvatureAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/CurvatureAnisotropicDiffusionImageFilter.png
            ${TEMP}/CurvatureAnisotropicDiffusionImageFilter.png
  CurvatureAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/CurvatureAnisotropicDiffusionImageFilter.png
        5 0.125 3
)
add_test( CurvatureFlowImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/CurvatureFlowImageFilter.png
            ${TEMP}/CurvatureFlowImageFilter.png
  CurvatureFlowImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/CurvatureFlowImageFilter.png
        5 0.125 1.0
)
add_test( DanielssonDistanceMapImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/DanielssonDistanceMapImageFilter.png
            ${TEMP}/DanielssonDistanceMapImageFilter.png
  DanielssonDistanceMapImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/FivePoints.png
        ${TEMP}/DanielssonDistanceMapImageFilter.png
        ${TEMP}/DanielssonDistanceMapImageFilter2.png
        ${TEMP}/DanielssonDistanceMapImageFilter.mha
)
add_test( DiscreteGaussianImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/DiscreteGaussianImageFilter.png
            ${TEMP}/DiscreteGaussianImageFilter.png
  DiscreteGaussianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/DiscreteGaussianImageFilter.png
        1.73 10
)
add_test( GradientAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES8}
  --compare ${BASELINE}/GradientAnisotropicDiffusionImageFilter.png
            ${TEMP}/GradientAnisotropicDiffusionImageFilter.png
  GradientAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GradientAnisotropicDiffusionImageFilter.png
        5 0.125 1.0
)
add_test( GradientMagnitudeImageFilterTest ${FILTERING_EXAMPLES8}
  --compare ${BASELINE}/GradientMagnitudeImageFilter.png
            ${TEMP}/GradientMagnitudeImageFilter.png
  GradientMagnitudeImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GradientMagnitudeImageFilter.png
)
add_test( GradientMagnitudeRecursiveGaussianImageFilterTest ${FILTERING_EXAMPLES8}
  --compare ${BASELINE}/GradientMagnitudeRecursiveGaussianImageFilter.png
            ${TEMP}/GradientMagnitudeRecursiveGaussianImageFilter.png
  GradientMagnitudeRecursiveGaussianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GradientMagnitudeRecursiveGaussianImageFilter.png
        5
)
add_test( GradientRecursiveGaussianImageFilterTest ${FILTERING_EXAMPLES8}
  GradientRecursiveGaussianImageFilterTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/HeadMRVolume.mhd
        ${TEMP}/GradientRecursiveGaussianImageFilterTest.mha
        1.5  # Sigma
)
add_test( GradientVectorFlowImageFilterTest ${FILTERING_EXAMPLES8}
  GradientVectorFlowImageFilterTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/GradientRecursiveGaussianImageFilterTest.mha
        ${TEMP}/GradientVectorFlowImageFilterTest.mha
        5        # number of iterations
        2000.0   # noise level
)
set_tests_properties( GradientVectorFlowImageFilterTest
  PROPERTIES DEPENDS GradientRecursiveGaussianImageFilterTest)
add_test( DerivativeImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/DerivativeImageFilter.mha
            ${TEMP}/DerivativeImageFilter.mha
  DerivativeImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/DerivativeImageFilter.mha
        ${TEMP}/DerivativeImageFilter.png
        1 1
)
add_test( LaplacianImageFilterTest ${FILTERING_EXAMPLES8}
  --compare ${BASELINE}/LaplacianImageFilter.png
            ${TEMP}/LaplacianImageFilter.png
  LaplacianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/LaplacianImageFilter.png
)
add_test( LaplacianSharpeningImageFilterTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/LaplacianSharpeningImageFilter.png
            ${TEMP}/LaplacianSharpeningImageFilter.png
  LaplacianSharpeningImageFilterTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
        ${TEMP}/LaplacianSharpeningImageFilter.png
)
add_test( MathematicalMorphologyBinaryFiltersTest ${FILTERING_EXAMPLES8}
  --compare ${BASELINE}/MathematicalMorphologyBinaryFiltersErode.png
            ${TEMP}/MathematicalMorphologyBinaryFiltersErode.png
  MathematicalMorphologyBinaryFiltersTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MathematicalMorphologyBinaryFiltersErode.png
        ${TEMP}/MathematicalMorphologyBinaryFiltersDilate.png
        150 180
)
add_test( MathematicalMorphologyGrayscaleFiltersTest ${FILTERING_EXAMPLES8}
  --compare ${BASELINE}/MathematicalMorphologyGrayscaleFiltersErode.png
            ${TEMP}/MathematicalMorphologyGrayscaleFiltersErode.png
  MathematicalMorphologyGrayscaleFiltersTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MathematicalMorphologyGrayscaleFiltersErode.png
        ${TEMP}/MathematicalMorphologyGrayscaleFiltersDilate.png
)
add_test( MeanImageFilterTest ${FILTERING_EXAMPLES8}
  --compare ${BASELINE}/MeanImageFilter.png
            ${TEMP}/MeanImageFilter.png
  MeanImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MeanImageFilter.png
)
add_test( MedianImageFilterTest ${FILTERING_EXAMPLES8}
  --compare ${BASELINE}/MedianImageFilter.png
            ${TEMP}/MedianImageFilter.png
  MedianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MedianImageFilter.png
        1
)
add_test( ResampleImageFilterTest1 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilterTest1.png
            ${TEMP}/ResampleImageFilterTest1.png
  ResampleImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest1.png
        0
)
add_test( ResampleOrientedImageFilterTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleOrientedImageFilterTest.png
            ${TEMP}/ResampleOrientedImageFilterTest.png
  ResampleOrientedImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanHeadSlice.png
        ${TEMP}/ResampleOrientedImageFilterTest.png
        0 1 1 0
)
add_test( ResampleImageFilterTest2 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilterTest2.png
            ${TEMP}/ResampleImageFilterTest2.png
  ResampleImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest2.png
        1
)
add_test( ResampleImageFilterTest3 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilterTest3.png
            ${TEMP}/ResampleImageFilterTest3.png
  ResampleImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest3.png
        2
)
add_test( ResampleImageFilterTest4 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilterTest4.png
            ${TEMP}/ResampleImageFilterTest4.png
  ResampleImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest4.png
        3
)
add_test( ResampleImageFilter2Test1 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter2Test1.png
            ${TEMP}/ResampleImageFilter2Test1.png
  ResampleImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test1.png
        0
)
add_test( ResampleImageFilter2Test2 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter2Test2.png
            ${TEMP}/ResampleImageFilter2Test2.png
  ResampleImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test2.png
        1
)
add_test( ResampleImageFilter2Test3 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter2Test3.png
            ${TEMP}/ResampleImageFilter2Test3.png
  ResampleImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test3.png
        2
)
add_test( ResampleImageFilter2Test4 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter2Test4.png
            ${TEMP}/ResampleImageFilter2Test4.png
  ResampleImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test4.png
        3
)
add_test( ResampleImageFilter2Test5 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter2Test5.png
            ${TEMP}/ResampleImageFilter2Test5.png
  ResampleImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test5.png
        3
)
add_test( ResampleImageFilter3Test1 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter3Test1PixelCentered.png
            ${TEMP}/ResampleImageFilter3Test1PixelCentered.png
  ResampleImageFilter3Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter3Test1PixelCentered.png
        0
)

add_test( ResampleImageFilter4Test ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter4TestPixelCentered.png
            ${TEMP}/ResampleImageFilter4TestPixelCentered.png
  ResampleImageFilter4Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter4TestPixelCentered.png
        20
)
add_test( ResampleImageFilter5Test ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter5TestPixelCentered.png
            ${TEMP}/ResampleImageFilter5TestPixelCentered.png
  ResampleImageFilter5Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter5TestPixelCentered.png
        10 1.2
)
add_test( ResampleImageFilter8Test ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter8TestPixelCentered.png
            ${TEMP}/ResampleImageFilter8TestPixelCentered.png
  ResampleImageFilter8Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter8TestPixelCentered.png
        45
)
add_test( ResampleImageFilter9Test ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter9TestPixelCentered.png
            ${TEMP}/ResampleImageFilter9TestNearestPixelCentered.png
  ResampleImageFilter9Test
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/ResampleImageFilter9TestNearestPixelCentered.png
        ${TEMP}/ResampleImageFilter9TestLinearPixelCentered.png
)
add_test( SigmoidImageFilterTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/SigmoidImageFilter.png
            ${TEMP}/SigmoidImageFilter.png
  SigmoidImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/SigmoidImageFilter.png
        10 240 10 170
)
add_test( SmoothingRecursiveGaussianImageFilterTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/SmoothingRecursiveGaussianImageFilter.png
            ${TEMP}/SmoothingRecursiveGaussianImageFilter.png
  SmoothingRecursiveGaussianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/SmoothingRecursiveGaussianImageFilter.png
        3
)
add_test( SmoothingRecursiveGaussianImageFilter2Test ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/SmoothingRecursiveGaussianImageFilter2.png
            ${TEMP}/SmoothingRecursiveGaussianImageFilter2.png
  SmoothingRecursiveGaussianImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/SmoothingRecursiveGaussianImageFilter2.png
        3
)
add_test( ThresholdAboveBelowOutsideImageFilterTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ThresholdAboveImageFilter.png
            ${TEMP}/ThresholdAboveImageFilter.png
  --compare ${BASELINE}/ThresholdBelowImageFilter.png
            ${TEMP}/ThresholdBelowImageFilter.png
  --compare ${BASELINE}/ThresholdOutsideImageFilter.png
            ${TEMP}/ThresholdOutsideImageFilter.png
  ThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdBelowImageFilter.png
        ${TEMP}/ThresholdAboveImageFilter.png
        ${TEMP}/ThresholdOutsideImageFilter.png
)
add_test( VectorGradientAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VectorGradientAnisotropicDiffusionImageFilter.png
            ${TEMP}/VectorGradientAnisotropicDiffusionImageFilter.png
  VectorGradientAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/VectorGradientAnisotropicDiffusionImageFilterOriginal.png
        ${TEMP}/VectorGradientAnisotropicDiffusionImageFilter.png
        .25 5
)
add_test( VectorCurvatureAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VectorCurvatureAnisotropicDiffusionImageFilter.png
            ${TEMP}/VectorCurvatureAnisotropicDiffusionImageFilter.png
  VectorCurvatureAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/VectorCurvatureAnisotropicDiffusionImageFilterOriginal.png
        ${TEMP}/VectorCurvatureAnisotropicDiffusionImageFilter.png
        0.125 5
)
add_test( VectorIndexSelectionTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VectorIndexSelection.png
            ${TEMP}/VectorIndexSelection.png
  VectorIndexSelectionTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/VectorIndexSelection.png
        1
)
add_test( ZeroCrossingBasedEdgeDetectionImageFilterTest ${FILTERING_EXAMPLES8}
  --compare ${BASELINE}/ZeroCrossingBasedEdgeDetectionImageFilter1.png
            ${TEMP}/ZeroCrossingBasedEdgeDetectionImageFilter1.png
  ZeroCrossingBasedEdgeDetectionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ZeroCrossingBasedEdgeDetectionImageFilter1.png
        5.0
        0.03
)
add_test( RGBGradientAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES8}
  --compare ${BASELINE}/RGBGradientAnisotropicDiffusionImageFilter.png
            ${TEMP}/RGBGradientAnisotropicDiffusionImageFilter.png
  RGBGradientAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/RGBGradientAnisotropicDiffusionImageFilter.png
        0.125 20
)
add_test( RGBCurvatureAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES8}
  --compare ${BASELINE}/RGBCurvatureAnisotropicDiffusionImageFilter.png
            ${TEMP}/RGBCurvatureAnisotropicDiffusionImageFilter.png
  RGBCurvatureAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/RGBCurvatureAnisotropicDiffusionImageFilter.png
        0.125 20
)
add_test( MorphologicalImageEnhancementTest ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/MorphologicalImageEnhancement.png
            ${TEMP}/MorphologicalImageEnhancement.png
  MorphologicalImageEnhancementTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/peppers.png
        ${TEMP}/MorphologicalImageEnhancement.png
        3
)
add_test( OtsuThresholdImageFilterTest ${FILTERING_EXAMPLES5}
  --compare ${BASELINE}/OtsuThresholdImageFilter.png
            ${TEMP}/OtsuThresholdImageFilter.png
  OtsuThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/OtsuThresholdImageFilter.png
        255 0
)
add_test( OtsuMultipleThresholdImageFilterTest ${FILTERING_EXAMPLES5}
  --compare ${BASELINE}/OtsuMultipleThresholdOutput002.png
            ${TEMP}/OtsuMultipleThresholdOutput002.png
  OtsuMultipleThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/OtsuMultipleThresholdOutput
        png 3
)
add_test( DigitallyReconstructedRadiograph1Test ${FILTERING_EXAMPLES7}
  --compare ${BASELINE}/DigitallyReconstructedRadiograph1.png
            ${TEMP}/DigitallyReconstructedRadiograph1.png
  DigitallyReconstructedRadiograph1Test
        -o ${TEMP}/DigitallyReconstructedRadiograph1.png
)
add_test( AntiAliasBinaryImageFilterTest ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/AntiAliasBinaryImageFilter.png
            ${TEMP}/AntiAliasBinaryImageFilter.png
  AntiAliasBinaryImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VentricleModel.png
        ${TEMP}/AntiAliasBinaryImageFilterDoublePixelType.mha
        ${TEMP}/AntiAliasBinaryImageFilter.png
        .001 50
)
add_test( CannyEdgeDetectionImageFilterTest ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/CannyEdgeDetectionImageFilter.png
            ${TEMP}/CannyEdgeDetectionImageFilter.png
  CannyEdgeDetectionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/CannyEdgeDetectionImageFilter.png
        9     # Sigma
        0     # Upper Threshold
        0     # Lower Threshold
)
add_test( CannyEdgeDetectionImageFilterTest2 ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/CannyEdgeDetectionImageFilter2.png
            ${TEMP}/CannyEdgeDetectionImageFilter2.png
  CannyEdgeDetectionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/CannyEdgeDetectionImageFilter2.png
        9     # Sigma
        5     # Upper Threshold
        1     # Lower Threshold
)
add_test( FlipImageFilterTest1 ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/FlipImageFilterX.png
            ${TEMP}/FlipImageFilterX.png
  FlipImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/FlipImageFilterX.png
        1 0
)
add_test( FlipImageFilterTest2 ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/FlipImageFilterY.png
            ${TEMP}/FlipImageFilterY.png
  FlipImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/FlipImageFilterY.png
        0 1
)
add_test( FlipImageFilterTest3 ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/FlipImageFilterXY.png
            ${TEMP}/FlipImageFilterXY.png
  FlipImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/FlipImageFilterXY.png
        1 1
)
add_test( GaussianBlurImageFunctionTest ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/GaussianBlurImageFunction.mha
            ${TEMP}/GaussianBlurImageFunction.mha
  GaussianBlurImageFunctionTest
        ${ITK_SOURCE_DIR}/Examples/Data/VentricleModel.png
        ${TEMP}/GaussianBlurImageFunction.mha
        2 3
)
add_test( FFTImageFilterTest ${FILTERING_EXAMPLES7}
  --compare ${BASELINE}/FFTImageFilterReal.png
            ${TEMP}/FFTImageFilterReal.png
  FFTImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/FatMRISlice.png
        ${TEMP}/FFTImageFilterReal.png
        ${TEMP}/FFTImageFilterImaginary.png
        ${TEMP}/FFTImageFilterComplex.mhd
)
add_test( FFTDirectInverseTest ${FILTERING_EXAMPLES6}
  --compare ${ITK_SOURCE_DIR}/Examples/Data/Point.png
            ${TEMP}/FFTDirectInversePoint.png
  FFTDirectInverseTest
        ${ITK_SOURCE_DIR}/Examples/Data/Point.png
        ${TEMP}/FFTDirectInversePoint.png
)
if( USE_FFTWF )
#${BASELINE}/FFTDirectInverse2Point.png
  add_test( FFTDirectInverse2Test ${FILTERING_EXAMPLES6}
    --compare ${ITK_SOURCE_DIR}/Examples/Data/Point.png
              ${TEMP}/FFTDirectInverse2Point.png
    FFTDirectInverse2Test
          ${ITK_SOURCE_DIR}/Examples/Data/Point.png
          ${TEMP}/FFTDirectInverse2Point.png
  )
endif( USE_FFTWF )
add_test( FFTImageFilterTest2 ${FILTERING_EXAMPLES7}
  FFTImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/FFTcenteredDC.png
        ${TEMP}/FFTImageFilterCenteredDCReal.png
        ${TEMP}/FFTImageFilterCenteredDCImaginary.png
        ${TEMP}/FFTImageFilterComplex2.mhd
)
add_test( FFTImageFilterTest3 ${FILTERING_EXAMPLES7}
  FFTImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/FFTcorneredDC.png
        ${TEMP}/FFTImageFilterCorneredDCReal.png
        ${TEMP}/FFTImageFilterCorneredDCImaginary.png
        ${TEMP}/FFTImageFilterComplex3.mhd
)
add_test( RGBToGrayscaleTest ${FILTERING_EXAMPLES7}
  --compare ${BASELINE}/RGBToGrayscale.png
            ${TEMP}/RGBToGrayscale.png
  RGBToGrayscaleTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanHeadSlice.png
        ${TEMP}/RGBToGrayscale.png
)
add_test( LaplacianRecursiveGaussianImageFilter1Test ${FILTERING_EXAMPLES7}
  --compare ${BASELINE}/LaplacianRecursiveGaussianImageFilter1.mha
            ${TEMP}/LaplacianRecursiveGaussianImageFilter1.mha
  LaplacianRecursiveGaussianImageFilter1Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/LaplacianRecursiveGaussianImageFilter1.mha
        3
)
add_test( LaplacianRecursiveGaussianImageFilter2Test ${FILTERING_EXAMPLES7}
  --compare ${BASELINE}/LaplacianRecursiveGaussianImageFilter2.mha
            ${TEMP}/LaplacianRecursiveGaussianImageFilter2.mha
  LaplacianRecursiveGaussianImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/LaplacianRecursiveGaussianImageFilter2.mha
        3
)
add_test( ResampleImageFilter6Test ${FILTERING_EXAMPLES4}
  --compare ${BASELINE}/ResampleImageFilter6PixelCentered.png
            ${TEMP}/ResampleImageFilter6PixelCentered.png
  ResampleImageFilter6Test
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanHeadSlice.png
        ${TEMP}/ResampleImageFilter6PixelCentered.png
        3
)
add_test( ResampleImageFilter7Test ${FILTERING_EXAMPLES4}
  --compare ${BASELINE}/ResampleImageFilter7PixelCentered.png
            ${TEMP}/ResampleImageFilter7PixelCentered.png
  --compareNumberOfPixelsTolerance 1
  ResampleImageFilter7Test
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanHeadSlice.png
        ${TEMP}/ResampleImageFilter7PixelCentered.png
        33
)
add_test( ResampleVolumesToBeIsotropicTest ${FILTERING_EXAMPLES4}
  --compare ${BASELINE}/ResampleVolumesToBeIsotropic.mha
            ${TEMP}/ResampleVolumesToBeIsotropic.mha
  ResampleVolumesToBeIsotropicTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/mri3D.mhd
        ${TEMP}/ResampleVolumesToBeIsotropic.mha
        0 255
)
add_test( ScaleSpaceGenerator2DTest ${FILTERING_EXAMPLES4}
  --compare ${BASELINE}/ScaleSpaceGenerator2D005.mhd
            ${TEMP}/ScaleSpaceGenerator2D005.mhd
  ScaleSpaceGenerator2DTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
        ${TEMP}/ScaleSpaceGenerator2D
        10
)
add_test( SubsampleVolumeTest ${FILTERING_EXAMPLES4}
  --compare ${BASELINE}/SubsampleVolume.mha
            ${TEMP}/SubsampleVolume.mha
  SubsampleVolumeTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/HeadMRVolume.mhd
        ${TEMP}/SubsampleVolume.mha
        2 2 2
)
add_test( SpatialObjectToImage1 ${FILTERING_EXAMPLES9}
  SpatialObjectToImage1
        ${TEMP}/SpatialObjectToImage1.mha
)
add_test( SpatialObjectToImage2 ${FILTERING_EXAMPLES9}
  SpatialObjectToImage2
        ${TEMP}/SpatialObjectToImage2.mha
)
add_test( SpatialObjectToImage3 ${FILTERING_EXAMPLES9}
  SpatialObjectToImage3
        ${TEMP}/SpatialObjectToImage3.mha
)
# The test tests 3 filters.
#   DiffusionTensor3DReconstructionImageFilter
#   TensorFractionalAnisotropyImageFilter
#   TensorRelativeAnisotropyImageFilter
#
add_test( DiffusionTensor3DReconstructionImageFilterTest1 ${FILTERING_EXAMPLES5}
  --compare ${BASELINE}/FACorpusCallosum.mhd
            ${TEMP}/FACorpusCallosum.mhd
  DiffusionTensor3DReconstructionImageFilterTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/DwiCorpusCallosum.nhdr
        80 ${TEMP}/Tensors.mhd ${TEMP}/FACorpusCallosum.mhd
        ${TEMP}/RACorpusCallosum.mhd 0
)
add_test( DiffusionTensor3DReconstructionImageFilterTest2 ${FILTERING_EXAMPLES5}
  --compare ${BASELINE}/RACorpusCallosum.mhd
            ${TEMP}/RACorpusCallosum.mhd
  DiffusionTensor3DReconstructionImageFilterTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/DwiCorpusCallosum.nhdr
        80 ${TEMP}/Tensors.mhd ${TEMP}/FACorpusCallosum.mhd
        ${TEMP}/RACorpusCallosum.mhd 0
)
add_executable(FilteringExamples FilteringExamples.cxx)
target_link_libraries(FilteringExamples ITKIO ITKBasicFilters )
add_executable(FilteringExamples2 FilteringExamples2.cxx)
target_link_libraries(FilteringExamples2 ITKIO ITKBasicFilters )
add_executable(FilteringExamples3 FilteringExamples3.cxx)
target_link_libraries(FilteringExamples3 ITKIO ITKBasicFilters )
add_executable(FilteringExamples4 FilteringExamples4.cxx)
target_link_libraries(FilteringExamples4 ITKIO ITKBasicFilters )
add_executable(FilteringExamples5 FilteringExamples5.cxx)
target_link_libraries(FilteringExamples5 ITKIO ITKBasicFilters
                      ITKStatistics)
add_executable(FilteringExamples7 FilteringExamples7.cxx)
target_link_libraries(FilteringExamples7 ITKIO ITKBasicFilters )
add_executable(FilteringExamples8 FilteringExamples8.cxx)
target_link_libraries(FilteringExamples8 ITKIO ITKBasicFilters )
add_executable(FilteringExamples9 FilteringExamples9.cxx)
target_link_libraries(FilteringExamples9 ITKIO ITKBasicFilters )
add_executable(FilteringExamples6 FilteringExamples6.cxx)
if( USE_FFTWF )
  target_link_libraries(FilteringExamples6 ITKIO ITKBasicFilters  ITKAlgorithms)
else( USE_FFTWF )
  target_link_libraries(FilteringExamples6 ITKIO ITKBasicFilters )
endif( USE_FFTWF )
endif(BUILD_TESTING)
endif( NOT ITK_DISABLE_CXX_TESTING )
add_executable(DiffusionTensor3DReconstructionImageFilter DiffusionTensor3DReconstructionImageFilter.cxx)
target_link_libraries(DiffusionTensor3DReconstructionImageFilter ITKIO ITKBasicFilters )
add_executable(FFTDirectInverse FFTDirectInverse.cxx )
target_link_libraries(FFTDirectInverse  ITKIO)
add_executable(ZeroCrossingBasedEdgeDetectionImageFilter ZeroCrossingBasedEdgeDetectionImageFilter.cxx )
target_link_libraries(ZeroCrossingBasedEdgeDetectionImageFilter  ITKBasicFilters ITKIO)
add_executable(WarpImageFilter1 WarpImageFilter1.cxx )
target_link_libraries(WarpImageFilter1  ITKBasicFilters ITKIO)
add_executable(SurfaceExtraction SurfaceExtraction.cxx )
target_link_libraries(SurfaceExtraction  ITKBasicFilters ITKIO)
add_executable(DigitallyReconstructedRadiograph1 DigitallyReconstructedRadiograph1.cxx )
target_link_libraries(DigitallyReconstructedRadiograph1  ITKIO)
add_executable(ResampleImageFilter7 ResampleImageFilter7.cxx )
target_link_libraries(ResampleImageFilter7  ITKBasicFilters ITKIO)
