PROJECT(FilteringExamples)
INCLUDE_REGULAR_EXPRESSION("^.*$")

ADD_EXECUTABLE(AntiAliasBinaryImageFilter AntiAliasBinaryImageFilter.cxx )
TARGET_LINK_LIBRARIES(AntiAliasBinaryImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(FlipImageFilter FlipImageFilter.cxx )
TARGET_LINK_LIBRARIES(FlipImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(FFTImageFilter FFTImageFilter.cxx )
TARGET_LINK_LIBRARIES(FFTImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(FFTImageFilterFourierDomainFiltering FFTImageFilterFourierDomainFiltering.cxx )
TARGET_LINK_LIBRARIES(FFTImageFilterFourierDomainFiltering ITKCommon ITKIO)

ADD_EXECUTABLE(GradientMagnitudeImageFilter GradientMagnitudeImageFilter.cxx )
TARGET_LINK_LIBRARIES(GradientMagnitudeImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(GradientMagnitudeRecursiveGaussianImageFilter GradientMagnitudeRecursiveGaussianImageFilter.cxx )
TARGET_LINK_LIBRARIES(GradientMagnitudeRecursiveGaussianImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(GradientRecursiveGaussianImageFilter GradientRecursiveGaussianImageFilter.cxx )
TARGET_LINK_LIBRARIES(GradientRecursiveGaussianImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(SecondDerivativeRecursiveGaussianImageFilter SecondDerivativeRecursiveGaussianImageFilter.cxx )
TARGET_LINK_LIBRARIES(SecondDerivativeRecursiveGaussianImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(BinaryThresholdImageFilter BinaryThresholdImageFilter.cxx )
TARGET_LINK_LIBRARIES(BinaryThresholdImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(BinaryMedianImageFilter BinaryMedianImageFilter.cxx )
TARGET_LINK_LIBRARIES(BinaryMedianImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(VotingBinaryHoleFillingImageFilter VotingBinaryHoleFillingImageFilter.cxx )
TARGET_LINK_LIBRARIES(VotingBinaryHoleFillingImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(VotingBinaryIterativeHoleFillingImageFilter VotingBinaryIterativeHoleFillingImageFilter.cxx )
TARGET_LINK_LIBRARIES(VotingBinaryIterativeHoleFillingImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(OtsuThresholdImageFilter OtsuThresholdImageFilter.cxx )
TARGET_LINK_LIBRARIES(OtsuThresholdImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(OtsuMultipleThresholdImageFilter OtsuMultipleThresholdImageFilter.cxx )
TARGET_LINK_LIBRARIES(OtsuMultipleThresholdImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(ThresholdImageFilter ThresholdImageFilter.cxx )
TARGET_LINK_LIBRARIES(ThresholdImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(CastingImageFilters CastingImageFilters.cxx )
TARGET_LINK_LIBRARIES(CastingImageFilters ITKCommon ITKIO)

ADD_EXECUTABLE(CannyEdgeDetectionImageFilter CannyEdgeDetectionImageFilter.cxx )
TARGET_LINK_LIBRARIES(CannyEdgeDetectionImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(DigitallyReconstructedRadiograph1 DigitallyReconstructedRadiograph1.cxx )
TARGET_LINK_LIBRARIES(DigitallyReconstructedRadiograph1 ITKCommon ITKIO)

ADD_EXECUTABLE(DerivativeImageFilter DerivativeImageFilter.cxx )
TARGET_LINK_LIBRARIES(DerivativeImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(SigmoidImageFilter SigmoidImageFilter.cxx )
TARGET_LINK_LIBRARIES(SigmoidImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(MedianImageFilter MedianImageFilter.cxx )
TARGET_LINK_LIBRARIES(MedianImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(MeanImageFilter MeanImageFilter.cxx )
TARGET_LINK_LIBRARIES(MeanImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(MathematicalMorphologyBinaryFilters MathematicalMorphologyBinaryFilters.cxx )
TARGET_LINK_LIBRARIES(MathematicalMorphologyBinaryFilters ITKCommon ITKIO)

ADD_EXECUTABLE(MathematicalMorphologyGrayscaleFilters MathematicalMorphologyGrayscaleFilters.cxx )
TARGET_LINK_LIBRARIES(MathematicalMorphologyGrayscaleFilters ITKCommon ITKIO)

ADD_EXECUTABLE(MorphologicalImageEnhancement MorphologicalImageEnhancement.cxx )
TARGET_LINK_LIBRARIES(MorphologicalImageEnhancement ITKCommon ITKIO)

ADD_EXECUTABLE(CurvatureFlowImageFilter CurvatureFlowImageFilter.cxx )
TARGET_LINK_LIBRARIES(CurvatureFlowImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(MinMaxCurvatureFlowImageFilter MinMaxCurvatureFlowImageFilter.cxx )
TARGET_LINK_LIBRARIES(MinMaxCurvatureFlowImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(BinaryMinMaxCurvatureFlowImageFilter BinaryMinMaxCurvatureFlowImageFilter.cxx )
TARGET_LINK_LIBRARIES(BinaryMinMaxCurvatureFlowImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(DiscreteGaussianImageFilter DiscreteGaussianImageFilter.cxx )
TARGET_LINK_LIBRARIES(DiscreteGaussianImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(GaussianBlurImageFunction GaussianBlurImageFunction.cxx )
TARGET_LINK_LIBRARIES(GaussianBlurImageFunction ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(SmoothingRecursiveGaussianImageFilter SmoothingRecursiveGaussianImageFilter.cxx )
TARGET_LINK_LIBRARIES(SmoothingRecursiveGaussianImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(SmoothingRecursiveGaussianImageFilter2 SmoothingRecursiveGaussianImageFilter2.cxx )
TARGET_LINK_LIBRARIES(SmoothingRecursiveGaussianImageFilter2 ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(LaplacianRecursiveGaussianImageFilter1 LaplacianRecursiveGaussianImageFilter1.cxx )
TARGET_LINK_LIBRARIES(LaplacianRecursiveGaussianImageFilter1 ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(LaplacianRecursiveGaussianImageFilter2 LaplacianRecursiveGaussianImageFilter2.cxx )
TARGET_LINK_LIBRARIES(LaplacianRecursiveGaussianImageFilter2 ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(ScaleSpaceGenerator2D ScaleSpaceGenerator2D.cxx )
TARGET_LINK_LIBRARIES(ScaleSpaceGenerator2D ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(BinomialBlurImageFilter BinomialBlurImageFilter.cxx )
TARGET_LINK_LIBRARIES(BinomialBlurImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(CurvatureAnisotropicDiffusionImageFilter CurvatureAnisotropicDiffusionImageFilter.cxx )
TARGET_LINK_LIBRARIES(CurvatureAnisotropicDiffusionImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(GradientAnisotropicDiffusionImageFilter GradientAnisotropicDiffusionImageFilter.cxx )
TARGET_LINK_LIBRARIES(GradientAnisotropicDiffusionImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(VectorGradientAnisotropicDiffusionImageFilter VectorGradientAnisotropicDiffusionImageFilter.cxx )
TARGET_LINK_LIBRARIES(VectorGradientAnisotropicDiffusionImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(VectorCurvatureAnisotropicDiffusionImageFilter VectorCurvatureAnisotropicDiffusionImageFilter.cxx )
TARGET_LINK_LIBRARIES(VectorCurvatureAnisotropicDiffusionImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(RGBCurvatureAnisotropicDiffusionImageFilter RGBCurvatureAnisotropicDiffusionImageFilter.cxx )
TARGET_LINK_LIBRARIES(RGBCurvatureAnisotropicDiffusionImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(RGBGradientAnisotropicDiffusionImageFilter RGBGradientAnisotropicDiffusionImageFilter.cxx )
TARGET_LINK_LIBRARIES(RGBGradientAnisotropicDiffusionImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(RGBToGrayscale RGBToGrayscale.cxx )
TARGET_LINK_LIBRARIES(RGBToGrayscale ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(DanielssonDistanceMapImageFilter DanielssonDistanceMapImageFilter.cxx )
TARGET_LINK_LIBRARIES(DanielssonDistanceMapImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(BilateralImageFilter BilateralImageFilter.cxx )
TARGET_LINK_LIBRARIES(BilateralImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(ResampleImageFilter ResampleImageFilter.cxx )
TARGET_LINK_LIBRARIES(ResampleImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(ResampleImageFilter2 ResampleImageFilter2.cxx )
TARGET_LINK_LIBRARIES(ResampleImageFilter2 ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(ResampleImageFilter3 ResampleImageFilter3.cxx )
TARGET_LINK_LIBRARIES(ResampleImageFilter3 ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(ResampleImageFilter4 ResampleImageFilter4.cxx )
TARGET_LINK_LIBRARIES(ResampleImageFilter4 ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(ResampleImageFilter5 ResampleImageFilter5.cxx )
TARGET_LINK_LIBRARIES(ResampleImageFilter5 ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(ResampleImageFilter6 ResampleImageFilter6.cxx )
TARGET_LINK_LIBRARIES(ResampleImageFilter6 ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(ResampleImageFilter8 ResampleImageFilter8.cxx )
TARGET_LINK_LIBRARIES(ResampleImageFilter8 ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(ResampleImageFilter9 ResampleImageFilter9.cxx )
TARGET_LINK_LIBRARIES(ResampleImageFilter9 ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(ResampleVolumesToBeIsotropic ResampleVolumesToBeIsotropic.cxx )
TARGET_LINK_LIBRARIES(ResampleVolumesToBeIsotropic ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(WarpImageFilter1 WarpImageFilter1.cxx )
TARGET_LINK_LIBRARIES(WarpImageFilter1 ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(SubsampleVolume SubsampleVolume.cxx )
TARGET_LINK_LIBRARIES(SubsampleVolume ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(LaplacianImageFilter LaplacianImageFilter.cxx )
TARGET_LINK_LIBRARIES(LaplacianImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(LaplacianSharpeningImageFilter LaplacianSharpeningImageFilter.cxx )
TARGET_LINK_LIBRARIES(LaplacianSharpeningImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(IsoSurfaceExtraction IsoSurfaceExtraction.cxx )
TARGET_LINK_LIBRARIES(IsoSurfaceExtraction ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(VectorIndexSelection VectorIndexSelection.cxx )
TARGET_LINK_LIBRARIES(VectorIndexSelection ITKCommon ITKBasicFilters ITKIO)

SET(FILTERING_EXAMPLES ${CXX_TEST_PATH}/FilteringExamples)
SET(FILTERING_EXAMPLES2 ${CXX_TEST_PATH}/FilteringExamples2)
SET(FILTERING_EXAMPLES3 ${CXX_TEST_PATH}/FilteringExamples3)
SET(FILTERING_EXAMPLES4 ${CXX_TEST_PATH}/FilteringExamples4)
SET(FILTERING_EXAMPLES5 ${CXX_TEST_PATH}/FilteringExamples5)
SET(FILTERING_EXAMPLES6 ${CXX_TEST_PATH}/FilteringExamples6)

SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Filtering)
SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

IF( NOT ITK_DISABLE_CXX_TESTING )

ADD_TEST(BilateralImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/BilateralImageFilter.png
            ${TEMP}/BilateralImageFilter.png
  BilateralImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BilateralImageFilter.png
        6.0 5.0
)
ADD_TEST( BinaryMinMaxCurvatureFlowImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/BinaryMinMaxCurvatureFlowImageFilter.png
            ${TEMP}/BinaryMinMaxCurvatureFlowImageFilter.png
  BinaryMinMaxCurvatureFlowImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BinaryMinMaxCurvatureFlowImageFilter.png
        10 .125 1 128
)
ADD_TEST( BinaryThresholdImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/BinaryThresholdImageFilter.png
            ${TEMP}/BinaryThresholdImageFilter.png
  BinaryThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BinaryThresholdImageFilter.png
        150 180 0 255
)
ADD_TEST( BinaryMedianImageFilterTest1 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/BinaryMedianImageFilter1.png
            ${TEMP}/BinaryMedianImageFilter1.png
  BinaryMedianImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/BinaryMedianImageFilter1.png
        1 1
)
ADD_TEST( BinaryMedianImageFilterTest2 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/BinaryMedianImageFilter2.png
            ${TEMP}/BinaryMedianImageFilter2.png
  BinaryMedianImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/BinaryMedianImageFilter2.png
        2 2
)
ADD_TEST( BinaryMedianImageFilterTest3 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/BinaryMedianImageFilter3.png
            ${TEMP}/BinaryMedianImageFilter3.png
  BinaryMedianImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/BinaryMedianImageFilter3.png
        3 3
)
ADD_TEST( VotingBinaryHoleFillingImageFilterTest1 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VotingBinaryHoleFillingImageFilter1.png
            ${TEMP}/VotingBinaryHoleFillingImageFilter1.png
  VotingBinaryHoleFillingImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryHoleFillingImageFilter1.png
        1 1
)
ADD_TEST( VotingBinaryHoleFillingImageFilterTest2 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VotingBinaryHoleFillingImageFilter2.png
            ${TEMP}/VotingBinaryHoleFillingImageFilter2.png
  VotingBinaryHoleFillingImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryHoleFillingImageFilter2.png
        2 2
)
ADD_TEST( VotingBinaryHoleFillingImageFilterTest3 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VotingBinaryHoleFillingImageFilter3.png
            ${TEMP}/VotingBinaryHoleFillingImageFilter3.png
  VotingBinaryHoleFillingImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryHoleFillingImageFilter3.png
        3 3
)
ADD_TEST( VotingBinaryIterativeHoleFillingImageFilterTest1 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VotingBinaryIterativeHoleFillingImageFilter1.png
            ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter1.png
  VotingBinaryIterativeHoleFillingImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter1.png
        1 1 10
)
ADD_TEST( VotingBinaryIterativeHoleFillingImageFilterTest2 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VotingBinaryIterativeHoleFillingImageFilter2.png
            ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter2.png
  VotingBinaryIterativeHoleFillingImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter2.png
        2 2 10
)
ADD_TEST( VotingBinaryIterativeHoleFillingImageFilterTest3 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VotingBinaryIterativeHoleFillingImageFilter3.png
            ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter3.png
  VotingBinaryIterativeHoleFillingImageFilterTest
        ${BASELINE}/BinaryThresholdImageFilter.png
        ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter3.png
        3 3 10
)
ADD_TEST( BinomialBlurImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/BinomialBlurImageFilter.png
            ${TEMP}/BinomialBlurImageFilter.png
  BinomialBlurImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BinomialBlurImageFilter.png
        5
)
ADD_TEST( CurvatureAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/CurvatureAnisotropicDiffusionImageFilter.png
            ${TEMP}/CurvatureAnisotropicDiffusionImageFilter.png
  CurvatureAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/CurvatureAnisotropicDiffusionImageFilter.png
        5 0.125 3
)
ADD_TEST( CurvatureFlowImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/CurvatureFlowImageFilter.png
            ${TEMP}/CurvatureFlowImageFilter.png
  CurvatureFlowImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/CurvatureFlowImageFilter.png
        5 0.125 1.0
)
ADD_TEST( DanielssonDistanceMapImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/DanielssonDistanceMapImageFilter.png
            ${TEMP}/DanielssonDistanceMapImageFilter.png
  DanielssonDistanceMapImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/FivePoints.png
        ${TEMP}/DanielssonDistanceMapImageFilter.png
        ${TEMP}/DanielssonDistanceMapImageFilter2.png
        ${TEMP}/DanielssonDistanceMapImageFilter.mha
)
ADD_TEST( DiscreteGaussianImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/DiscreteGaussianImageFilter.png
            ${TEMP}/DiscreteGaussianImageFilter.png
  DiscreteGaussianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/DiscreteGaussianImageFilter.png
        1.73 10
)
ADD_TEST( GradientAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/GradientAnisotropicDiffusionImageFilter.png
            ${TEMP}/GradientAnisotropicDiffusionImageFilter.png
  GradientAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GradientAnisotropicDiffusionImageFilter.png
        5 0.125 1.0
)
ADD_TEST( GradientMagnitudeImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/GradientMagnitudeImageFilter.png
            ${TEMP}/GradientMagnitudeImageFilter.png
  GradientMagnitudeImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GradientMagnitudeImageFilter.png
)
ADD_TEST( GradientMagnitudeRecursiveGaussianImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/GradientMagnitudeRecursiveGaussianImageFilter.png
            ${TEMP}/GradientMagnitudeRecursiveGaussianImageFilter.png
  GradientMagnitudeRecursiveGaussianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GradientMagnitudeRecursiveGaussianImageFilter.png
        5
)
ADD_TEST( DerivativeImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/DerivativeImageFilter.mha
            ${TEMP}/DerivativeImageFilter.mha
  DerivativeImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/DerivativeImageFilter.mha
        ${TEMP}/DerivativeImageFilter.png
        1 1
)
ADD_TEST( LaplacianImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/LaplacianImageFilter.png
            ${TEMP}/LaplacianImageFilter.png
  LaplacianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/LaplacianImageFilter.png
)
ADD_TEST( LaplacianSharpeningImageFilterTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/LaplacianSharpeningImageFilter.png
            ${TEMP}/LaplacianSharpeningImageFilter.png
  LaplacianSharpeningImageFilterTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
        ${TEMP}/LaplacianSharpeningImageFilter.png
)
ADD_TEST( MathematicalMorphologyBinaryFiltersTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/MathematicalMorphologyBinaryFiltersErode.png
            ${TEMP}/MathematicalMorphologyBinaryFiltersErode.png
  MathematicalMorphologyBinaryFiltersTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MathematicalMorphologyBinaryFiltersErode.png
        ${TEMP}/MathematicalMorphologyBinaryFiltersDilate.png
        150 180
)
ADD_TEST( MathematicalMorphologyGrayscaleFiltersTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/MathematicalMorphologyGrayscaleFiltersErode.png
            ${TEMP}/MathematicalMorphologyGrayscaleFiltersErode.png
  MathematicalMorphologyGrayscaleFiltersTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MathematicalMorphologyGrayscaleFiltersErode.png
        ${TEMP}/MathematicalMorphologyGrayscaleFiltersDilate.png
)
ADD_TEST( MeanImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/MeanImageFilter.png
            ${TEMP}/MeanImageFilter.png
  MeanImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MeanImageFilter.png
)
ADD_TEST( MedianImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/MedianImageFilter.png
            ${TEMP}/MedianImageFilter.png
  MedianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MedianImageFilter.png
        1
)

ADD_TEST( ResampleImageFilterTest1 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilterTest1.png
            ${TEMP}/ResampleImageFilterTest1.png
  ResampleImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest1.png
        0
)

ADD_TEST( ResampleImageFilterTest2 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilterTest2.png
            ${TEMP}/ResampleImageFilterTest2.png
  ResampleImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest2.png
        1
)

ADD_TEST( ResampleImageFilterTest3 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilterTest3.png
            ${TEMP}/ResampleImageFilterTest3.png
  ResampleImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest3.png
        2
)

ADD_TEST( ResampleImageFilterTest4 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilterTest4.png
            ${TEMP}/ResampleImageFilterTest4.png
  ResampleImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest4.png
        3
)

ADD_TEST( ResampleImageFilter2Test1 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter2Test1.png
            ${TEMP}/ResampleImageFilter2Test1.png
  ResampleImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test1.png
        0
)

ADD_TEST( ResampleImageFilter2Test2 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter2Test2.png
            ${TEMP}/ResampleImageFilter2Test2.png
  ResampleImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test2.png
        1
)

ADD_TEST( ResampleImageFilter2Test3 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter2Test3.png
            ${TEMP}/ResampleImageFilter2Test3.png
  ResampleImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test3.png
        2
)

ADD_TEST( ResampleImageFilter2Test4 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter2Test4.png
            ${TEMP}/ResampleImageFilter2Test4.png
  ResampleImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test4.png
        3
)

ADD_TEST( ResampleImageFilter2Test5 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter2Test5.png
            ${TEMP}/ResampleImageFilter2Test5.png
  ResampleImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test5.png
        3
)

ADD_TEST( ResampleImageFilter3Test1 ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter3Test1.png
            ${TEMP}/ResampleImageFilter3Test1.png
  ResampleImageFilter3Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter3Test1.png
        0
)

ADD_TEST( ResampleImageFilter4Test ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter4Test.png
            ${TEMP}/ResampleImageFilter4Test.png
  ResampleImageFilter4Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter4Test.png
        20
)

ADD_TEST( ResampleImageFilter5Test ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter5Test.png
            ${TEMP}/ResampleImageFilter5Test.png
  ResampleImageFilter5Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter5Test.png
        10 1.2
)

ADD_TEST( ResampleImageFilter8Test ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter8Test.png
            ${TEMP}/ResampleImageFilter8Test.png
  ResampleImageFilter8Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter8Test.png
        45
)

ADD_TEST( ResampleImageFilter9Test ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ResampleImageFilter9Test.png
            ${TEMP}/ResampleImageFilter9TestNearest.png
  ResampleImageFilter9Test
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/ResampleImageFilter9TestNearest.png
        ${TEMP}/ResampleImageFilter9TestLinear.png
)

ADD_TEST( SigmoidImageFilterTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/SigmoidImageFilter.png
            ${TEMP}/SigmoidImageFilter.png
  SigmoidImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/SigmoidImageFilter.png
        10 240 10 170
)

ADD_TEST( SmoothingRecursiveGaussianImageFilterTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/SmoothingRecursiveGaussianImageFilter.png
            ${TEMP}/SmoothingRecursiveGaussianImageFilter.png
  SmoothingRecursiveGaussianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/SmoothingRecursiveGaussianImageFilter.png
        3
)

ADD_TEST( SmoothingRecursiveGaussianImageFilter2Test ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/SmoothingRecursiveGaussianImageFilter2.png
            ${TEMP}/SmoothingRecursiveGaussianImageFilter2.png
  SmoothingRecursiveGaussianImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/SmoothingRecursiveGaussianImageFilter2.png
        3
)

ADD_TEST( ThresholdAboveImageFilterTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ThresholdAboveImageFilter.png
            ${TEMP}/ThresholdAboveImageFilter.png
  ThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdBelowImageFilter.png
        ${TEMP}/ThresholdAboveImageFilter.png
        ${TEMP}/ThresholdOutsideImageFilter.png
)

ADD_TEST( ThresholdBelowImageFilterTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ThresholdBelowImageFilter.png
            ${TEMP}/ThresholdBelowImageFilter.png
  ThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdBelowImageFilter.png
        ${TEMP}/ThresholdAboveImageFilter.png
        ${TEMP}/ThresholdOutsideImageFilter.png
)

ADD_TEST( ThresholdOutsideImageFilterTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/ThresholdOutsideImageFilter.png
            ${TEMP}/ThresholdOutsideImageFilter.png
  ThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdBelowImageFilter.png
        ${TEMP}/ThresholdAboveImageFilter.png
        ${TEMP}/ThresholdOutsideImageFilter.png
)

ADD_TEST( VectorGradientAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VectorGradientAnisotropicDiffusionImageFilter.png
            ${TEMP}/VectorGradientAnisotropicDiffusionImageFilter.png
  VectorGradientAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/VectorGradientAnisotropicDiffusionImageFilterOriginal.png
        ${TEMP}/VectorGradientAnisotropicDiffusionImageFilter.png
        .25 5
)

ADD_TEST( VectorCurvatureAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VectorCurvatureAnisotropicDiffusionImageFilter.png
            ${TEMP}/VectorCurvatureAnisotropicDiffusionImageFilter.png
  VectorCurvatureAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/VectorCurvatureAnisotropicDiffusionImageFilterOriginal.png
        ${TEMP}/VectorCurvatureAnisotropicDiffusionImageFilter.png
        0.125 5
)

ADD_TEST( VectorIndexSelectionTest ${FILTERING_EXAMPLES2}
  --compare ${BASELINE}/VectorIndexSelection.png
            ${TEMP}/VectorIndexSelection.png
  VectorIndexSelectionTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/VectorIndexSelection.png
        1
)

ADD_TEST( RGBGradientAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/RGBGradientAnisotropicDiffusionImageFilter.png
            ${TEMP}/RGBGradientAnisotropicDiffusionImageFilter.png
  RGBGradientAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/RGBGradientAnisotropicDiffusionImageFilter.png
        0.125 20
)

ADD_TEST( RGBCurvatureAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/RGBCurvatureAnisotropicDiffusionImageFilter.png
            ${TEMP}/RGBCurvatureAnisotropicDiffusionImageFilter.png
  RGBCurvatureAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/RGBCurvatureAnisotropicDiffusionImageFilter.png
        0.125 20
)

ADD_TEST( MorphologicalImageEnhancementTest ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/MorphologicalImageEnhancement.png
            ${TEMP}/MorphologicalImageEnhancement.png
  MorphologicalImageEnhancementTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/peppers.png
        ${TEMP}/MorphologicalImageEnhancement.png
        3
)

ADD_TEST( OtsuThresholdImageFilterTest ${FILTERING_EXAMPLES5}
  --compare ${BASELINE}/OtsuThresholdImageFilter.png
            ${TEMP}/OtsuThresholdImageFilter.png
  OtsuThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/OtsuThresholdImageFilter.png
        255 0
)

ADD_TEST( OtsuMultipleThresholdImageFilterTest ${FILTERING_EXAMPLES5}
  --compare ${BASELINE}/OtsuMultipleThresholdOutput002.png
            ${TEMP}/OtsuMultipleThresholdOutput002.png
  OtsuMultipleThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/OtsuMultipleThresholdOutput
        png 3
)

ADD_TEST( DigitallyReconstructedRadiograph1Test ${FILTERING_EXAMPLES6}
  --compare ${BASELINE}/DigitallyReconstructedRadiograph1.png
            ${TEMP}/DigitallyReconstructedRadiograph1.png
  DigitallyReconstructedRadiograph1Test
        -o ${TEMP}/DigitallyReconstructedRadiograph1.png
)

ADD_TEST( AntiAliasBinaryImageFilterTest ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/AntiAliasBinaryImageFilter.png
            ${TEMP}/AntiAliasBinaryImageFilter.png
  AntiAliasBinaryImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VentricleModel.png
        ${TEMP}/AntiAliasBinaryImageFilter.png
        .001 50
)

ADD_TEST( CannyEdgeDetectionImageFilterTest ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/CannyEdgeDetectionImageFilter.png
            ${TEMP}/CannyEdgeDetectionImageFilter.png
  CannyEdgeDetectionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/CannyEdgeDetectionImageFilter.png
        9
)

ADD_TEST( FlipImageFilterTest1 ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/FlipImageFilterX.png
            ${TEMP}/FlipImageFilterX.png
  FlipImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/FlipImageFilterX.png
        1 0
)

ADD_TEST( FlipImageFilterTest2 ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/FlipImageFilterY.png
            ${TEMP}/FlipImageFilterY.png
  FlipImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/FlipImageFilterY.png
        0 1
)

ADD_TEST( FlipImageFilterTest3 ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/FlipImageFilterXY.png
            ${TEMP}/FlipImageFilterXY.png
  FlipImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/FlipImageFilterXY.png
        1 1
)

ADD_TEST( GaussianBlurImageFunctionTest ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/GaussianBlurImageFunction.mha
            ${TEMP}/GaussianBlurImageFunction.mha
  GaussianBlurImageFunctionTest
        ${ITK_SOURCE_DIR}/Examples/Data/VentricleModel.png
        ${TEMP}/GaussianBlurImageFunction.mha
        2 3
)

ADD_TEST( FFTImageFilterTest ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/FFTImageFilterReal.png
            ${TEMP}/FFTImageFilterReal.png
  FFTImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/FatMRISlice.png
        ${TEMP}/FFTImageFilterReal.png
        ${TEMP}/FFTImageFilterImaginary.png
)

ADD_TEST( RGBToGrayscaleTest ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/RGBToGrayscale.png
            ${TEMP}/RGBToGrayscale.png
  RGBToGrayscaleTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanHeadSlice.png
        ${TEMP}/RGBToGrayscale.png
)

ADD_TEST( LaplacianRecursiveGaussianImageFilter1Test ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/LaplacianRecursiveGaussianImageFilter1.mha
            ${TEMP}/LaplacianRecursiveGaussianImageFilter1.mha
  LaplacianRecursiveGaussianImageFilter1Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/LaplacianRecursiveGaussianImageFilter1.mha
        3
)

ADD_TEST( LaplacianRecursiveGaussianImageFilter2Test ${FILTERING_EXAMPLES3}
  --compare ${BASELINE}/LaplacianRecursiveGaussianImageFilter2.mha
            ${TEMP}/LaplacianRecursiveGaussianImageFilter2.mha
  LaplacianRecursiveGaussianImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/LaplacianRecursiveGaussianImageFilter2.mha
        3
)

ADD_TEST( ResampleImageFilter6Test ${FILTERING_EXAMPLES4}
  --compare ${BASELINE}/ResampleImageFilter6.png
            ${TEMP}/ResampleImageFilter6.png
  ResampleImageFilter6Test
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanHeadSlice.png
        ${TEMP}/ResampleImageFilter6.png
        3
)

ADD_TEST( ResampleImageFilter7Test ${FILTERING_EXAMPLES4}
  --compare ${BASELINE}/ResampleImageFilter7.png
            ${TEMP}/ResampleImageFilter7.png
  ResampleImageFilter7Test
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanHeadSlice.png
        ${TEMP}/ResampleImageFilter7.png
        33
)

ADD_TEST( ResampleVolumesToBeIsotropicTest ${FILTERING_EXAMPLES4}
  --compare ${BASELINE}/ResampleVolumesToBeIsotropic.mha
            ${TEMP}/ResampleVolumesToBeIsotropic.mha
  ResampleVolumesToBeIsotropicTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/mri3D.mhd
        ${TEMP}/ResampleVolumesToBeIsotropic.mha
        0 255
)

ADD_TEST( ScaleSpaceGenerator2DTest ${FILTERING_EXAMPLES4}
  --compare ${BASELINE}/ScaleSpaceGenerator2D005.mhd
            ${TEMP}/ScaleSpaceGenerator2D005.mhd
  ScaleSpaceGenerator2DTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png
        ${TEMP}/ScaleSpaceGenerator2D
        10
)

ADD_TEST( SubsampleVolumeTest ${FILTERING_EXAMPLES4}
  --compare ${BASELINE}/SubsampleVolume.mha
            ${TEMP}/SubsampleVolume.mha
  SubsampleVolumeTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/HeadMRVolume.mhd
        ${TEMP}/SubsampleVolume.mha
        2 2 2
)

ADD_EXECUTABLE(FilteringExamples FilteringExamples.cxx)
TARGET_LINK_LIBRARIES(FilteringExamples ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(FilteringExamples2 FilteringExamples2.cxx)
TARGET_LINK_LIBRARIES(FilteringExamples2 ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(FilteringExamples3 FilteringExamples3.cxx)
TARGET_LINK_LIBRARIES(FilteringExamples3 ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(FilteringExamples4 FilteringExamples4.cxx)
TARGET_LINK_LIBRARIES(FilteringExamples4 ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(FilteringExamples5 FilteringExamples5.cxx)
TARGET_LINK_LIBRARIES(FilteringExamples5 ITKIO ITKBasicFilters ITKCommon)

ADD_EXECUTABLE(FilteringExamples6 FilteringExamples6.cxx)
TARGET_LINK_LIBRARIES(FilteringExamples6 ITKIO ITKBasicFilters ITKCommon)

IF(NOT BORLAND)
  ADD_EXECUTABLE(ResampleImageFilter7 ResampleImageFilter7.cxx )
  TARGET_LINK_LIBRARIES(ResampleImageFilter7 ITKCommon ITKBasicFilters ITKIO)
ENDIF(NOT BORLAND)

ENDIF( NOT ITK_DISABLE_CXX_TESTING )

