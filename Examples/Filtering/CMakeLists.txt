PROJECT(FilteringExamples)
INCLUDE_REGULAR_EXPRESSION("^.*$")

ADD_EXECUTABLE(GradientMagnitudeImageFilter GradientMagnitudeImageFilter.cxx )
TARGET_LINK_LIBRARIES(GradientMagnitudeImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(GradientMagnitudeRecursiveGaussianImageFilter GradientMagnitudeRecursiveGaussianImageFilter.cxx )
TARGET_LINK_LIBRARIES(GradientMagnitudeRecursiveGaussianImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(BinaryThresholdImageFilter BinaryThresholdImageFilter.cxx )
TARGET_LINK_LIBRARIES(BinaryThresholdImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(ThresholdImageFilter ThresholdImageFilter.cxx )
TARGET_LINK_LIBRARIES(ThresholdImageFilter ITKCommon ITKIO)

ADD_EXECUTABLE(CastingImageFilters CastingImageFilters.cxx )
TARGET_LINK_LIBRARIES(CastingImageFilters ITKCommon ITKIO)

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

ADD_EXECUTABLE(CurvatureFlowImageFilter CurvatureFlowImageFilter.cxx )
TARGET_LINK_LIBRARIES(CurvatureFlowImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(MinMaxCurvatureFlowImageFilter MinMaxCurvatureFlowImageFilter.cxx )
TARGET_LINK_LIBRARIES(MinMaxCurvatureFlowImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(BinaryMinMaxCurvatureFlowImageFilter BinaryMinMaxCurvatureFlowImageFilter.cxx )
TARGET_LINK_LIBRARIES(BinaryMinMaxCurvatureFlowImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(DiscreteGaussianImageFilter DiscreteGaussianImageFilter.cxx )
TARGET_LINK_LIBRARIES(DiscreteGaussianImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(SmoothingRecursiveGaussianImageFilter SmoothingRecursiveGaussianImageFilter.cxx )
TARGET_LINK_LIBRARIES(SmoothingRecursiveGaussianImageFilter ITKCommon ITKBasicFilters ITKIO)

ADD_EXECUTABLE(SmoothingRecursiveGaussianImageFilter2 SmoothingRecursiveGaussianImageFilter2.cxx )
TARGET_LINK_LIBRARIES(SmoothingRecursiveGaussianImageFilter2 ITKCommon ITKBasicFilters ITKIO)

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

ADD_EXECUTABLE(LaplacianImageFilter LaplacianImageFilter.cxx )
TARGET_LINK_LIBRARIES(LaplacianImageFilter ITKCommon ITKBasicFilters ITKIO)

SET(FILTERING_EXAMPLES ${CXX_TEST_PATH}/FilteringExamples)

SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Filtering)
SET(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

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
        5 0.125 3.0
)
ADD_TEST( CurvatureFlowImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/CurvatureFlowImageFilter.png
            ${TEMP}/CurvatureFlowImageFilter.png
  CurvatureFlowImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/CurvatureFlowImageFilter.png
        5 0.125 3.0
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
        5 0.125 3.0
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

ADD_TEST( ResampleImageFilterTest1 ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ResampleImageFilterTest1.png
            ${TEMP}/ResampleImageFilterTest1.png
  ResampleImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest1.png
        0
)

ADD_TEST( ResampleImageFilterTest2 ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ResampleImageFilterTest2.png
            ${TEMP}/ResampleImageFilterTest2.png
  ResampleImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest2.png
        1
)

ADD_TEST( ResampleImageFilterTest3 ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ResampleImageFilterTest3.png
            ${TEMP}/ResampleImageFilterTest3.png
  ResampleImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest3.png
        2
)

ADD_TEST( ResampleImageFilterTest4 ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ResampleImageFilterTest4.png
            ${TEMP}/ResampleImageFilterTest4.png
  ResampleImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest4.png
        3
)

ADD_TEST( ResampleImageFilter2Test1 ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ResampleImageFilter2Test1.png
            ${TEMP}/ResampleImageFilter2Test1.png
  ResampleImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test1.png
        0
)

ADD_TEST( ResampleImageFilter2Test2 ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ResampleImageFilter2Test2.png
            ${TEMP}/ResampleImageFilter2Test2.png
  ResampleImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test2.png
        1
)

ADD_TEST( ResampleImageFilter2Test3 ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ResampleImageFilter2Test3.png
            ${TEMP}/ResampleImageFilter2Test3.png
  ResampleImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test3.png
        2
)

ADD_TEST( ResampleImageFilter2Test4 ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ResampleImageFilter2Test4.png
            ${TEMP}/ResampleImageFilter2Test4.png
  ResampleImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test4.png
        3
)

ADD_TEST( ResampleImageFilter2Test5 ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ResampleImageFilter2Test5.png
            ${TEMP}/ResampleImageFilter2Test5.png
  ResampleImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test5.png
        3
)

ADD_TEST( ResampleImageFilter3Test1 ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ResampleImageFilter3Test1.png
            ${TEMP}/ResampleImageFilter3Test1.png
  ResampleImageFilter3Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter3Test1.png
        0
)

ADD_TEST( ResampleImageFilter3Test2 ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ResampleImageFilter3Test2.png
            ${TEMP}/ResampleImageFilter3Test2.png
  ResampleImageFilter3Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter3Test2.png
        1
)

ADD_TEST( ResampleImageFilter4Test ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ResampleImageFilter4Test.png
            ${TEMP}/ResampleImageFilter4Test.png
  ResampleImageFilter4Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter4Test.png
        20
)

ADD_TEST( ResampleImageFilter5Test ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ResampleImageFilter5Test.png
            ${TEMP}/ResampleImageFilter5Test.png
  ResampleImageFilter5Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter5Test.png
        10 1.2
)

ADD_TEST( SigmoidImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/SigmoidImageFilter.png
            ${TEMP}/SigmoidImageFilter.png
  SigmoidImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/SigmoidImageFilter.png
        10 240 10 170
)

ADD_TEST( SmoothingRecursiveGaussianImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/SmoothingRecursiveGaussianImageFilter.png
            ${TEMP}/SmoothingRecursiveGaussianImageFilter.png
  SmoothingRecursiveGaussianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/SmoothingRecursiveGaussianImageFilter.png
        3
)

ADD_TEST( SmoothingRecursiveGaussianImageFilter2Test ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/SmoothingRecursiveGaussianImageFilter2.png
            ${TEMP}/SmoothingRecursiveGaussianImageFilter2.png
  SmoothingRecursiveGaussianImageFilter2Test
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/SmoothingRecursiveGaussianImageFilter2.png
        3
)

ADD_TEST( ThresholdAboveImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ThresholdAboveImageFilter.png
            ${TEMP}/ThresholdAboveImageFilter.png
  ThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdBelowImageFilter.png
        ${TEMP}/ThresholdAboveImageFilter.png
        ${TEMP}/ThresholdOutsideImageFilter.png
)

ADD_TEST( ThresholdBelowImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ThresholdBelowImageFilter.png
            ${TEMP}/ThresholdBelowImageFilter.png
  ThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdBelowImageFilter.png
        ${TEMP}/ThresholdAboveImageFilter.png
        ${TEMP}/ThresholdOutsideImageFilter.png
)

ADD_TEST( ThresholdOutsideImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/ThresholdOutsideImageFilter.png
            ${TEMP}/ThresholdOutsideImageFilter.png
  ThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdBelowImageFilter.png
        ${TEMP}/ThresholdAboveImageFilter.png
        ${TEMP}/ThresholdOutsideImageFilter.png
)

ADD_TEST( VectorGradientAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/VectorGradientAnisotropicDiffusionImageFilter.png
            ${TEMP}/VectorGradientAnisotropicDiffusionImageFilter.png
  VectorGradientAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/VectorGradientAnisotropicDiffusionImageFilterOriginal.png
        ${TEMP}/VectorGradientAnisotropicDiffusionImageFilter.png
        .25 5
)

ADD_TEST( VectorCurvatureAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${BASELINE}/VectorCurvatureAnisotropicDiffusionImageFilter.png
            ${TEMP}/VectorCurvatureAnisotropicDiffusionImageFilter.png
  VectorCurvatureAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/VectorCurvatureAnisotropicDiffusionImageFilterOriginal.png
        ${TEMP}/VectorCurvatureAnisotropicDiffusionImageFilter.png
        0.125 5
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

ADD_EXECUTABLE(FilteringExamples FilteringExamples.cxx ${FilteringExamples_SRCS})
TARGET_LINK_LIBRARIES(FilteringExamples ITKIO ITKBasicFilters ITKCommon)
