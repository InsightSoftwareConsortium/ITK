PROJECT(FilteringExamples)

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

ADD_TEST(BilateralImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/BilateralImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/BilateralImageFilter.png
  BilateralImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/BilateralImageFilter.png
        6.0 5.0
)
ADD_TEST( BinaryMinMaxCurvatureFlowImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/BinaryMinMaxCurvatureFlowImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/BinaryMinMaxCurvatureFlowImageFilter.png
  BinaryMinMaxCurvatureFlowImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/BinaryMinMaxCurvatureFlowImageFilter.png
        10 .125 1 128
)
ADD_TEST( BinaryThresholdImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/BinaryThresholdImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/BinaryThresholdImageFilter.png
  BinaryThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/BinaryThresholdImageFilter.png
        150 180 0 255
)
ADD_TEST( BinomialBlurImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/BinomialBlurImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/BinomialBlurImageFilter.png
  BinomialBlurImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/BinomialBlurImageFilter.png
        5
)
ADD_TEST( CurvatureAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/CurvatureAnisotropicDiffusionImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/CurvatureAnisotropicDiffusionImageFilter.png
  CurvatureAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/CurvatureAnisotropicDiffusionImageFilter.png
        5 0.125 3.0
)
ADD_TEST( CurvatureFlowImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/CurvatureFlowImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/CurvatureFlowImageFilter.png
  CurvatureFlowImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/CurvatureFlowImageFilter.png
        5 0.125 3.0
)
ADD_TEST( DanielssonDistanceMapImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/DanielssonDistanceMapImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/DanielssonDistanceMapImageFilter.png
  DanielssonDistanceMapImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/FivePoints.png
        ${ITK_BINARY_DIR}/Testing/Temporary/DanielssonDistanceMapImageFilter.png
        ${ITK_BINARY_DIR}/Testing/Temporary/DanielssonDistanceMapImageFilter2.png
        ${ITK_BINARY_DIR}/Testing/Temporary/DanielssonDistanceMapImageFilter.mha
)
ADD_TEST( DiscreteGaussianImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/DiscreteGaussianImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/DiscreteGaussianImageFilter.png
  DiscreteGaussianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/DiscreteGaussianImageFilter.png
        1.73 10
)
ADD_TEST( GradientAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/GradientAnisotropicDiffusionImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/GradientAnisotropicDiffusionImageFilter.png
  GradientAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/GradientAnisotropicDiffusionImageFilter.png
        5 0.125 3.0
)
ADD_TEST( GradientMagnitudeImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/GradientMagnitudeImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/GradientMagnitudeImageFilter.png
  GradientMagnitudeImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/GradientMagnitudeImageFilter.png
)
ADD_TEST( GradientMagnitudeRecursiveGaussianImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/GradientMagnitudeRecursiveGaussianImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/GradientMagnitudeRecursiveGaussianImageFilter.png
  GradientMagnitudeRecursiveGaussianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/GradientMagnitudeRecursiveGaussianImageFilter.png
        5
)
ADD_TEST( DerivativeImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/DerivativeImageFilter.mha
            ${ITK_BINARY_DIR}/Testing/Temporary/DerivativeImageFilter.mha
  DerivativeImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/DerivativeImageFilter.mha
        1 1
)
ADD_TEST( LaplacianImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/LaplacianImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/LaplacianImageFilter.png
  LaplacianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/LaplacianImageFilter.png
)
ADD_TEST( MathematicalMorphologyBinaryFiltersTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/MathematicalMorphologyBinaryFiltersErode.png
            ${ITK_BINARY_DIR}/Testing/Temporary/MathematicalMorphologyBinaryFiltersErode.png
  MathematicalMorphologyBinaryFiltersTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/MathematicalMorphologyBinaryFiltersErode.png
        ${ITK_BINARY_DIR}/Testing/Temporary/MathematicalMorphologyBinaryFiltersDilate.png
        150 180
)
ADD_TEST( MathematicalMorphologyGrayscaleFiltersTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/MathematicalMorphologyGrayscaleFiltersErode.png
            ${ITK_BINARY_DIR}/Testing/Temporary/MathematicalMorphologyGrayscaleFiltersErode.png
  MathematicalMorphologyGrayscaleFiltersTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/MathematicalMorphologyGrayscaleFiltersErode.png
        ${ITK_BINARY_DIR}/Testing/Temporary/MathematicalMorphologyGrayscaleFiltersDilate.png
)
ADD_TEST( MeanImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/MeanImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/MeanImageFilter.png
  MeanImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/MeanImageFilter.png
)
ADD_TEST( MedianImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/MedianImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/MedianImageFilter.png
  MedianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/MedianImageFilter.png
        1
)
ADD_TEST( SigmoidImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/SigmoidImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/SigmoidImageFilter.png
  SigmoidImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/SigmoidImageFilter.png
        10 240 10 170
)

ADD_TEST( SmoothingRecursiveGaussianImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/SmoothingRecursiveGaussianImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/SmoothingRecursiveGaussianImageFilter.png
  SmoothingRecursiveGaussianImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/SmoothingRecursiveGaussianImageFilter.png
        3
)

ADD_TEST( ThresholdAboveImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/ThresholdAboveImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/ThresholdAboveImageFilter.png
  ThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/ThresholdBelowImageFilter.png
        ${ITK_BINARY_DIR}/Testing/Temporary/ThresholdAboveImageFilter.png
        ${ITK_BINARY_DIR}/Testing/Temporary/ThresholdOutsideImageFilter.png
)

ADD_TEST( ThresholdBelowImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/ThresholdBelowImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/ThresholdBelowImageFilter.png
  ThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/ThresholdBelowImageFilter.png
        ${ITK_BINARY_DIR}/Testing/Temporary/ThresholdAboveImageFilter.png
        ${ITK_BINARY_DIR}/Testing/Temporary/ThresholdOutsideImageFilter.png
)

ADD_TEST( ThresholdOutsideImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/ThresholdOutsideImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/ThresholdOutsideImageFilter.png
  ThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/ThresholdBelowImageFilter.png
        ${ITK_BINARY_DIR}/Testing/Temporary/ThresholdAboveImageFilter.png
        ${ITK_BINARY_DIR}/Testing/Temporary/ThresholdOutsideImageFilter.png
)

ADD_TEST( VectorGradientAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/VectorGradientAnisotropicDiffusionImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/VectorGradientAnisotropicDiffusionImageFilter.png
  VectorGradientAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/VectorGradientAnisotropicDiffusionImageFilterOriginal.png
        ${ITK_BINARY_DIR}/Testing/Temporary/VectorGradientAnisotropicDiffusionImageFilter.png
        .25 5
)

ADD_TEST( VectorCurvatureAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/VectorCurvatureAnisotropicDiffusionImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/VectorCurvatureAnisotropicDiffusionImageFilter.png
  VectorCurvatureAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/VectorCurvatureAnisotropicDiffusionImageFilterOriginal.png
        ${ITK_BINARY_DIR}/Testing/Temporary/VectorCurvatureAnisotropicDiffusionImageFilter.png
        0.125 5
)

ADD_TEST( RGBGradientAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/RGBGradientAnisotropicDiffusionImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/RGBGradientAnisotropicDiffusionImageFilter.png
  RGBGradientAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/RGBGradientAnisotropicDiffusionImageFilter.png
        0.125 20
)

ADD_TEST( RGBCurvatureAnisotropicDiffusionImageFilterTest ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/RGBCurvatureAnisotropicDiffusionImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/RGBCurvatureAnisotropicDiffusionImageFilter.png
  RGBCurvatureAnisotropicDiffusionImageFilterTest
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/RGBCurvatureAnisotropicDiffusionImageFilter.png
        0.125 20
)

ADD_EXECUTABLE(FilteringExamples FilteringExamples.cxx ${FilteringExamples_SRCS})
TARGET_LINK_LIBRARIES(FilteringExamples ITKIO ITKBasicFilters ITKCommon)
