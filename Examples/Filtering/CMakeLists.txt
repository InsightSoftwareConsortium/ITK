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

ADD_TEST(BilateralImageFilter ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/BilateralImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/BilateralImageFilter.png
  BilateralImageFilter
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/BilateralImageFilter.png
        6.0 5.0
)
ADD_TEST( BinaryMinMaxCurvatureFlowImageFilter ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/BinaryMinMaxCurvatureFlowImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/BinaryMinMaxCurvatureFlowImageFilter.png
  BinaryMinMaxCurvatureFlowImageFilter
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/BinaryMinMaxCurvatureFlowImageFilter.png
        10 .25 1 128
)
ADD_TEST( BinaryThresholdImageFilter ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/BinaryThresholdImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/BinaryThresholdImageFilter.png
  BinaryThresholdImageFilter
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/BinaryThresholdImageFilter.png
        150 180 0 255
)
ADD_TEST( BinomialBlurImageFilter ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/BinomialBlurImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/BinomialBlurImageFilter.png
  BinomialBlurImageFilter
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/BinomialBlurImageFilter.png
        5
)
ADD_TEST( CurvatureAnisotropicDiffusionImageFilter ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/CurvatureAnisotropicDiffusionImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/CurvatureAnisotropicDiffusionImageFilter.png
  CurvatureAnisotropicDiffusionImageFilter
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/CurvatureAnisotropicDiffusionImageFilter.png
        5 0.25 3.0
)
ADD_TEST( CurvatureFlowImageFilter ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/CurvatureFlowImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/CurvatureFlowImageFilter.png
  CurvatureFlowImageFilter
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/CurvatureFlowImageFilter.png
        5 0.25 3.0
)
ADD_TEST( DanielssonDistanceMapImageFilter ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/DanielssonDistanceMapImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/DanielssonDistanceMapImageFilter.png
  DanielssonDistanceMapImageFilter
        ${ITK_SOURCE_DIR}/Examples/Data/FivePoints.png
        ${ITK_BINARY_DIR}/Testing/Temporary/DanielssonDistanceMapImageFilter.png
        ${ITK_BINARY_DIR}/Testing/Temporary/DanielssonDistanceMapImageFilter2.png
        ${ITK_BINARY_DIR}/Testing/Temporary/DanielssonDistanceMapImageFilter.mha
)
ADD_TEST( DiscreteGaussianImageFilter ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/DiscreteGaussianImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/DiscreteGaussianImageFilter.png
  DiscreteGaussianImageFilter
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/DiscreteGaussianImageFilter.png
        1.73 10
)
ADD_TEST( GradientAnisotropicDiffusionImageFilter ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/GradientAnisotropicDiffusionImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/GradientAnisotropicDiffusionImageFilter.png
  GradientAnisotropicDiffusionImageFilter
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/GradientAnisotropicDiffusionImageFilter.png
        5 0.25 3.0
)
ADD_TEST( GradientMagnitudeImageFilter ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/GradientMagnitudeImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/GradientMagnitudeImageFilter.png
  GradientMagnitudeImageFilter
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/GradientMagnitudeImageFilter.png
)
ADD_TEST( GradientMagnitudeRecursiveGaussianImageFilter ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/GradientMagnitudeRecursiveGaussianImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/GradientMagnitudeRecursiveGaussianImageFilter.png
  GradientMagnitudeRecursiveGaussianImageFilter
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/GradientMagnitudeRecursiveGaussianImageFilter.png
        5
)
ADD_TEST( LaplacianImageFilter ${FILTERING_EXAMPLES}
  --compare ${ITK_DATA_ROOT}/Baseline/Filtering/LaplacianImageFilter.png
            ${ITK_BINARY_DIR}/Testing/Temporary/LaplacianImageFilter.png
  LaplacianImageFilter
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_BINARY_DIR}/Testing/Temporary/LaplacianImageFilter.png
)
ADD_EXECUTABLE(FilteringExamples FilteringExamples.cxx ${FilteringExamples_SRCS})
TARGET_LINK_LIBRARIES(FilteringExamples ITKIO ITKBasicFilters ITKCommon)
