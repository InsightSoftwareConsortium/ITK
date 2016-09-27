set(BASELINE ${ITK_DATA_ROOT}/Baseline/Filtering)
set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

itk_add_test(NAME BilateralImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/BilateralImageFilter.png,:}
            ${TEMP}/BilateralImageFilter.png
  $<TARGET_FILE:BilateralImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BilateralImageFilter.png
        6.0 5.0
)
itk_add_test(NAME  BinaryMinMaxCurvatureFlowImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/BinaryMinMaxCurvatureFlowImageFilter.png}
            ${TEMP}/BinaryMinMaxCurvatureFlowImageFilter.png
  $<TARGET_FILE:BinaryMinMaxCurvatureFlowImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BinaryMinMaxCurvatureFlowImageFilter.png
        10 .125 1 128
)
itk_add_test(NAME  BinaryThresholdImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/BinaryThresholdImageFilter.png}
            ${TEMP}/BinaryThresholdImageFilter.png
  $<TARGET_FILE:BinaryThresholdImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BinaryThresholdImageFilter.png
        150 180 0 255
)
itk_add_test(NAME  BinaryMedianImageFilterTest1 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/BinaryMedianImageFilter1.png}
            ${TEMP}/BinaryMedianImageFilter1.png
  $<TARGET_FILE:BinaryMedianImageFilter>
        DATA{${BASELINE}/BinaryThresholdImageFilter.png}
        ${TEMP}/BinaryMedianImageFilter1.png
        1 1
)
itk_add_test(NAME  BinaryMedianImageFilterTest2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/BinaryMedianImageFilter2.png}
            ${TEMP}/BinaryMedianImageFilter2.png
  $<TARGET_FILE:BinaryMedianImageFilter>
        DATA{${BASELINE}/BinaryThresholdImageFilter.png}
        ${TEMP}/BinaryMedianImageFilter2.png
        2 2
)
itk_add_test(NAME  BinaryMedianImageFilterTest3 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/BinaryMedianImageFilter3.png}
            ${TEMP}/BinaryMedianImageFilter3.png
  $<TARGET_FILE:BinaryMedianImageFilter>
        DATA{${BASELINE}/BinaryThresholdImageFilter.png}
        ${TEMP}/BinaryMedianImageFilter3.png
        3 3
)
itk_add_test(NAME  VotingBinaryHoleFillingImageFilterTest1 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/VotingBinaryHoleFillingImageFilter1.png}
            ${TEMP}/VotingBinaryHoleFillingImageFilter1.png
  $<TARGET_FILE:VotingBinaryHoleFillingImageFilter>
        DATA{${BASELINE}/BinaryThresholdImageFilter.png}
        ${TEMP}/VotingBinaryHoleFillingImageFilter1.png
        1 1
)
itk_add_test(NAME  VotingBinaryHoleFillingImageFilterTest2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/VotingBinaryHoleFillingImageFilter2.png}
            ${TEMP}/VotingBinaryHoleFillingImageFilter2.png
  $<TARGET_FILE:VotingBinaryHoleFillingImageFilter>
        DATA{${BASELINE}/BinaryThresholdImageFilter.png}
        ${TEMP}/VotingBinaryHoleFillingImageFilter2.png
        2 2
)
itk_add_test(NAME  VotingBinaryHoleFillingImageFilterTest3 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/VotingBinaryHoleFillingImageFilter3.png}
            ${TEMP}/VotingBinaryHoleFillingImageFilter3.png
  $<TARGET_FILE:VotingBinaryHoleFillingImageFilter>
        DATA{${BASELINE}/BinaryThresholdImageFilter.png}
        ${TEMP}/VotingBinaryHoleFillingImageFilter3.png
        3 3
)
itk_add_test(NAME  VotingBinaryIterativeHoleFillingImageFilterTest1 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/VotingBinaryIterativeHoleFillingImageFilter1.png}
            ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter1.png
  $<TARGET_FILE:VotingBinaryIterativeHoleFillingImageFilter>
        DATA{${BASELINE}/BinaryThresholdImageFilter.png}
        ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter1.png
        1 1 10
)
itk_add_test(NAME  VotingBinaryIterativeHoleFillingImageFilterTest2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/VotingBinaryIterativeHoleFillingImageFilter2.png}
            ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter2.png
  $<TARGET_FILE:VotingBinaryIterativeHoleFillingImageFilter>
        DATA{${BASELINE}/BinaryThresholdImageFilter.png}
        ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter2.png
        2 2 10
)
itk_add_test(NAME  VotingBinaryIterativeHoleFillingImageFilterTest3 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/VotingBinaryIterativeHoleFillingImageFilter3.png}
            ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter3.png
  $<TARGET_FILE:VotingBinaryIterativeHoleFillingImageFilter>
        DATA{${BASELINE}/BinaryThresholdImageFilter.png}
        ${TEMP}/VotingBinaryIterativeHoleFillingImageFilter3.png
        3 3 10
)
itk_add_test(NAME  BinomialBlurImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/BinomialBlurImageFilter.png}
            ${TEMP}/BinomialBlurImageFilter.png
  $<TARGET_FILE:BinomialBlurImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/BinomialBlurImageFilter.png
        5
)
itk_add_test(NAME  CompositeFilterExampleTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{Baseline/CompositeFilterExample.mha}
            ${TEMP}/CompositeFilterExample.mha
  $<TARGET_FILE:CompositeFilterExample>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/CompositeFilterExample.mha
)
itk_add_test(NAME  CurvatureAnisotropicDiffusionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/CurvatureAnisotropicDiffusionImageFilter.png,:}
            ${TEMP}/CurvatureAnisotropicDiffusionImageFilter.png
  $<TARGET_FILE:CurvatureAnisotropicDiffusionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/CurvatureAnisotropicDiffusionImageFilter.png
        5 0.125 3
)
itk_add_test(NAME  CurvatureFlowImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/CurvatureFlowImageFilter.png}
            ${TEMP}/CurvatureFlowImageFilter.png
  $<TARGET_FILE:CurvatureFlowImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/CurvatureFlowImageFilter.png
        5 0.125 1.0
)
itk_add_test(NAME  DanielssonDistanceMapImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/DanielssonDistanceMapImageFilter.png}
            ${TEMP}/DanielssonDistanceMapImageFilter.png
  $<TARGET_FILE:DanielssonDistanceMapImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/FivePoints.png
        ${TEMP}/DanielssonDistanceMapImageFilter.png
        ${TEMP}/DanielssonDistanceMapImageFilter2.png
        ${TEMP}/DanielssonDistanceMapImageFilter.mha
)
itk_add_test(NAME  DiscreteGaussianImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/DiscreteGaussianImageFilter.png}
            ${TEMP}/DiscreteGaussianImageFilter.png
  $<TARGET_FILE:DiscreteGaussianImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/DiscreteGaussianImageFilter.png
        1.73 10
)
itk_add_test(NAME  GradientAnisotropicDiffusionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/GradientAnisotropicDiffusionImageFilter.png}
            ${TEMP}/GradientAnisotropicDiffusionImageFilter.png
  $<TARGET_FILE:GradientAnisotropicDiffusionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GradientAnisotropicDiffusionImageFilter.png
        5 0.125 1.0
)
itk_add_test(NAME  GradientMagnitudeImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/GradientMagnitudeImageFilter.png}
            ${TEMP}/GradientMagnitudeImageFilter.png
  $<TARGET_FILE:GradientMagnitudeImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GradientMagnitudeImageFilter.png
)
itk_add_test(NAME  GradientMagnitudeRecursiveGaussianImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/GradientMagnitudeRecursiveGaussianImageFilter.png}
            ${TEMP}/GradientMagnitudeRecursiveGaussianImageFilter.png
  $<TARGET_FILE:GradientMagnitudeRecursiveGaussianImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/GradientMagnitudeRecursiveGaussianImageFilter.png
        5
)
itk_add_test(NAME  GradientRecursiveGaussianImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:GradientRecursiveGaussianImageFilter>
        DATA{${ITK_SOURCE_DIR}/Testing/Data/Input/HeadMRVolume.mhd,HeadMRVolume.raw}
        ${TEMP}/GradientRecursiveGaussianImageFilterTest.mha
        1.5  # Sigma
)
itk_add_test(NAME  GradientVectorFlowImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:GradientVectorFlowImageFilter>
        DATA{${ITK_SOURCE_DIR}/Testing/Data/Input/GradientRecursiveGaussianImageFilterTest.mha}
        ${TEMP}/GradientVectorFlowImageFilterTest.mha
        5        # number of iterations
        2000.0   # noise level
)
set_tests_properties( GradientVectorFlowImageFilterTest
  PROPERTIES DEPENDS GradientRecursiveGaussianImageFilterTest)
itk_add_test(NAME  DerivativeImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/DerivativeImageFilter.mha}
            ${TEMP}/DerivativeImageFilter.mha
  $<TARGET_FILE:DerivativeImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/DerivativeImageFilter.mha
        ${TEMP}/DerivativeImageFilter.png
        1 1
)
itk_add_test(NAME  LaplacianImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/LaplacianImageFilter.png,:}
            ${TEMP}/LaplacianImageFilter.png
  $<TARGET_FILE:LaplacianImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/LaplacianImageFilter.png
)
itk_add_test(NAME  LaplacianSharpeningImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/LaplacianSharpeningImageFilter.png,:}
            ${TEMP}/LaplacianSharpeningImageFilter.png
  $<TARGET_FILE:LaplacianSharpeningImageFilter>
        DATA{${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png}
        ${TEMP}/LaplacianSharpeningImageFilter.png
)
itk_add_test(NAME  MathematicalMorphologyBinaryFiltersTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/MathematicalMorphologyBinaryFiltersErode.png}
            ${TEMP}/MathematicalMorphologyBinaryFiltersErode.png
  $<TARGET_FILE:MathematicalMorphologyBinaryFilters>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MathematicalMorphologyBinaryFiltersErode.png
        ${TEMP}/MathematicalMorphologyBinaryFiltersDilate.png
        150 180
)
itk_add_test(NAME  MathematicalMorphologyGrayscaleFiltersTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/MathematicalMorphologyGrayscaleFiltersErode.png}
            ${TEMP}/MathematicalMorphologyGrayscaleFiltersErode.png
  $<TARGET_FILE:MathematicalMorphologyGrayscaleFilters>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MathematicalMorphologyGrayscaleFiltersErode.png
        ${TEMP}/MathematicalMorphologyGrayscaleFiltersDilate.png
)
itk_add_test(NAME  MeanImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/MeanImageFilter.png}
            ${TEMP}/MeanImageFilter.png
  $<TARGET_FILE:MeanImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MeanImageFilter.png
)
itk_add_test(NAME  MedianImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/MedianImageFilter.png}
            ${TEMP}/MedianImageFilter.png
  $<TARGET_FILE:MedianImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/MedianImageFilter.png
        1
)
itk_add_test(NAME  ResampleImageFilterTest1 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilterTest1.png}
            ${TEMP}/ResampleImageFilterTest1.png
  $<TARGET_FILE:ResampleImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest1.png
        0
)
itk_add_test(NAME  ResampleOrientedImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleOrientedImageFilterTest.png}
            ${TEMP}/ResampleOrientedImageFilterTest.png
  $<TARGET_FILE:ResampleOrientedImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanHeadSlice.png
        ${TEMP}/ResampleOrientedImageFilterTest.png
        0 1 1 0
)
itk_add_test(NAME  ResampleImageFilterTest2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilterTest2.png}
            ${TEMP}/ResampleImageFilterTest2.png
  $<TARGET_FILE:ResampleImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest2.png
        1
)
itk_add_test(NAME  ResampleImageFilterTest3 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilterTest3.png}
            ${TEMP}/ResampleImageFilterTest3.png
  $<TARGET_FILE:ResampleImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest3.png
        2
)
itk_add_test(NAME  ResampleImageFilterTest4 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilterTest4.png}
            ${TEMP}/ResampleImageFilterTest4.png
  $<TARGET_FILE:ResampleImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilterTest4.png
        3
)
itk_add_test(NAME  ResampleImageFilter2Test1 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilter2Test1.png}
            ${TEMP}/ResampleImageFilter2Test1.png
  $<TARGET_FILE:ResampleImageFilter2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test1.png
        0
)
itk_add_test(NAME  ResampleImageFilter2Test2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilter2Test2.png}
            ${TEMP}/ResampleImageFilter2Test2.png
  $<TARGET_FILE:ResampleImageFilter2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test2.png
        1
)
itk_add_test(NAME  ResampleImageFilter2Test3 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilter2Test3.png}
            ${TEMP}/ResampleImageFilter2Test3.png
  $<TARGET_FILE:ResampleImageFilter2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test3.png
        2
)
itk_add_test(NAME  ResampleImageFilter2Test4 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilter2Test4.png}
            ${TEMP}/ResampleImageFilter2Test4.png
  $<TARGET_FILE:ResampleImageFilter2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test4.png
        3
)
itk_add_test(NAME  ResampleImageFilter2Test5 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilter2Test5.png}
            ${TEMP}/ResampleImageFilter2Test5.png
  $<TARGET_FILE:ResampleImageFilter2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter2Test5.png
        3
)
itk_add_test(NAME  ResampleImageFilter3Test1 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilter3Test1PixelCentered.png}
            ${TEMP}/ResampleImageFilter3Test1PixelCentered.png
  $<TARGET_FILE:ResampleImageFilter3>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter3Test1PixelCentered.png
        0
)

itk_add_test(NAME  ResampleImageFilter4Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilter4TestPixelCentered.png}
            ${TEMP}/ResampleImageFilter4TestPixelCentered.png
  $<TARGET_FILE:ResampleImageFilter4>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter4TestPixelCentered.png
        20
)
itk_add_test(NAME  ResampleImageFilter5Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilter5TestPixelCentered.png}
            ${TEMP}/ResampleImageFilter5TestPixelCentered.png
  $<TARGET_FILE:ResampleImageFilter5>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter5TestPixelCentered.png
        10 1.2
)
itk_add_test(NAME  ResampleImageFilter8Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilter8TestPixelCentered.png}
            ${TEMP}/ResampleImageFilter8TestPixelCentered.png
  $<TARGET_FILE:ResampleImageFilter8>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ResampleImageFilter8TestPixelCentered.png
        45
)
itk_add_test(NAME  ResampleImageFilter9Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilter9TestPixelCentered.png,:}
            ${TEMP}/ResampleImageFilter9TestNearestPixelCentered.png
  $<TARGET_FILE:ResampleImageFilter9>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/ResampleImageFilter9TestNearestPixelCentered.png
        ${TEMP}/ResampleImageFilter9TestLinearPixelCentered.png
)
itk_add_test(NAME  SigmoidImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/SigmoidImageFilter.png,:}
            ${TEMP}/SigmoidImageFilter.png
  $<TARGET_FILE:SigmoidImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/SigmoidImageFilter.png
        10 240 10 170
)
itk_add_test(NAME  SmoothingRecursiveGaussianImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/SmoothingRecursiveGaussianImageFilter.png}
            ${TEMP}/SmoothingRecursiveGaussianImageFilter.png
  $<TARGET_FILE:SmoothingRecursiveGaussianImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/SmoothingRecursiveGaussianImageFilter.png
        3
)
itk_add_test(NAME  SmoothingRecursiveGaussianImageFilter2Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/SmoothingRecursiveGaussianImageFilter2.png}
            ${TEMP}/SmoothingRecursiveGaussianImageFilter2.png
  $<TARGET_FILE:SmoothingRecursiveGaussianImageFilter2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/SmoothingRecursiveGaussianImageFilter2.png
        3
)
itk_add_test(NAME  ThresholdAboveBelowOutsideImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ThresholdAboveImageFilter.png}
            ${TEMP}/ThresholdAboveImageFilter.png
  --compare DATA{${BASELINE}/ThresholdBelowImageFilter.png}
            ${TEMP}/ThresholdBelowImageFilter.png
  --compare DATA{${BASELINE}/ThresholdOutsideImageFilter.png}
            ${TEMP}/ThresholdOutsideImageFilter.png
  $<TARGET_FILE:ThresholdImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ThresholdBelowImageFilter.png
        ${TEMP}/ThresholdAboveImageFilter.png
        ${TEMP}/ThresholdOutsideImageFilter.png
)
itk_add_test(NAME  VectorGradientAnisotropicDiffusionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/VectorGradientAnisotropicDiffusionImageFilter.png}
            ${TEMP}/VectorGradientAnisotropicDiffusionImageFilter.png
  $<TARGET_FILE:VectorGradientAnisotropicDiffusionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/VectorGradientAnisotropicDiffusionImageFilterOriginal.png
        ${TEMP}/VectorGradientAnisotropicDiffusionImageFilter.png
        .25 5
)
itk_add_test(NAME  VectorCurvatureAnisotropicDiffusionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/VectorCurvatureAnisotropicDiffusionImageFilter.png,:}
            ${TEMP}/VectorCurvatureAnisotropicDiffusionImageFilter.png
  $<TARGET_FILE:VectorCurvatureAnisotropicDiffusionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/VectorCurvatureAnisotropicDiffusionImageFilterOriginal.png
        ${TEMP}/VectorCurvatureAnisotropicDiffusionImageFilter.png
        0.125 5
)
itk_add_test(NAME  VectorIndexSelectionTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/VectorIndexSelection.png}
            ${TEMP}/VectorIndexSelection.png
  $<TARGET_FILE:VectorIndexSelection>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/VectorIndexSelection.png
        1
)
itk_add_test(NAME  ZeroCrossingBasedEdgeDetectionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ZeroCrossingBasedEdgeDetectionImageFilter1.png}
            ${TEMP}/ZeroCrossingBasedEdgeDetectionImageFilter1.png
  $<TARGET_FILE:ZeroCrossingBasedEdgeDetectionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/ZeroCrossingBasedEdgeDetectionImageFilter1.png
        5.0
        0.03
)
itk_add_test(NAME  RGBGradientAnisotropicDiffusionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/RGBGradientAnisotropicDiffusionImageFilter.png}
            ${TEMP}/RGBGradientAnisotropicDiffusionImageFilter.png
  $<TARGET_FILE:RGBGradientAnisotropicDiffusionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/RGBGradientAnisotropicDiffusionImageFilter.png
        0.125 20
)
itk_add_test(NAME  RGBCurvatureAnisotropicDiffusionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/RGBCurvatureAnisotropicDiffusionImageFilter.png,:}
            ${TEMP}/RGBCurvatureAnisotropicDiffusionImageFilter.png
  $<TARGET_FILE:RGBCurvatureAnisotropicDiffusionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/RGBCurvatureAnisotropicDiffusionImageFilter.png
        0.125 20
)
itk_add_test(NAME  MorphologicalImageEnhancementTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/MorphologicalImageEnhancement.png}
            ${TEMP}/MorphologicalImageEnhancement.png
  $<TARGET_FILE:MorphologicalImageEnhancement>
        DATA{${ITK_SOURCE_DIR}/Testing/Data/Input/peppers.png}
        ${TEMP}/MorphologicalImageEnhancement.png
        3
)
itk_add_test(NAME  OtsuThresholdImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/OtsuThresholdImageFilter.png}
            ${TEMP}/OtsuThresholdImageFilter.png
  $<TARGET_FILE:OtsuThresholdImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/OtsuThresholdImageFilter.png
        255 0
)
itk_add_test(NAME  OtsuMultipleThresholdImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/OtsuMultipleThresholdOutput002.png}
            ${TEMP}/OtsuMultipleThresholdOutput002.png
  $<TARGET_FILE:OtsuMultipleThresholdImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/OtsuMultipleThresholdOutput
        png 3
)
itk_add_test(NAME  DigitallyReconstructedRadiograph1Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/DigitallyReconstructedRadiograph1.png}
            ${TEMP}/DigitallyReconstructedRadiograph1.png
  $<TARGET_FILE:DigitallyReconstructedRadiograph1>
        -o ${TEMP}/DigitallyReconstructedRadiograph1.png
)
itk_add_test(NAME  AntiAliasBinaryImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/AntiAliasBinaryImageFilter.png}
            ${TEMP}/AntiAliasBinaryImageFilter.png
  $<TARGET_FILE:AntiAliasBinaryImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VentricleModel.png
        ${TEMP}/AntiAliasBinaryImageFilterDoublePixelType.mha
        ${TEMP}/AntiAliasBinaryImageFilter.png
        .001 50
)
itk_add_test(NAME  CannyEdgeDetectionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/CannyEdgeDetectionImageFilter.png}
            ${TEMP}/CannyEdgeDetectionImageFilter.png
  $<TARGET_FILE:CannyEdgeDetectionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/CannyEdgeDetectionImageFilter.png
        9     # Sigma
        0     # Upper Threshold
        0     # Lower Threshold
)
itk_add_test(NAME  CannyEdgeDetectionImageFilterTest2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/CannyEdgeDetectionImageFilter2.png}
            ${TEMP}/CannyEdgeDetectionImageFilter2.png
  $<TARGET_FILE:CannyEdgeDetectionImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/CannyEdgeDetectionImageFilter2.png
        9     # Sigma
        5     # Upper Threshold
        1     # Lower Threshold
)
itk_add_test(NAME  FlipImageFilterTest1 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/FlipImageFilterX.png}
            ${TEMP}/FlipImageFilterX.png
  $<TARGET_FILE:FlipImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/FlipImageFilterX.png
        1 0
)
itk_add_test(NAME  FlipImageFilterTest2 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/FlipImageFilterY.png}
            ${TEMP}/FlipImageFilterY.png
  $<TARGET_FILE:FlipImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/FlipImageFilterY.png
        0 1
)
itk_add_test(NAME  FlipImageFilterTest3 COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/FlipImageFilterXY.png}
            ${TEMP}/FlipImageFilterXY.png
  $<TARGET_FILE:FlipImageFilter>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanEyeSlice.png
        ${TEMP}/FlipImageFilterXY.png
        1 1
)
itk_add_test(NAME  GaussianBlurImageFunctionTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/GaussianBlurImageFunction.mha}
            ${TEMP}/GaussianBlurImageFunction.mha
  $<TARGET_FILE:GaussianBlurImageFunction>
        ${ITK_SOURCE_DIR}/Examples/Data/VentricleModel.png
        ${TEMP}/GaussianBlurImageFunction.mha
        2 3
)
itk_add_test(NAME  FFTImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/FFTImageFilterReal.png}
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
if( ITK_USE_FFTWF )
#DATA{${BASELINE}/FFTDirectInverse2Point.png}
  itk_add_test(NAME  FFTDirectInverse2Test COMMAND ${ITK_TEST_DRIVER}
    --compare ${ITK_SOURCE_DIR}/Examples/Data/Point.png
              ${TEMP}/FFTDirectInverse2Point.png
    $<TARGET_FILE:FFTDirectInverse2>
          ${ITK_SOURCE_DIR}/Examples/Data/Point.png
          ${TEMP}/FFTDirectInverse2Point.png
  )
endif()
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
  --compare DATA{${BASELINE}/RGBToGrayscale.png}
            ${TEMP}/RGBToGrayscale.png
  $<TARGET_FILE:RGBToGrayscale>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanHeadSlice.png
        ${TEMP}/RGBToGrayscale.png
)
itk_add_test(NAME  LaplacianRecursiveGaussianImageFilter1Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/LaplacianRecursiveGaussianImageFilter1.mha}
            ${TEMP}/LaplacianRecursiveGaussianImageFilter1.mha
  $<TARGET_FILE:LaplacianRecursiveGaussianImageFilter1>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/LaplacianRecursiveGaussianImageFilter1.mha
        3
)
itk_add_test(NAME  LaplacianRecursiveGaussianImageFilter2Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/LaplacianRecursiveGaussianImageFilter2.mha}
            ${TEMP}/LaplacianRecursiveGaussianImageFilter2.mha
  $<TARGET_FILE:LaplacianRecursiveGaussianImageFilter2>
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${TEMP}/LaplacianRecursiveGaussianImageFilter2.mha
        3
)
itk_add_test(NAME  ResampleImageFilter6Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilter6PixelCentered.png}
            ${TEMP}/ResampleImageFilter6PixelCentered.png
  $<TARGET_FILE:ResampleImageFilter6>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanHeadSlice.png
        ${TEMP}/ResampleImageFilter6PixelCentered.png
        3
)
itk_add_test(NAME  ResampleImageFilter7Test COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleImageFilter7PixelCentered.png}
            ${TEMP}/ResampleImageFilter7PixelCentered.png
  --compareNumberOfPixelsTolerance 1
  $<TARGET_FILE:ResampleImageFilter7>
        ${ITK_SOURCE_DIR}/Examples/Data/VisibleWomanHeadSlice.png
        ${TEMP}/ResampleImageFilter7PixelCentered.png
        33
)

# The data contained in ${ITK_DATA_ROOT}/Input/DicomSeries/
# is required by mri3D.mhd:
# ElementDataFile = DicomSeries/Image%04d.dcm 75 77 1
# The data file and its associated files are in different
# directories, respectively ${ITK_DATA_ROOT}/Input/ and
# ${ITK_DATA_ROOT}/Input/DicomSeries/
# Since DATA{} does not allow to specify an associated
# file that is in a different directory, we call ExternalData_Expand_Arguments
# manually to download the required files.
ExternalData_Expand_Arguments(ITKData
                              ExpandedData
                              DATA{${ITK_DATA_ROOT}/Input/DicomSeries/,REGEX:Image[0-9]+.dcm}
                             )
itk_add_test(NAME  ResampleVolumesToBeIsotropicTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ResampleVolumesToBeIsotropic.mha}
            ${TEMP}/ResampleVolumesToBeIsotropic.mha
  $<TARGET_FILE:ResampleVolumesToBeIsotropic>
        DATA{${ITK_SOURCE_DIR}/Testing/Data/Input/mri3D.mhd}
        ${TEMP}/ResampleVolumesToBeIsotropic.mha
        0 255
)
itk_add_test(NAME  ScaleSpaceGenerator2DTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/ScaleSpaceGenerator2D005.mhd,ScaleSpaceGenerator2D005.raw}
            ${TEMP}/ScaleSpaceGenerator2D005.mhd
  $<TARGET_FILE:ScaleSpaceGenerator2D>
        DATA{${ITK_SOURCE_DIR}/Testing/Data/Input/cthead1.png}
        ${TEMP}/ScaleSpaceGenerator2D
        10
)
itk_add_test(NAME  SubsampleVolumeTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/SubsampleVolume.mha}
            ${TEMP}/SubsampleVolume.mha
  $<TARGET_FILE:SubsampleVolume>
        DATA{${ITK_SOURCE_DIR}/Testing/Data/Input/HeadMRVolume.mhd,HeadMRVolume.raw}
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
set_property(TEST SpatialObjectToImage2 APPEND PROPERTY LABELS RUNS_LONG)
itk_add_test(NAME  SpatialObjectToImage3 COMMAND ${ITK_TEST_DRIVER}
  $<TARGET_FILE:SpatialObjectToImage3>
        ${TEMP}/SpatialObjectToImage3.mha
)

# This test tests 3 filters.
#   DiffusionTensor3DReconstructionImageFilter
#   TensorFractionalAnisotropyImageFilter
#   TensorRelativeAnisotropyImageFilter
#
itk_add_test(NAME  DiffusionTensor3DReconstructionImageFilterTest COMMAND ${ITK_TEST_DRIVER}
  --compare DATA{${BASELINE}/Tensors.mhd,Tensors.raw}
            ${TEMP}/Tensors.mhd
  --compare DATA{${BASELINE}/FACorpusCallosum.mhd,FACorpusCallosum.raw}
            ${TEMP}/FACorpusCallosum.mhd
  --compare DATA{${BASELINE}/RACorpusCallosum.mhd,RACorpusCallosum.raw}
            ${TEMP}/RACorpusCallosum.mhd
  $<TARGET_FILE:DiffusionTensor3DReconstructionImageFilter>
        DATA{${ITK_SOURCE_DIR}/Testing/Data/Input/DwiCorpusCallosum.nhdr,DwiCorpusCallosum.raw}
        80 ${TEMP}/Tensors.mhd ${TEMP}/FACorpusCallosum.mhd
        ${TEMP}/RACorpusCallosum.mhd 0
)
