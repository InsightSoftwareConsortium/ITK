IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-BasicFilters ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/BasicFilters)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)

SET(BASIC_FILTERS_TESTS ${CXX_TEST_PATH}/itkBasicFiltersTests)
SET(BASIC_FILTERS_HEADER_TEST ${CXX_TEST_PATH}/itkBasicFiltersHeaderTest)

ADD_TEST(itkBasicFiltersPrintTest ${BASIC_FILTERS_TESTS} itkBasicFiltersPrintTest)
ADD_TEST(itkBasicFiltersPrintTest2 ${BASIC_FILTERS_TESTS} itkBasicFiltersPrintTest2)

ADD_TEST(itkAcosImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkAcosImageFilterAndAdaptorTest)
ADD_TEST(itkAdaptImageFilterTest ${BASIC_FILTERS_TESTS} itkAdaptImageFilterTest)
ADD_TEST(itkAdaptImageFilterTest2 ${BASIC_FILTERS_TESTS} itkAdaptImageFilterTest2)
ADD_TEST(itkAddImageFilterTest ${BASIC_FILTERS_TESTS} itkAddImageFilterTest)
ADD_TEST(itkAsinImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkAsinImageFilterAndAdaptorTest)
ADD_TEST(itkAtanImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkAtanImageFilterAndAdaptorTest)
ADD_TEST(itkBSplineDecompositionImageFilterTest ${BASIC_FILTERS_TESTS} itkBSplineDecompositionImageFilterTest)
ADD_TEST(itkBSplineInterpolateImageFunctionTest ${BASIC_FILTERS_TESTS} itkBSplineInterpolateImageFunctionTest)
ADD_TEST(itkBSplineResampleImageFilterTest ${BASIC_FILTERS_TESTS} itkBSplineResampleImageFilterTest)
ADD_TEST(itkBSplineResampleImageFunctionTest ${BASIC_FILTERS_TESTS} itkBSplineResampleImageFunctionTest)
ADD_TEST(itkBasicArchitectureTest ${BASIC_FILTERS_TESTS} itkBasicArchitectureTest)
ADD_TEST(itkBilateralImageFilterTest ${BASIC_FILTERS_TESTS} itkBilateralImageFilterTest)
ADD_TEST(itkBinaryDilateImageFilterTest ${BASIC_FILTERS_TESTS} itkBinaryDilateImageFilterTest)
ADD_TEST(itkBinaryErodeImageFilterTest ${BASIC_FILTERS_TESTS} itkBinaryErodeImageFilterTest)
ADD_TEST(itkBinaryMagnitudeImageFilterTest ${BASIC_FILTERS_TESTS} itkBinaryMagnitudeImageFilterTest)
ADD_TEST(itkBinaryMaskToNarrowBandPointSetFilterTest ${BASIC_FILTERS_TESTS} itkBinaryMaskToNarrowBandPointSetFilterTest)
ADD_TEST(itkBinaryMedianImageFilterTest ${BASIC_FILTERS_TESTS} itkBinaryMedianImageFilterTest)
ADD_TEST(itkBinaryThresholdImageFilterTest ${BASIC_FILTERS_TESTS} itkBinaryThresholdImageFilterTest)
ADD_TEST(itkBloxBoundaryPointImageTest ${BASIC_FILTERS_TESTS} itkBloxBoundaryPointImageTest)
ADD_TEST(itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest ${BASIC_FILTERS_TESTS} itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest)
ADD_TEST(itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest${BASIC_FILTERS_TESTS} itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest)
ADD_TEST(itkBloxCoreAtomTest ${BASIC_FILTERS_TESTS} itkBloxCoreAtomTest)
ADD_TEST(itkCannyEdgeDetectionImageFilterTest ${BASIC_FILTERS_TESTS} itkCannyEdgeDetectionImageFilterTest)
ADD_TEST(itkChangeInformationImageFilterTest ${BASIC_FILTERS_TESTS} itkChangeInformationImageFilterTest)
ADD_TEST(itkComposeRGBImageFilterTest ${BASIC_FILTERS_TESTS} itkComposeRGBImageFilterTest)
ADD_TEST(itkConstantPadImageTest ${BASIC_FILTERS_TESTS} itkConstantPadImageTest)
ADD_TEST(itkCosImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkCosImageFilterAndAdaptorTest)
ADD_TEST(itkCropImageFilterTest ${BASIC_FILTERS_TESTS} itkCropImageFilterTest)
ADD_TEST(itkCurvatureAnisotropicDiffusionImageFilterTest ${BASIC_FILTERS_TESTS} itkCurvatureAnisotropicDiffusionImageFilterTest)
ADD_TEST(itkCyclicReferences ${BASIC_FILTERS_TESTS} itkCyclicReferences)
ADD_TEST(itkDanielssonDistanceMapImageFilterTest ${BASIC_FILTERS_TESTS} itkDanielssonDistanceMapImageFilterTest)
ADD_TEST(itkDerivativeImageFilterTest ${BASIC_FILTERS_TESTS} itkDerivativeImageFilterTest)
ADD_TEST(itkDifferenceOfGaussiansGradientTest ${BASIC_FILTERS_TESTS} itkDifferenceOfGaussiansGradientTest)
ADD_TEST(itkDiscreteGaussianImageFilterTest ${BASIC_FILTERS_TESTS} itkDiscreteGaussianImageFilterTest)
ADD_TEST(itkDivideImageFilterTest ${BASIC_FILTERS_TESTS} itkDivideImageFilterTest)
ADD_TEST(itkEdgePotentialImageFilterTest ${BASIC_FILTERS_TESTS} itkEdgePotentialImageFilterTest)
ADD_TEST(itkEigenAnalysis2DImageFilterTest ${BASIC_FILTERS_TESTS} itkEigenAnalysis2DImageFilterTest)
ADD_TEST(itkExpImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkExpImageFilterAndAdaptorTest)
ADD_TEST(itkExpNegativeImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkExpNegativeImageFilterAndAdaptorTest)
ADD_TEST(itkExpandImageFilterTest ${BASIC_FILTERS_TESTS} itkExpandImageFilterTest)
ADD_TEST(itkExtractImageTest ${BASIC_FILTERS_TESTS} itkExtractImageTest)
ADD_TEST(itkExtractImageTest ${BASIC_FILTERS_TESTS} itkExtractOrthogonalSwath2DImageFilterTest)
ADD_TEST(itkFilterDispatchTest ${BASIC_FILTERS_TESTS} itkFilterDispatchTest)
ADD_TEST(itkFlipImageFilterTest ${BASIC_FILTERS_TESTS} itkFlipImageFilterTest)
ADD_TEST(itkFloodFillIteratorTest ${BASIC_FILTERS_TESTS} itkFloodFillIteratorTest)
ADD_TEST(itkGaussianImageSourceTest ${BASIC_FILTERS_TESTS} itkGaussianImageSourceTest)
ADD_TEST(itkGradientAnisotropicDiffusionImageFilterTest ${BASIC_FILTERS_TESTS} itkGradientAnisotropicDiffusionImageFilterTest)
ADD_TEST(itkGradientImageFilterTest ${BASIC_FILTERS_TESTS} itkGradientImageFilterTest)
ADD_TEST(itkGradientMagnitudeImageFilterTest ${BASIC_FILTERS_TESTS} itkGradientMagnitudeImageFilterTest)
ADD_TEST(itkGradientMagnitudeRecursiveGaussianFilterTest ${BASIC_FILTERS_TESTS} itkGradientMagnitudeRecursiveGaussianFilterTest)
ADD_TEST(itkGradientRecursiveGaussianFilterTest ${BASIC_FILTERS_TESTS} itkGradientRecursiveGaussianFilterTest)
ADD_TEST(itkGradientToMagnitudeImageFilterTest ${BASIC_FILTERS_TESTS} itkGradientToMagnitudeImageFilterTest)
ADD_TEST(itkHardConnectedComponentImageFilterTest ${BASIC_FILTERS_TESTS} itkHardConnectedComponentImageFilterTest)
ADD_TEST(itkHausdorffDistanceImageFilterTest ${BASIC_FILTERS_TESTS} itkHausdorffDistanceImageFilterTest)
ADD_TEST(itkHoughTransform2DCirclesImageTest ${BASIC_FILTERS_TESTS} itkHoughTransform2DCirclesImageTest)
ADD_TEST(itkHoughTransform2DLinesImageTest ${BASIC_FILTERS_TESTS} itkHoughTransform2DLinesImageTest)
ADD_TEST(itkImageAdaptorNthElementTest ${BASIC_FILTERS_TESTS} itkImageAdaptorNthElementTest)
ADD_TEST(itkImageAdaptorPipeLineTest ${BASIC_FILTERS_TESTS} itkImageAdaptorPipeLineTest)
ADD_TEST(itkImageToParametricSpaceFilterTest ${BASIC_FILTERS_TESTS} itkImageToParametricSpaceFilterTest)
ADD_TEST(itkImportImageTest ${BASIC_FILTERS_TESTS} itkImportImageTest)
ADD_TEST(itkIntensityWindowingImageFilterTest ${BASIC_FILTERS_TESTS} itkIntensityWindowingImageFilterTest )
ADD_TEST(itkInteriorExteriorMeshFilterTest ${BASIC_FILTERS_TESTS} itkInteriorExteriorMeshFilterTest )
ADD_TEST(itkInterpolateImageFilterTest ${BASIC_FILTERS_TESTS} itkInterpolateImageFilterTest )
ADD_TEST(itkInterpolateImagePointsFilterTest ${BASIC_FILTERS_TESTS} itkInterpolateImagePointsFilterTest )
ADD_TEST(itkJoinImageFilterTest ${BASIC_FILTERS_TESTS} itkJoinImageFilterTest)
ADD_TEST(itkLaplacianImageFilterTest ${BASIC_FILTERS_TESTS} itkLaplacianImageFilterTest)
ADD_TEST(itkLog10ImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkLog10ImageFilterAndAdaptorTest)
ADD_TEST(itkLogImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkLogImageFilterAndAdaptorTest)

ADD_TEST(itkMaskImageFilterTest ${BASIC_FILTERS_TESTS} itkMaskImageFilterTest)
ADD_TEST(itkMathematicalMorphologyImageFilterTest ${BASIC_FILTERS_TESTS} itkMathematicalMorphologyImageFilterTest)
ADD_TEST(itkMaximumImageFilterTest ${BASIC_FILTERS_TESTS} itkMaximumImageFilterTest)
ADD_TEST(itkMeanImageFilterTest ${BASIC_FILTERS_TESTS} itkMeanImageFilterTest)
ADD_TEST(itkMedianImageFilterTest ${BASIC_FILTERS_TESTS} itkMedianImageFilterTest)
ADD_TEST(itkMinimumImageFilterTest ${BASIC_FILTERS_TESTS} itkMinimumImageFilterTest)
ADD_TEST(itkMinimumMaximumImageCalculatorTest ${BASIC_FILTERS_TESTS} itkMinimumMaximumImageCalculatorTest)
ADD_TEST(itkMinimumMaximumImageFilterTest ${BASIC_FILTERS_TESTS} itkMinimumMaximumImageFilterTest)
ADD_TEST(itkMirrorPadImageTest ${BASIC_FILTERS_TESTS} itkMirrorPadImageTest)
ADD_TEST(itkMultiplyImageFilterTest ${BASIC_FILTERS_TESTS} itkMultiplyImageFilterTest)
ADD_TEST(itkNaryAddImageFilterTest ${BASIC_FILTERS_TESTS} itkNaryAddImageFilterTest)
ADD_TEST(itkNarrowBandTest ${BASIC_FILTERS_TESTS} itkNarrowBandTest)
ADD_TEST(itkNarrowBandImageFilterBaseTest ${BASIC_FILTERS_TESTS} itkNarrowBandImageFilterBaseTest)
ADD_TEST(itkNeighborhoodOperatorImageFilterTest ${BASIC_FILTERS_TESTS} itkNeighborhoodOperatorImageFilterTest)
ADD_TEST(itkNormalizeImageFilterTest ${BASIC_FILTERS_TESTS} itkNormalizeImageFilterTest)
ADD_TEST(itkObjectMorphologyImageFilterTest ${BASIC_FILTERS_TESTS} itkObjectMorphologyImageFilterTest)
ADD_TEST(itkPathToChainCodePathFilterTest ${BASIC_FILTERS_TESTS} itkPathToChainCodePathFilterTest)
ADD_TEST(itkPathToImageFilterTest ${BASIC_FILTERS_TESTS} itkPathToImageFilterTest)
ADD_TEST(itkPermuteAxesImageFilterTest ${BASIC_FILTERS_TESTS} itkPermuteAxesImageFilterTest)
ADD_TEST(itkPlaheImageFilterTest ${BASIC_FILTERS_TESTS} itkPlaheImageFilterTest)
ADD_TEST(itkRGBToVectorAdaptImageFilterTest.cxx ${BASIC_FILTERS_TESTS} itkRGBToVectorAdaptImageFilterTest)
ADD_TEST(itkRecursiveGaussianImageFiltersTest ${BASIC_FILTERS_TESTS} itkRecursiveGaussianImageFiltersTest)
ADD_TEST(itkReflectImageFilterTest ${BASIC_FILTERS_TESTS} itkReflectImageFilterTest)
ADD_TEST(itkReflectiveImageRegionIteratorTest ${BASIC_FILTERS_TESTS}  itkReflectiveImageRegionIteratorTest )
ADD_TEST(itkRegionOfInterestImageFilterTest ${BASIC_FILTERS_TESTS}  itkRegionOfInterestImageFilterTest )
ADD_TEST(itkResampleImageTest ${BASIC_FILTERS_TESTS} itkResampleImageTest)
ADD_TEST(itkRescaleIntensityImageFilterTest ${BASIC_FILTERS_TESTS} itkRescaleIntensityImageFilterTest)
ADD_TEST(itkShiftScaleImageFilterTest ${BASIC_FILTERS_TESTS} itkShiftScaleImageFilterTest)
ADD_TEST(itkShiftScaleInPlaceImageFilterTest ${BASIC_FILTERS_TESTS} itkShiftScaleInPlaceImageFilterTest)
ADD_TEST(itkShrinkImageTest ${BASIC_FILTERS_TESTS} itkShrinkImageTest)
ADD_TEST(itkSigmoidImageFilterTest ${BASIC_FILTERS_TESTS} itkSigmoidImageFilterTest)
ADD_TEST(itkSimilarityIndexImageFilterTest ${BASIC_FILTERS_TESTS} itkSimilarityIndexImageFilterTest)
ADD_TEST(itkSinImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkSinImageFilterAndAdaptorTest)
ADD_TEST(itkSobelEdgeDetectionImageFilterTest ${BASIC_FILTERS_TESTS} itkSobelEdgeDetectionImageFilterTest)
ADD_TEST(itkSmoothingRecursiveGaussianImageFilterTest ${BASIC_FILTERS_TESTS} itkSmoothingRecursiveGaussianImageFilterTest)
ADD_TEST(itkSparseFieldFourthOrderLevelSetImageFilterTest ${BASIC_FILTERS_TESTS} itkSparseFieldFourthOrderLevelSetImageFilterTest)
ADD_TEST(itkSparseFieldLayerTest ${BASIC_FILTERS_TESTS} itkSparseFieldLayerTest)
ADD_TEST(itkSpatialObjectToImageFilterTest ${BASIC_FILTERS_TESTS} itkSpatialObjectToImageFilterTest)
ADD_TEST(itkSpatialObjectToImageStatisticsCalculatorTest ${BASIC_FILTERS_TESTS} itkSpatialObjectToImageStatisticsCalculatorTest)
ADD_TEST(itkSpatialFunctionImageEvaluatorFilterTest ${BASIC_FILTERS_TESTS} itkSpatialFunctionImageEvaluatorFilterTest)
ADD_TEST(itkSqrtImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkSqrtImageFilterAndAdaptorTest)
ADD_TEST(itkSquareImageFilterTest ${BASIC_FILTERS_TESTS} itkSquareImageFilterTest)
ADD_TEST(itkSquaredDifferenceImageFilterTest ${BASIC_FILTERS_TESTS} itkSquaredDifferenceImageFilterTest)
ADD_TEST(itkStatisticsImageFilterTest ${BASIC_FILTERS_TESTS} itkStatisticsImageFilterTest)
ADD_TEST(itkStreamingImageFilterTest ${BASIC_FILTERS_TESTS} itkStreamingImageFilterTest)
ADD_TEST(itkStreamingImageFilterTest2 ${BASIC_FILTERS_TESTS} itkStreamingImageFilterTest2)
ADD_TEST(itkSubtractImageFilterTest ${BASIC_FILTERS_TESTS} itkSubtractImageFilterTest)
ADD_TEST(itkTanImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkTanImageFilterAndAdaptorTest)
ADD_TEST(itkTernaryMagnitudeImageFilterTest ${BASIC_FILTERS_TESTS} itkTernaryMagnitudeImageFilterTest)
ADD_TEST(itkThresholdImageFilterTest ${BASIC_FILTERS_TESTS} itkThresholdImageFilterTest)
ADD_TEST(itkTobogganImageFilterTest ${BASIC_FILTERS_TESTS} itkTobogganImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Baseline/BasicFilters/TobogganImageFilterTest.png)
ADD_TEST(itkTransformMeshFilterTest ${BASIC_FILTERS_TESTS} itkTransformMeshFilterTest)
ADD_TEST(itkTwoOutputExampleImageFilterTest ${BASIC_FILTERS_TESTS} itkTwoOutputExampleImageFilterTest)
ADD_TEST(itkVectorAnisotropicDiffusionImageFilterTest ${BASIC_FILTERS_TESTS} itkVectorAnisotropicDiffusionImageFilterTest)
ADD_TEST(itkVectorExpandImageFilterTest ${BASIC_FILTERS_TESTS} itkVectorExpandImageFilterTest)
ADD_TEST(itkVectorNeighborhoodOperatorImageFilterTest ${BASIC_FILTERS_TESTS} itkVectorNeighborhoodOperatorImageFilterTest)
ADD_TEST(itkWarpImageFilterTest ${BASIC_FILTERS_TESTS} itkWarpImageFilterTest)
ADD_TEST(itkWrapPadImageTest ${BASIC_FILTERS_TESTS} itkWrapPadImageTest)
ADD_TEST(itkZeroCrossingBasedEdgeDetectionImageFilterTest ${BASIC_FILTERS_TESTS} itkZeroCrossingBasedEdgeDetectionImageFilterTest)
ADD_TEST(itkZeroCrossingImageFilterTest ${BASIC_FILTERS_TESTS} itkZeroCrossingImageFilterTest)

IF(ITK_DATA_ROOT)
ADD_TEST(itkAdaptiveHistogramEqualizationImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AdaptiveHistogramEqualizationImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest.png
  itkAdaptiveHistogramEqualizationImageFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest.png 10 0.5 0.5)

ADD_TEST(itkAdaptiveHistogramEqualizationImageFilterTest2 ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AdaptiveHistogramEqualizationImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest2.png
  itkAdaptiveHistogramEqualizationImageFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest2.png 10 1.0 0.25)

ADD_TEST(itkBilateralImageFilterTest2 ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/BilateralImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest2.png
  itkBilateralImageFilterTest2
            ${ITK_DATA_ROOT}/Input/cake_easy.png
            ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest2.png)

ADD_TEST(itkBilateralImageFilterTest3 ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/BilateralImageFilterTest3.png
            ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest3.png
  itkBilateralImageFilterTest3
            ${ITK_DATA_ROOT}/Input/cake_easy.png
            ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest3.png)

ADD_TEST(itkConfidenceConnectedImageFilterTest ${BASIC_FILTERS_TESTS}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConfidenceConnectedImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/ConfidenceConnectedImageFilterTest.png
  itkConfidenceConnectedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/ConfidenceConnectedImageFilterTest.png)

ADD_TEST(itkConnectedComponentImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest.png
  itkConnectedComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest.png 130 145)

ADD_TEST(itkConnectedThresholdImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedThresholdImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest.png
  itkConnectedThresholdImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest.png)

ADD_TEST(itkDoubleThresholdImageFilterTest ${BASIC_FILTERS_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/DoubleThresholdImageFilterTest.png 
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest.png itkDoubleThresholdImageFilterTest 
            ${ITK_DATA_ROOT}/../../Examples/Data/BrainProtonDensitySlice.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest.png 217 246 255 255)

ADD_TEST(itkDoubleThresholdImageFilterTest2 ${BASIC_FILTERS_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/DoubleThresholdImageFilterTest2.png 
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest2.png itkDoubleThresholdImageFilterTest 
            ${ITK_DATA_ROOT}/../../Examples/Data/BrainProtonDensitySlice.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest2.png 150 164 164 180)

ADD_TEST(itkGradientAnisotropicDiffusionImageFilterTest2 ${BASIC_FILTERS_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GradientAnisotropicDiffusionImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterTest2.png
  itkGradientAnisotropicDiffusionImageFilterTest2
            ${ITK_DATA_ROOT}/Input/cake_easy.png
            ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterTest2.png)

ADD_TEST(itkGrayscaleFillholeImageFilterTest ${BASIC_FILTERS_TESTS} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleFillholeImageFilterTest.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleFillholeImageFilterTest.png itkGrayscaleFillholeImageFilterTest 
               ${ITK_DATA_ROOT}/Input/cthead1.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleFillholeImageFilterTest.png )

ADD_TEST(itkGrayscaleConnectedClosingImageFilterTest ${BASIC_FILTERS_TESTS} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedClosingImageFilterTest.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest.png itkGrayscaleConnectedClosingImageFilterTest 
               ${ITK_DATA_ROOT}/Input/cthead1.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest.png 174 41)

ADD_TEST(itkGrayscaleConnectedClosingImageFilterTest2 ${BASIC_FILTERS_TESTS} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedClosingImageFilterTest2.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest2.png itkGrayscaleConnectedClosingImageFilterTest 
               ${ITK_DATA_ROOT}/Input/cthead1.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest2.png 150 86)

ADD_TEST(itkGrayscaleConnectedOpeningImageFilterTest ${BASIC_FILTERS_TESTS} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedOpeningImageFilterTest.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest.png itkGrayscaleConnectedOpeningImageFilterTest 
               ${ITK_DATA_ROOT}/Input/cthead1.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest.png 174 41)

ADD_TEST(itkGrayscaleConnectedOpeningImageFilterTest2 ${BASIC_FILTERS_TESTS} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedOpeningImageFilterTest2.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest2.png itkGrayscaleConnectedOpeningImageFilterTest 
               ${ITK_DATA_ROOT}/Input/cthead1.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest2.png 150 86)

ADD_TEST(itkHConvexConcaveImageFilterTest ${BASIC_FILTERS_TESTS} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HConvexConcaveImageFilterTest.png 
               ${ITK_TEST_OUTPUT_DIR}/HConvexConcaveImageFilterTest.png itkHConvexConcaveImageFilterTest 
               ${ITK_DATA_ROOT}/Input/cthead1.png 
               ${ITK_TEST_OUTPUT_DIR}/HConvexConcaveImageFilterTest.png 50)

ADD_TEST(itkHMaximaMinimaImageFilterTest ${BASIC_FILTERS_TESTS} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest.png 
               ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest.png itkHMaximaMinimaImageFilterTest 
               ${ITK_DATA_ROOT}/Input/cake_easy.png 
               ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest.png 35)

ADD_TEST(itkHMaximaMinimaImageFilterTest2 ${BASIC_FILTERS_TESTS} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest2.png 
               ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest2.png itkHMaximaMinimaImageFilterTest 
               ${ITK_DATA_ROOT}/Input/cake_hard.png 
               ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest2.png 35)

ADD_TEST(itkIsolatedConnectedImageFilterTest ${BASIC_FILTERS_TESTS}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/IsolatedConnectedImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest.png
  itkIsolatedConnectedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest.png)

ADD_TEST(itkLaplacianRecursiveGaussianImageFilterTest ${BASIC_FILTERS_TESTS}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/LaplacianRecursiveGaussianImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/LaplacianRecursiveGaussianImageFilterTest.png
  itkLaplacianRecursiveGaussianImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/LaplacianRecursiveGaussianImageFilterTest.png)


ADD_TEST(itkNeighborhoodConnectedImageFilterTest ${BASIC_FILTERS_TESTS}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/NeighborhoodConnectedImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/NeighborhoodConnectedImageFilterTest.png
  itkNeighborhoodConnectedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/NeighborhoodConnectedImageFilterTest.png)

ADD_TEST(itkPasteImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PasteImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/PasteImageFilterTest.png
  itkPasteImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_DATA_ROOT}/Input/cake_easy.png
            ${ITK_TEST_OUTPUT_DIR}/PasteImageFilterTest.png)

ADD_TEST(itkPromoteDimensionImageTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PromoteDimensionImageTest.mhd
            ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd
  itkPromoteDimensionImageTest
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd)

ADD_TEST(itkRelabelComponentImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RelabelComponentImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/RelabelComponentImageFilterTest.png
  itkRelabelComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/RelabelComponentImageFilterTest.png 130 145)

ADD_TEST(itkVectorConfidenceConnectedImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorConfidenceConnectedImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest.png
  itkVectorConfidenceConnectedImageFilterTest
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest.png 
            72 117 23 94 4.5 10)

ADD_TEST(itkVectorGradientMagnitudeImageFilterTest1a ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1a.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png
  itkVectorGradientMagnitudeImageFilterTest1
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png 
            0)

ADD_TEST(itkVectorGradientMagnitudeImageFilterTest1b ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1b.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png
  itkVectorGradientMagnitudeImageFilterTest1
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png
            1)

ADD_TEST(itkVectorGradientMagnitudeImageFilterTest2 ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png
  itkVectorGradientMagnitudeImageFilterTest2
            ${ITK_DATA_ROOT}/Input/VHFColor.mhd
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png
            5 0)

ADD_TEST(itkVectorGradientMagnitudeImageFilterTest2b ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2b.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png
  itkVectorGradientMagnitudeImageFilterTest2
            ${ITK_DATA_ROOT}/Input/VHFColor.mhd
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png
            5 1)


ENDIF(ITK_DATA_ROOT)

SET(BasicFilters_SRCS
itkBasicFiltersPrintTest.cxx
itkBasicFiltersPrintTest2.cxx
itkAcosImageFilterAndAdaptorTest.cxx
itkAdaptImageFilterTest.cxx
itkAdaptImageFilterTest2.cxx
itkAdaptiveHistogramEqualizationImageFilterTest.cxx
itkAddImageFilterTest.cxx
itkAsinImageFilterAndAdaptorTest.cxx
itkAtanImageFilterAndAdaptorTest.cxx
itkBSplineDecompositionImageFilterTest.cxx
itkBSplineInterpolateImageFunctionTest.cxx
itkBSplineResampleImageFilterTest.cxx
itkBSplineResampleImageFunctionTest.cxx
itkBasicArchitectureTest.cxx
itkBilateralImageFilterTest.cxx
itkBilateralImageFilterTest2.cxx
itkBilateralImageFilterTest3.cxx
itkBinaryDilateImageFilterTest.cxx
itkBinaryErodeImageFilterTest.cxx
itkBinaryMagnitudeImageFilterTest.cxx
itkBinaryMaskToNarrowBandPointSetFilterTest.cxx
itkBinaryMedianImageFilterTest.cxx
itkBinaryThresholdImageFilterTest.cxx
itkBloxBoundaryPointImageTest.cxx
itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest.cxx
itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest.cxx
itkBloxCoreAtomTest.cxx
itkCannyEdgeDetectionImageFilterTest.cxx
itkChangeInformationImageFilterTest.cxx
itkComposeRGBImageFilterTest.cxx
itkConfidenceConnectedImageFilterTest.cxx
itkConnectedComponentImageFilterTest.cxx
itkConnectedThresholdImageFilterTest.cxx
itkConstantPadImageTest.cxx
itkCosImageFilterAndAdaptorTest.cxx
itkCropImageFilterTest.cxx
itkCurvatureAnisotropicDiffusionImageFilterTest.cxx
itkCyclicReferences.cxx
itkDanielssonDistanceMapImageFilterTest.cxx
itkDerivativeImageFilterTest.cxx
itkDifferenceOfGaussiansGradientTest.cxx
itkDiscreteGaussianImageFilterTest.cxx
itkDivideImageFilterTest.cxx
itkDoubleThresholdImageFilterTest.cxx
itkEdgePotentialImageFilterTest.cxx
itkEigenAnalysis2DImageFilterTest.cxx
itkExpImageFilterAndAdaptorTest.cxx
itkExpNegativeImageFilterAndAdaptorTest.cxx
itkExpandImageFilterTest.cxx
itkExtractImageTest.cxx
itkExtractOrthogonalSwath2DImageFilterTest.cxx
itkFilterDispatchTest.cxx
itkFlipImageFilterTest.cxx
itkFloodFillIteratorTest.cxx
itkGaussianImageSourceTest.cxx
itkGradientAnisotropicDiffusionImageFilterTest.cxx
itkGradientAnisotropicDiffusionImageFilterTest2.cxx
itkGradientImageFilterTest.cxx
itkGradientMagnitudeImageFilterTest.cxx
itkGradientMagnitudeRecursiveGaussianFilterTest.cxx
itkGradientRecursiveGaussianFilterTest.cxx
itkGradientToMagnitudeImageFilterTest.cxx
itkGrayscaleConnectedOpeningImageFilterTest.cxx
itkGrayscaleConnectedClosingImageFilterTest.cxx
itkGrayscaleFillholeImageFilterTest.cxx
itkHardConnectedComponentImageFilterTest.cxx
itkHausdorffDistanceImageFilterTest.cxx
itkHConvexConcaveImageFilterTest.cxx
itkHMaximaMinimaImageFilterTest.cxx
itkHoughTransform2DCirclesImageTest.cxx
itkHoughTransform2DLinesImageTest.cxx
itkImageAdaptorNthElementTest.cxx
itkImageAdaptorPipeLineTest.cxx
itkImageToParametricSpaceFilterTest.cxx
itkImportImageTest.cxx
itkIntensityWindowingImageFilterTest.cxx
itkInteriorExteriorMeshFilterTest.cxx
itkInterpolateImageFilterTest.cxx
itkInterpolateImagePointsFilterTest.cxx
itkIsolatedConnectedImageFilterTest.cxx
itkJoinImageFilterTest.cxx
itkLaplacianImageFilterTest.cxx
itkLaplacianRecursiveGaussianImageFilterTest.cxx
itkLog10ImageFilterAndAdaptorTest.cxx
itkLogImageFilterAndAdaptorTest.cxx
itkMaskImageFilterTest.cxx
itkMathematicalMorphologyImageFilterTest.cxx
itkMaximumImageFilterTest.cxx
itkMeanImageFilterTest.cxx
itkMedianImageFilterTest.cxx
itkMinimumImageFilterTest.cxx
itkMinimumMaximumImageCalculatorTest.cxx
itkMinimumMaximumImageFilterTest.cxx
itkMirrorPadImageTest.cxx
itkMultiplyImageFilterTest.cxx
itkNaryAddImageFilterTest.cxx
itkNeighborhoodConnectedImageFilterTest.cxx
itkNeighborhoodOperatorImageFilterTest.cxx
itkNormalizeImageFilterTest.cxx
itkObjectMorphologyImageFilterTest.cxx
itkPasteImageFilterTest.cxx
itkPathToChainCodePathFilterTest.cxx
itkPathToImageFilterTest.cxx
itkPromoteDimensionImageTest.cxx
itkPermuteAxesImageFilterTest.cxx
itkPlaheImageFilterTest.cxx
itkRGBToVectorAdaptImageFilterTest.cxx
itkRecursiveGaussianImageFiltersTest.cxx
itkReflectImageFilterTest.cxx
itkReflectiveImageRegionIteratorTest.cxx
itkRegionOfInterestImageFilterTest.cxx
itkRelabelComponentImageFilterTest.cxx
itkResampleImageTest.cxx
itkRescaleIntensityImageFilterTest.cxx
itkShiftScaleImageFilterTest.cxx
itkShiftScaleInPlaceImageFilterTest.cxx
itkShrinkImageTest.cxx
itkSigmoidImageFilterTest.cxx
itkSimilarityIndexImageFilterTest.cxx
itkSinImageFilterAndAdaptorTest.cxx
itkSobelEdgeDetectionImageFilterTest.cxx
itkSmoothingRecursiveGaussianImageFilterTest.cxx
itkSparseFieldFourthOrderLevelSetImageFilterTest.cxx
itkSparseFieldLayerTest.cxx
itkSpatialObjectToImageFilterTest.cxx
itkSpatialObjectToImageStatisticsCalculatorTest.cxx
itkSpatialFunctionImageEvaluatorFilterTest.cxx
itkSqrtImageFilterAndAdaptorTest.cxx
itkSquareImageFilterTest.cxx
itkSquaredDifferenceImageFilterTest.cxx
itkStatisticsImageFilterTest.cxx
itkStreamingImageFilterTest.cxx
itkStreamingImageFilterTest2.cxx
itkSubtractImageFilterTest.cxx
itkTanImageFilterAndAdaptorTest.cxx
itkTernaryMagnitudeImageFilterTest.cxx
itkThresholdImageFilterTest.cxx
itkTobogganImageFilterTest.cxx
itkTransformMeshFilterTest.cxx
itkTwoOutputExampleImageFilterTest.cxx
itkVectorAnisotropicDiffusionImageFilterTest.cxx
itkVectorExpandImageFilterTest.cxx
itkVectorGradientMagnitudeImageFilterTest1.cxx
itkVectorGradientMagnitudeImageFilterTest2.cxx
itkVectorNeighborhoodOperatorImageFilterTest.cxx
itkVectorConfidenceConnectedImageFilterTest.cxx
itkWarpImageFilterTest.cxx
itkWrapPadImageTest.cxx
itkZeroCrossingBasedEdgeDetectionImageFilterTest.cxx
itkZeroCrossingImageFilterTest.cxx

itkNarrowBandTest.cxx
itkNarrowBandImageFilterBaseTest.cxx

)


ADD_TEST(itkBasicFiltersHeaderTest ${BASIC_FILTERS_HEADER_TEST})
ADD_EXECUTABLE(itkBasicFiltersHeaderTest itkBasicFiltersHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkBasicFiltersHeaderTest ITKIO ITKNumerics ITKBasicFilters)
ADD_EXECUTABLE(itkBasicFiltersTests itkBasicFiltersTests.cxx ${BasicFilters_SRCS})
TARGET_LINK_LIBRARIES(itkBasicFiltersTests ITKIO ITKNumerics ITKBasicFilters)

