IF( NOT ITK_DISABLE_CXX_TESTING )
IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-BasicFilters ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/BasicFilters)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)


SET(BASIC_FILTERS_TESTS ${CXX_TEST_PATH}/itkBasicFiltersTests)
SET(BASIC_FILTERS_TESTS2 ${CXX_TEST_PATH}/itkBasicFiltersTests2)
SET(BASIC_FILTERS_TESTS3 ${CXX_TEST_PATH}/itkBasicFiltersTests3)
SET(BASIC_FILTERS_HEADER_TEST ${CXX_TEST_PATH}/itkBasicFiltersHeaderTest)
SET(BASELINE ${ITK_DATA_ROOT}/Baseline/BasicFilters)
SET(INPUTDATA ${ITK_DATA_ROOT}/Input)
SET(TEMP ${ITKTesting_BINARY_DIR}/Temporary)

ADD_TEST(itkBasicFiltersPrintTest ${BASIC_FILTERS_TESTS2} itkBasicFiltersPrintTest)
ADD_TEST(itkBasicFiltersPrintTest2 ${BASIC_FILTERS_TESTS2} itkBasicFiltersPrintTest2)

ADD_TEST(itkAcosImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkAcosImageFilterAndAdaptorTest)
ADD_TEST(itkAbsImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkAbsImageFilterAndAdaptorTest)
ADD_TEST(itkAbsoluteValueDifferenceImageFilterTest ${BASIC_FILTERS_TESTS} itkAbsoluteValueDifferenceImageFilterTest)
ADD_TEST(itkAdaptImageFilterTest ${BASIC_FILTERS_TESTS} itkAdaptImageFilterTest)
ADD_TEST(itkAdaptImageFilterTest2 ${BASIC_FILTERS_TESTS} itkAdaptImageFilterTest2)
ADD_TEST(itkAddImageFilterTest ${BASIC_FILTERS_TESTS} itkAddImageFilterTest)
ADD_TEST(itkAndImageFilterTest ${BASIC_FILTERS_TESTS} itkAndImageFilterTest)
ADD_TEST(itkAsinImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkAsinImageFilterAndAdaptorTest)
ADD_TEST(itkAtanImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkAtanImageFilterAndAdaptorTest)
ADD_TEST(itkBSplineDecompositionImageFilterTest ${BASIC_FILTERS_TESTS} itkBSplineDecompositionImageFilterTest)
ADD_TEST(itkBSplineInterpolateImageFunctionTest ${BASIC_FILTERS_TESTS} itkBSplineInterpolateImageFunctionTest)
ADD_TEST(itkBSplineResampleImageFilterTest ${BASIC_FILTERS_TESTS} itkBSplineResampleImageFilterTest)
ADD_TEST(itkBSplineResampleImageFunctionTest ${BASIC_FILTERS_TESTS} itkBSplineResampleImageFunctionTest)
ADD_TEST(itkBasicArchitectureTest ${BASIC_FILTERS_TESTS} itkBasicArchitectureTest)
ADD_TEST(itkBilateralImageFilterTest ${BASIC_FILTERS_TESTS} itkBilateralImageFilterTest)
ADD_TEST(itkBinaryDilateImageFilterTest ${BASIC_FILTERS_TESTS} itkBinaryDilateImageFilterTest)
ADD_TEST(itkBinaryDilateImageFilterTest2 ${BASIC_FILTERS_TESTS} itkBinaryDilateImageFilterTest2)
ADD_TEST(itkBinaryErodeImageFilterTest ${BASIC_FILTERS_TESTS} itkBinaryErodeImageFilterTest)
ADD_TEST(itkBinaryMagnitudeImageFilterTest ${BASIC_FILTERS_TESTS} itkBinaryMagnitudeImageFilterTest)
ADD_TEST(itkBinaryMaskToNarrowBandPointSetFilterTest ${BASIC_FILTERS_TESTS} itkBinaryMaskToNarrowBandPointSetFilterTest)
ADD_TEST(itkBinaryMedianImageFilterTest ${BASIC_FILTERS_TESTS} itkBinaryMedianImageFilterTest)
ADD_TEST(itkVotingBinaryImageFilterTest ${BASIC_FILTERS_TESTS3} itkVotingBinaryImageFilterTest)
ADD_TEST(itkVotingBinaryHoleFillingImageFilterTest ${BASIC_FILTERS_TESTS3} itkVotingBinaryHoleFillingImageFilterTest)
ADD_TEST(itkVotingBinaryIterativeHoleFillingImageFilterTest ${BASIC_FILTERS_TESTS3} itkVotingBinaryIterativeHoleFillingImageFilterTest)
ADD_TEST(itkBinaryThresholdImageFilterTest ${BASIC_FILTERS_TESTS} itkBinaryThresholdImageFilterTest)
ADD_TEST(itkBloxBoundaryPointImageTest ${BASIC_FILTERS_TESTS} itkBloxBoundaryPointImageTest)
ADD_TEST(itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest ${BASIC_FILTERS_TESTS} itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest)
ADD_TEST(itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest ${BASIC_FILTERS_TESTS} itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest)
ADD_TEST(itkBloxCoreAtomTest ${BASIC_FILTERS_TESTS} itkBloxCoreAtomTest)
ADD_TEST(itkCannyEdgeDetectionImageFilterTest ${BASIC_FILTERS_TESTS} itkCannyEdgeDetectionImageFilterTest)
ADD_TEST(itkChainCodeToFourierSeriesPathFilterTest ${BASIC_FILTERS_TESTS} itkChainCodeToFourierSeriesPathFilterTest)
ADD_TEST(itkChangeInformationImageFilterTest ${BASIC_FILTERS_TESTS} itkChangeInformationImageFilterTest)
ADD_TEST(itkCheckerBoardImageFilterTest ${BASIC_FILTERS_TESTS3} itkCheckerBoardImageFilterTest)
ADD_TEST(itkComposeRGBImageFilterTest ${BASIC_FILTERS_TESTS} itkComposeRGBImageFilterTest)
ADD_TEST(itkCompose2DCovariantVectorImageFilterTest ${BASIC_FILTERS_TESTS} itkCompose2DCovariantVectorImageFilterTest)
ADD_TEST(itkCompose3DCovariantVectorImageFilterTest ${BASIC_FILTERS_TESTS} itkCompose3DCovariantVectorImageFilterTest)
ADD_TEST(itkCompose2DVectorImageFilterTest ${BASIC_FILTERS_TESTS} itkCompose2DVectorImageFilterTest)
ADD_TEST(itkCompose3DVectorImageFilterTest ${BASIC_FILTERS_TESTS} itkCompose3DVectorImageFilterTest)
ADD_TEST(itkComplexToRealFilterAndAdaptorTest ${BASIC_FILTERS_TESTS2} itkComplexToRealFilterAndAdaptorTest)
ADD_TEST(itkComplexToImaginaryFilterAndAdaptorTest ${BASIC_FILTERS_TESTS2} itkComplexToImaginaryFilterAndAdaptorTest)
ADD_TEST(itkContourDirectedMeanDistanceImageFilterTest ${BASIC_FILTERS_TESTS2} itkContourDirectedMeanDistanceImageFilterTest)
ADD_TEST(itkContourMeanDistanceImageFilterTest ${BASIC_FILTERS_TESTS2} itkContourMeanDistanceImageFilterTest)
ADD_TEST(itkConstantPadImageTest ${BASIC_FILTERS_TESTS} itkConstantPadImageTest)
ADD_TEST(itkConstrainedValueAdditionImageFilterTest ${BASIC_FILTERS_TESTS2} itkConstrainedValueAdditionImageFilterTest)
ADD_TEST(itkConstrainedValueDifferenceImageFilterTest ${BASIC_FILTERS_TESTS2} itkConstrainedValueDifferenceImageFilterTest)
ADD_TEST(itkCosImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkCosImageFilterAndAdaptorTest)
ADD_TEST(itkCropImageFilterTest ${BASIC_FILTERS_TESTS} itkCropImageFilterTest)
ADD_TEST(itkCurvatureAnisotropicDiffusionImageFilterTest ${BASIC_FILTERS_TESTS} itkCurvatureAnisotropicDiffusionImageFilterTest)
ADD_TEST(itkCyclicReferences ${BASIC_FILTERS_TESTS} itkCyclicReferences)
ADD_TEST(itkDanielssonDistanceMapImageFilterTest ${BASIC_FILTERS_TESTS} itkDanielssonDistanceMapImageFilterTest)
ADD_TEST(itkSignedDanielssonDistanceMapImageFilterTest ${BASIC_FILTERS_TESTS2} itkSignedDanielssonDistanceMapImageFilterTest)
ADD_TEST(itkDerivativeImageFilterTest ${BASIC_FILTERS_TESTS} itkDerivativeImageFilterTest)
ADD_TEST(itkInverseDeformationFieldImageFilterTest ${BASIC_FILTERS_TESTS} itkInverseDeformationFieldImageFilterTest
           ${TEMP}/itkInverseDeformationFieldImageFilterTest.mha
           )
ADD_TEST(itkIterativeInverseDeformationFieldImageFilterTest ${BASIC_FILTERS_TESTS} itkIterativeInverseDeformationFieldImageFilterTest
           ${TEMP}/itkIterativeInverseDeformationFieldImageFilterTest.mha
           )
ADD_TEST(itkDeformationFieldSourceTest ${BASIC_FILTERS_TESTS} itkDeformationFieldSourceTest
           ${TEMP}/itkDeformationFieldSourceTestLandmarks.txt
           ${TEMP}/itkDeformationFieldSourceTest.mha
           )
ADD_TEST(itkDeformationFieldJacobianDeterminantFilterTest ${BASIC_FILTERS_TESTS2} itkDeformationFieldJacobianDeterminantFilterTest)
ADD_TEST(itkDifferenceOfGaussiansGradientTest ${BASIC_FILTERS_TESTS} itkDifferenceOfGaussiansGradientTest)
ADD_TEST(itkDiscreteGaussianImageFilterTest ${BASIC_FILTERS_TESTS} itkDiscreteGaussianImageFilterTest)
ADD_TEST(itkDivideImageFilterTest ${BASIC_FILTERS_TESTS} itkDivideImageFilterTest)
ADD_TEST(itkEdgePotentialImageFilterTest ${BASIC_FILTERS_TESTS} itkEdgePotentialImageFilterTest)
ADD_TEST(itkEigenAnalysis2DImageFilterTest ${BASIC_FILTERS_TESTS} itkEigenAnalysis2DImageFilterTest)
ADD_TEST(itkExpImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkExpImageFilterAndAdaptorTest)
ADD_TEST(itkExpNegativeImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkExpNegativeImageFilterAndAdaptorTest)
ADD_TEST(itkExpandImageFilterTest ${BASIC_FILTERS_TESTS} itkExpandImageFilterTest)
ADD_TEST(itkExtractImageTest ${BASIC_FILTERS_TESTS} itkExtractImageTest)
ADD_TEST(itkFilterDispatchTest ${BASIC_FILTERS_TESTS} itkFilterDispatchTest)
ADD_TEST(itkFlipImageFilterTest ${BASIC_FILTERS_TESTS} itkFlipImageFilterTest)
ADD_TEST(itkFloodFillIteratorTest ${BASIC_FILTERS_TESTS} itkFloodFillIteratorTest)
ADD_TEST(itkGaussianImageSourceTest ${BASIC_FILTERS_TESTS} itkGaussianImageSourceTest)
ADD_TEST(itkGradientAnisotropicDiffusionImageFilterTest ${BASIC_FILTERS_TESTS} itkGradientAnisotropicDiffusionImageFilterTest)
ADD_TEST(itkGradientImageFilterTest ${BASIC_FILTERS_TESTS} itkGradientImageFilterTest)
ADD_TEST(itkGradientMagnitudeImageFilterTest ${BASIC_FILTERS_TESTS} itkGradientMagnitudeImageFilterTest)
ADD_TEST(itkGradientMagnitudeRecursiveGaussianFilterTest ${BASIC_FILTERS_TESTS} itkGradientMagnitudeRecursiveGaussianFilterTest)
ADD_TEST(itkGradientRecursiveGaussianFilterTest ${BASIC_FILTERS_TESTS} itkGradientRecursiveGaussianFilterTest)
ADD_TEST(itkGrayscaleFunctionDilateImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${BASELINE}/itkGrayscaleFunctionDilateImageFilterTest.mha
            ${TEMP}/itkGrayscaleFunctionDilateImageFilterTest.mha
  itkGrayscaleFunctionDilateImageFilterTest
            ${TEMP}/itkGrayscaleFunctionDilateImageFilterTest.mha
)
ADD_TEST(itkGrayscaleFunctionErodeImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${BASELINE}/itkGrayscaleFunctionErodeImageFilterTest.mha
            ${TEMP}/itkGrayscaleFunctionErodeImageFilterTest.mha
  itkGrayscaleFunctionErodeImageFilterTest
            ${TEMP}/itkGrayscaleFunctionErodeImageFilterTest.mha
)

ADD_TEST(itkGradientToMagnitudeImageFilterTest ${BASIC_FILTERS_TESTS} itkGradientToMagnitudeImageFilterTest)
ADD_TEST(itkHardConnectedComponentImageFilterTest ${BASIC_FILTERS_TESTS} itkHardConnectedComponentImageFilterTest)
ADD_TEST(itkHausdorffDistanceImageFilterTest ${BASIC_FILTERS_TESTS} itkHausdorffDistanceImageFilterTest)
ADD_TEST(itkHoughTransform2DCirclesImageTest ${BASIC_FILTERS_TESTS} itkHoughTransform2DCirclesImageTest)
ADD_TEST(itkHoughTransform2DLinesImageTest ${BASIC_FILTERS_TESTS} itkHoughTransform2DLinesImageTest)
ADD_TEST(itkImageAdaptorNthElementTest ${BASIC_FILTERS_TESTS} itkImageAdaptorNthElementTest)
ADD_TEST(itkImageAdaptorPipeLineTest ${BASIC_FILTERS_TESTS} itkImageAdaptorPipeLineTest)
ADD_TEST(itkImageToParametricSpaceFilterTest ${BASIC_FILTERS_TESTS} itkImageToParametricSpaceFilterTest)
ADD_TEST(itkImageToMeshFilterTest ${BASIC_FILTERS_TESTS} itkImageToMeshFilterTest)
ADD_TEST(itkImplicitManifoldNormalVectorFilterTest ${BASIC_FILTERS_TESTS} itkImplicitManifoldNormalVectorFilterTest)
ADD_TEST(itkImportImageTest ${BASIC_FILTERS_TESTS} itkImportImageTest)
ADD_TEST(itkIntensityWindowingImageFilterTest ${BASIC_FILTERS_TESTS} itkIntensityWindowingImageFilterTest )
ADD_TEST(itkInteriorExteriorMeshFilterTest ${BASIC_FILTERS_TESTS} itkInteriorExteriorMeshFilterTest )
ADD_TEST(itkInterpolateImageFilterTest ${BASIC_FILTERS_TESTS} itkInterpolateImageFilterTest )
ADD_TEST(itkInterpolateImagePointsFilterTest ${BASIC_FILTERS_TESTS} itkInterpolateImagePointsFilterTest )
ADD_TEST(itkJoinImageFilterTest ${BASIC_FILTERS_TESTS} itkJoinImageFilterTest)
ADD_TEST(itkJoinSeriesImageFilterTest ${BASIC_FILTERS_TESTS2} itkJoinSeriesImageFilterTest)
ADD_TEST(itkLaplacianImageFilterTest ${BASIC_FILTERS_TESTS} itkLaplacianImageFilterTest)
ADD_TEST(itkLog10ImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkLog10ImageFilterAndAdaptorTest)
ADD_TEST(itkLogImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkLogImageFilterAndAdaptorTest)

ADD_TEST(itkMaskImageFilterTest ${BASIC_FILTERS_TESTS} itkMaskImageFilterTest)
ADD_TEST(itkMaskNegatedImageFilterTest ${BASIC_FILTERS_TESTS} itkMaskNegatedImageFilterTest)
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
ADD_TEST(itkNaryMaximumImageFilterTest ${BASIC_FILTERS_TESTS} itkNaryMaximumImageFilterTest)
ADD_TEST(itkNarrowBandTest ${BASIC_FILTERS_TESTS} itkNarrowBandTest)
ADD_TEST(itkNarrowBandImageFilterBaseTest ${BASIC_FILTERS_TESTS} itkNarrowBandImageFilterBaseTest)
ADD_TEST(itkNeighborhoodOperatorImageFilterTest ${BASIC_FILTERS_TESTS} itkNeighborhoodOperatorImageFilterTest)
ADD_TEST(itkNoiseImageFilterTest ${BASIC_FILTERS_TESTS}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkNoiseImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/itkNoiseImageFilterTest.png
   itkNoiseImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkNoiseImageFilterTest.png)
ADD_TEST(itkNonThreadedShrinkImageTest ${BASIC_FILTERS_TESTS} itkNonThreadedShrinkImageTest)
ADD_TEST(itkNormalizeImageFilterTest ${BASIC_FILTERS_TESTS} itkNormalizeImageFilterTest)
ADD_TEST(itkObjectMorphologyImageFilterTest ${BASIC_FILTERS_TESTS} itkObjectMorphologyImageFilterTest)
ADD_TEST(itkOrImageFilterTest ${BASIC_FILTERS_TESTS} itkOrImageFilterTest)
ADD_TEST(itkParallelSparseFieldLevelSetImageFilterTest ${BASIC_FILTERS_TESTS} itkParallelSparseFieldLevelSetImageFilterTest)
ADD_TEST(itkPathToChainCodePathFilterTest ${BASIC_FILTERS_TESTS} itkPathToChainCodePathFilterTest)
ADD_TEST(itkPathToImageFilterTest ${BASIC_FILTERS_TESTS} itkPathToImageFilterTest)
ADD_TEST(itkPermuteAxesImageFilterTest ${BASIC_FILTERS_TESTS} itkPermuteAxesImageFilterTest)
ADD_TEST(itkRGBToVectorAdaptImageFilterTest.cxx ${BASIC_FILTERS_TESTS} itkRGBToVectorAdaptImageFilterTest)
ADD_TEST(itkRGBToLuminanceImageFilterAndAdaptorTest.cxx ${BASIC_FILTERS_TESTS} itkRGBToLuminanceImageFilterAndAdaptorTest)
ADD_TEST(itkRecursiveGaussianImageFiltersTest ${BASIC_FILTERS_TESTS} itkRecursiveGaussianImageFiltersTest)
ADD_TEST(itkReflectImageFilterTest ${BASIC_FILTERS_TESTS} itkReflectImageFilterTest)
ADD_TEST(itkReflectiveImageRegionIteratorTest ${BASIC_FILTERS_TESTS}  itkReflectiveImageRegionIteratorTest )
ADD_TEST(itkRegionOfInterestImageFilterTest ${BASIC_FILTERS_TESTS}  itkRegionOfInterestImageFilterTest )
ADD_TEST(itkResampleImageTest ${BASIC_FILTERS_TESTS} itkResampleImageTest)
ADD_TEST(itkResamplePhasedArray3DSpecialCoordinatesImageTest ${BASIC_FILTERS_TESTS} itkResamplePhasedArray3DSpecialCoordinatesImageTest)
ADD_TEST(itkRescaleIntensityImageFilterTest ${BASIC_FILTERS_TESTS} itkRescaleIntensityImageFilterTest)
ADD_TEST(itkShiftScaleImageFilterTest ${BASIC_FILTERS_TESTS2} itkShiftScaleImageFilterTest)
ADD_TEST(itkShiftScaleInPlaceImageFilterTest ${BASIC_FILTERS_TESTS2} itkShiftScaleInPlaceImageFilterTest)
ADD_TEST(itkShrinkImageTest ${BASIC_FILTERS_TESTS2} itkShrinkImageTest)
ADD_TEST(itkSigmoidImageFilterTest ${BASIC_FILTERS_TESTS2} itkSigmoidImageFilterTest)
ADD_TEST(itkSimilarityIndexImageFilterTest ${BASIC_FILTERS_TESTS2} itkSimilarityIndexImageFilterTest)
ADD_TEST(itkSimplexMeshToTriangleMeshFilterTest ${BASIC_FILTERS_TESTS2} itkSimplexMeshToTriangleMeshFilterTest)
ADD_TEST(itkSimplexMeshAdaptTopologyFilterTest ${BASIC_FILTERS_TESTS2} itkSimplexMeshAdaptTopologyFilterTest)
ADD_TEST(itkSinImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS2} itkSinImageFilterAndAdaptorTest)
ADD_TEST(itkSobelEdgeDetectionImageFilterTest ${BASIC_FILTERS_TESTS2} itkSobelEdgeDetectionImageFilterTest)
ADD_TEST(itkSmoothingRecursiveGaussianImageFilterTest ${BASIC_FILTERS_TESTS2} itkSmoothingRecursiveGaussianImageFilterTest)
ADD_TEST(itkSparseFieldFourthOrderLevelSetImageFilterTest ${BASIC_FILTERS_TESTS2} itkSparseFieldFourthOrderLevelSetImageFilterTest)
ADD_TEST(itkSparseFieldLayerTest ${BASIC_FILTERS_TESTS2} itkSparseFieldLayerTest)
ADD_TEST(itkSpatialObjectToImageFilterTest ${BASIC_FILTERS_TESTS2} itkSpatialObjectToImageFilterTest)
ADD_TEST(itkSpatialObjectToImageStatisticsCalculatorTest ${BASIC_FILTERS_TESTS2} itkSpatialObjectToImageStatisticsCalculatorTest)
ADD_TEST(itkSpatialObjectToPointSetFilterTest ${BASIC_FILTERS_TESTS2} itkSpatialObjectToPointSetFilterTest)
ADD_TEST(itkSpatialFunctionImageEvaluatorFilterTest ${BASIC_FILTERS_TESTS2} itkSpatialFunctionImageEvaluatorFilterTest)
ADD_TEST(itkSqrtImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS2} itkSqrtImageFilterAndAdaptorTest)
ADD_TEST(itkSquareImageFilterTest ${BASIC_FILTERS_TESTS2} itkSquareImageFilterTest)
ADD_TEST(itkSquaredDifferenceImageFilterTest ${BASIC_FILTERS_TESTS2} itkSquaredDifferenceImageFilterTest)
ADD_TEST(itkStatisticsImageFilterTest ${BASIC_FILTERS_TESTS2} itkStatisticsImageFilterTest)
ADD_TEST(itkStreamingImageFilterTest ${BASIC_FILTERS_TESTS2} itkStreamingImageFilterTest)
ADD_TEST(itkStreamingImageFilterTest2 ${BASIC_FILTERS_TESTS2} itkStreamingImageFilterTest2)
ADD_TEST(itkSubtractImageFilterTest ${BASIC_FILTERS_TESTS2} itkSubtractImageFilterTest)
ADD_TEST(itkTanImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS2} itkTanImageFilterAndAdaptorTest)
ADD_TEST(itkTernaryMagnitudeImageFilterTest ${BASIC_FILTERS_TESTS2} itkTernaryMagnitudeImageFilterTest)
ADD_TEST(itkThresholdImageFilterTest ${BASIC_FILTERS_TESTS2} itkThresholdImageFilterTest)
ADD_TEST(itkThresholdLabelerImageFilterTest ${BASIC_FILTERS_TESTS2} itkThresholdLabelerImageFilterTest)
ADD_TEST(itkTransformMeshFilterTest ${BASIC_FILTERS_TESTS2} itkTransformMeshFilterTest)
ADD_TEST(itkTriangleMeshToSimplexMeshFilterTest ${BASIC_FILTERS_TESTS2} itkTriangleMeshToSimplexMeshFilterTest)
ADD_TEST(itkTriangleMeshToSimplexMeshFilter2Test ${BASIC_FILTERS_TESTS2} itkTriangleMeshToSimplexMeshFilter2Test)
ADD_TEST(itkTwoOutputExampleImageFilterTest ${BASIC_FILTERS_TESTS2} itkTwoOutputExampleImageFilterTest)
ADD_TEST(itkVectorAnisotropicDiffusionImageFilterTest ${BASIC_FILTERS_TESTS3} itkVectorAnisotropicDiffusionImageFilterTest)
ADD_TEST(itkVectorExpandImageFilterTest ${BASIC_FILTERS_TESTS3} itkVectorExpandImageFilterTest)
ADD_TEST(itkVectorNeighborhoodOperatorImageFilterTest ${BASIC_FILTERS_TESTS3} itkVectorNeighborhoodOperatorImageFilterTest)
ADD_TEST(itkVectorRescaleIntensityImageFilterTest ${BASIC_FILTERS_TESTS3} itkVectorRescaleIntensityImageFilterTest)
ADD_TEST(itkWarpImageFilterTest ${BASIC_FILTERS_TESTS3} itkWarpImageFilterTest)
ADD_TEST(itkWarpMeshFilterTest ${BASIC_FILTERS_TESTS3} itkWarpMeshFilterTest)
ADD_TEST(itkWarpVectorImageFilterTest ${BASIC_FILTERS_TESTS3} itkWarpVectorImageFilterTest)
ADD_TEST(itkWeightedAddImageFilterTest ${BASIC_FILTERS_TESTS3} itkWeightedAddImageFilterTest)
ADD_TEST(itkWrapPadImageTest ${BASIC_FILTERS_TESTS3} itkWrapPadImageTest)
ADD_TEST(itkXorImageFilterTest ${BASIC_FILTERS_TESTS3} itkXorImageFilterTest)
ADD_TEST(itkZeroCrossingBasedEdgeDetectionImageFilterTest ${BASIC_FILTERS_TESTS3} itkZeroCrossingBasedEdgeDetectionImageFilterTest)
ADD_TEST(itkZeroCrossingImageFilterTest ${BASIC_FILTERS_TESTS3} itkZeroCrossingImageFilterTest)

ADD_TEST(itkOrientImageFilterTest ${BASIC_FILTERS_TESTS} itkOrientImageFilterTest)
ADD_TEST(itkOrientImageFilterTest2 ${BASIC_FILTERS_TESTS} itkOrientImageFilterTest2)

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

ADD_TEST(itkBinaryThresholdImageFilterTest2 ${BASIC_FILTERS_TESTS}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/BinaryThresholdImageFilterTest2.png
             ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png
  itkBinaryThresholdImageFilterTest2
             ${ITK_DATA_ROOT}/Input/cthead1.png  ${ITK_DATA_ROOT}/Input/DicomSeries/Image0076.dcm
             ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png )

ADD_TEST(itkConfidenceConnectedImageFilterTest ${BASIC_FILTERS_TESTS}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConfidenceConnectedImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/ConfidenceConnectedImageFilterTest.png
  itkConfidenceConnectedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/ConfidenceConnectedImageFilterTest.png
             165 165)

ADD_TEST(itkConnectedComponentImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest.png
  itkConnectedComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest.png 130 145)

ADD_TEST(itkConnectedComponentImageFilterTest2 ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest2.png
  itkConnectedComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/Shapes.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest2.png 128 255 0)

ADD_TEST(itkConnectedComponentImageFilterTest3 ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest3.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest3.png
  itkConnectedComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/Shapes.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest3.png 128 255 1)

ADD_TEST(itkConnectedThresholdImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedThresholdImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest.png
  itkConnectedThresholdImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest.png
            165 165)

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

ADD_TEST(itkGrayscaleMorphologicalClosingImageFilterTest ${BASIC_FILTERS_TESTS2} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleMorphologicalClosingImageFilterTest.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalClosingImageFilterTest.png 
   itkGrayscaleMorphologicalClosingImageFilterTest 
               ${ITK_DATA_ROOT}/Input/cthead1.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalClosingImageFilterTest.png )

ADD_TEST(itkGrayscaleMorphologicalOpeningImageFilterTest ${BASIC_FILTERS_TESTS2} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleMorphologicalOpeningImageFilterTest.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalOpeningImageFilterTest.png 
   itkGrayscaleMorphologicalOpeningImageFilterTest 
               ${ITK_DATA_ROOT}/Input/cthead1.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalOpeningImageFilterTest.png )

ADD_TEST(itkSimpleContourExtractorImageFilterTest ${BASIC_FILTERS_TESTS2} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/SimpleContourExtractorImageFilterTest.png 
               ${ITK_TEST_OUTPUT_DIR}/SimpleContourExtractorImageFilterTest.png 
   itkSimpleContourExtractorImageFilterTest 
               ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png 
               ${ITK_TEST_OUTPUT_DIR}/SimpleContourExtractorImageFilterTest.png )

ADD_TEST(itkGrayscaleFillholeImageFilterTest ${BASIC_FILTERS_TESTS} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleFillholeImageFilterTest.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleFillholeImageFilterTest.png itkGrayscaleFillholeImageFilterTest 
               ${ITK_DATA_ROOT}/Input/cthead1.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleFillholeImageFilterTest.png )

ADD_TEST(itkGrayscaleConnectedClosingImageFilterTest ${BASIC_FILTERS_TESTS} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedClosingImageFilterTest.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest.png itkGrayscaleConnectedClosingImageFilterTest 
               ${ITK_DATA_ROOT}/Input/cthead1.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest.png
               174 214)

ADD_TEST(itkGrayscaleConnectedClosingImageFilterTest2 ${BASIC_FILTERS_TESTS} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedClosingImageFilterTest2.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest2.png itkGrayscaleConnectedClosingImageFilterTest 
               ${ITK_DATA_ROOT}/Input/cthead1.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest2.png
               150 169)

ADD_TEST(itkGrayscaleConnectedOpeningImageFilterTest ${BASIC_FILTERS_TESTS} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedOpeningImageFilterTest.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest.png itkGrayscaleConnectedOpeningImageFilterTest 
               ${ITK_DATA_ROOT}/Input/cthead1.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest.png 174 214)

ADD_TEST(itkGrayscaleConnectedOpeningImageFilterTest2 ${BASIC_FILTERS_TESTS} 
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedOpeningImageFilterTest2.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest2.png itkGrayscaleConnectedOpeningImageFilterTest 
               ${ITK_DATA_ROOT}/Input/cthead1.png 
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest2.png 150 169)

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
             ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest.png
             true
             115 85 107 110)

ADD_TEST(itkIsolatedConnectedImageFilterTest2 ${BASIC_FILTERS_TESTS}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/IsolatedConnectedImageFilterTest2.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest2.png
  itkIsolatedConnectedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest2.png
             false
             175 125 100 170 176 125 101 170)

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
             ${ITK_TEST_OUTPUT_DIR}/NeighborhoodConnectedImageFilterTest.png
             146 167)

ADD_TEST(itkExtractOrthogonalSwath2DImageFilterTest ${BASIC_FILTERS_TESTS} 
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ExtractOrthogonalSwath2DImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/ExtractOrthogonalSwath2DImageFilterTest.png
  itkExtractOrthogonalSwath2DImageFilterTest 
            ${ITK_TEST_OUTPUT_DIR}/ExtractOrthogonalSwath2DImageFilterTest.png
)

ADD_TEST(itkTileImageFilterTest ${BASIC_FILTERS_TESTS2}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/TileImageFilterTest4.png
            ${ITK_TEST_OUTPUT_DIR}/TileImageFilterTest4.png
  itkTileImageFilterTest
            1 2 0
            ${ITK_DATA_ROOT}/Input/STAPLE1.png
            ${ITK_DATA_ROOT}/Input/STAPLE2.png
            ${ITK_DATA_ROOT}/Input/STAPLE3.png
            ${ITK_DATA_ROOT}/Input/STAPLE4.png
            ${ITK_DATA_ROOT}/Input/Shapes.png
            ${ITK_DATA_ROOT}/Input/Spots.png
            ${ITK_DATA_ROOT}/Input/SpotsInverted.png
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSliceInitialLevelSet.png
            ${ITK_DATA_ROOT}/Input/cake_easy.png
            ${ITK_DATA_ROOT}/Input/cake_hard.png
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${ITK_DATA_ROOT}/Input/smooth_circle.png
            ${ITK_DATA_ROOT}/Input/smooth_square.png
            ${ITK_TEST_OUTPUT_DIR}/TileImageFilterTest%d.png)

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

ADD_TEST(itkRemoveBoundaryObjectsTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RemoveBoundaryObjectsTest.png
            ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest.png
  itkRemoveBoundaryObjectsTest
            ${ITK_DATA_ROOT}/Input/Spots.png
            ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest.png)

ADD_TEST(itkRemoveBoundaryObjectsTest2 ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RemoveBoundaryObjectsTest2.png
            ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest2.png
  itkRemoveBoundaryObjectsTest2
            ${ITK_DATA_ROOT}/Input/SpotsInverted.png
            ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest2.png)

ADD_TEST(itkTobogganImageFilterTest ${BASIC_FILTERS_TESTS2}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/TobogganImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/TobogganImageFilterTest.png
         itkTobogganImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/TobogganImageFilterTest.png)

ADD_TEST(itkVectorConfidenceConnectedImageFilterTest ${BASIC_FILTERS_TESTS3}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorConfidenceConnectedImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest.png
  itkVectorConfidenceConnectedImageFilterTest
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest.png 
            72 44 23 67 5.0 6)

ADD_TEST(itkVectorGradientMagnitudeImageFilterTest1a ${BASIC_FILTERS_TESTS3}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1a.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png
  itkVectorGradientMagnitudeImageFilterTest1
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png 
            0)

ADD_TEST(itkVectorGradientMagnitudeImageFilterTest1b ${BASIC_FILTERS_TESTS3}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1b.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png
  itkVectorGradientMagnitudeImageFilterTest1
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png
            1)

ADD_TEST(itkVectorGradientMagnitudeImageFilterTest2 ${BASIC_FILTERS_TESTS3}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png
  itkVectorGradientMagnitudeImageFilterTest2
            ${ITK_DATA_ROOT}/Input/VHFColor.mhd
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png
            5 0)

ADD_TEST(itkVectorGradientMagnitudeImageFilterTest2b ${BASIC_FILTERS_TESTS3}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2b.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png
  itkVectorGradientMagnitudeImageFilterTest2
            ${ITK_DATA_ROOT}/Input/VHFColor.mhd
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png
            5 1)

ADD_TEST(itkVectorResampleImageFilterTest ${BASIC_FILTERS_TESTS3}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorResampleImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png
  itkVectorResampleImageFilterTest
            ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png)

ENDIF(ITK_DATA_ROOT)

SET(BasicFilters_SRCS
itkAcosImageFilterAndAdaptorTest.cxx
itkAbsImageFilterAndAdaptorTest.cxx
itkAbsoluteValueDifferenceImageFilterTest.cxx
itkAdaptImageFilterTest.cxx
itkAdaptImageFilterTest2.cxx
itkAdaptiveHistogramEqualizationImageFilterTest.cxx
itkAddImageFilterTest.cxx
itkAndImageFilterTest.cxx
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
itkBinaryDilateImageFilterTest2.cxx
itkBinaryErodeImageFilterTest.cxx
itkBinaryMagnitudeImageFilterTest.cxx
itkBinaryMaskToNarrowBandPointSetFilterTest.cxx
itkBinaryMedianImageFilterTest.cxx
itkBinaryThresholdImageFilterTest.cxx
itkBinaryThresholdImageFilterTest2.cxx
itkBloxBoundaryPointImageTest.cxx
itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest.cxx
itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest.cxx
itkBloxCoreAtomTest.cxx
itkCannyEdgeDetectionImageFilterTest.cxx
itkChainCodeToFourierSeriesPathFilterTest.cxx
itkChangeInformationImageFilterTest.cxx
itkComposeRGBImageFilterTest.cxx
itkCompose2DCovariantVectorImageFilterTest.cxx
itkCompose3DCovariantVectorImageFilterTest.cxx
itkCompose2DVectorImageFilterTest.cxx
itkCompose3DVectorImageFilterTest.cxx
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
itkDeformationFieldSourceTest.cxx
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
itkGrayscaleFunctionDilateImageFilterTest.cxx
itkGrayscaleFunctionErodeImageFilterTest.cxx
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
itkImageToMeshFilterTest.cxx 
itkImplicitManifoldNormalVectorFilterTest.cxx
itkImportImageTest.cxx
itkIntensityWindowingImageFilterTest.cxx
itkInteriorExteriorMeshFilterTest.cxx
itkInterpolateImageFilterTest.cxx
itkInterpolateImagePointsFilterTest.cxx
itkInverseDeformationFieldImageFilterTest.cxx
itkIterativeInverseDeformationFieldImageFilterTest.cxx
itkIsolatedConnectedImageFilterTest.cxx
itkJoinImageFilterTest.cxx
itkLaplacianImageFilterTest.cxx
itkLaplacianRecursiveGaussianImageFilterTest.cxx
itkLog10ImageFilterAndAdaptorTest.cxx
itkLogImageFilterAndAdaptorTest.cxx
itkMaskImageFilterTest.cxx
itkMaskNegatedImageFilterTest.cxx
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
itkNaryMaximumImageFilterTest.cxx
itkNeighborhoodConnectedImageFilterTest.cxx
itkNeighborhoodOperatorImageFilterTest.cxx
itkNoiseImageFilterTest.cxx
itkNonThreadedShrinkImageTest.cxx
itkNormalizeImageFilterTest.cxx
itkOrImageFilterTest.cxx
itkOrientImageFilterTest.cxx
itkOrientImageFilterTest2.cxx
itkObjectMorphologyImageFilterTest.cxx
itkParallelSparseFieldLevelSetImageFilterTest.cxx
itkPasteImageFilterTest.cxx
itkPathToChainCodePathFilterTest.cxx
itkPathToImageFilterTest.cxx
itkPromoteDimensionImageTest.cxx
itkPermuteAxesImageFilterTest.cxx
itkRGBToVectorAdaptImageFilterTest.cxx
itkRGBToLuminanceImageFilterAndAdaptorTest.cxx
itkRecursiveGaussianImageFiltersTest.cxx
itkReflectImageFilterTest.cxx
itkReflectiveImageRegionIteratorTest.cxx
itkRegionOfInterestImageFilterTest.cxx
itkRelabelComponentImageFilterTest.cxx
itkRemoveBoundaryObjectsTest.cxx
itkRemoveBoundaryObjectsTest2.cxx
itkResampleImageTest.cxx
itkResamplePhasedArray3DSpecialCoordinatesImageTest.cxx
itkRescaleIntensityImageFilterTest.cxx
itkNarrowBandTest.cxx
itkNarrowBandImageFilterBaseTest.cxx

)

SET(BasicFilters2_SRCS
itkComplexToRealFilterAndAdaptorTest.cxx
itkComplexToImaginaryFilterAndAdaptorTest.cxx
itkContourDirectedMeanDistanceImageFilterTest.cxx
itkContourMeanDistanceImageFilterTest.cxx
itkConstrainedValueAdditionImageFilterTest.cxx
itkConstrainedValueDifferenceImageFilterTest.cxx
itkBasicFiltersPrintTest.cxx
itkBasicFiltersPrintTest2.cxx
itkDeformationFieldJacobianDeterminantFilterTest.cxx 
itkGrayscaleMorphologicalClosingImageFilterTest.cxx
itkGrayscaleMorphologicalOpeningImageFilterTest.cxx
itkJoinSeriesImageFilterTest.cxx
itkSimpleContourExtractorImageFilterTest.cxx
itkShiftScaleImageFilterTest.cxx
itkShiftScaleInPlaceImageFilterTest.cxx
itkShrinkImageTest.cxx
itkSigmoidImageFilterTest.cxx
itkSimilarityIndexImageFilterTest.cxx
itkSimplexMeshToTriangleMeshFilterTest.cxx
itkSimplexMeshAdaptTopologyFilterTest.cxx
itkSignedDanielssonDistanceMapImageFilterTest.cxx
itkSinImageFilterAndAdaptorTest.cxx
itkSobelEdgeDetectionImageFilterTest.cxx
itkSmoothingRecursiveGaussianImageFilterTest.cxx
itkSparseFieldFourthOrderLevelSetImageFilterTest.cxx
itkSparseFieldLayerTest.cxx
itkSpatialObjectToImageFilterTest.cxx
itkSpatialObjectToImageStatisticsCalculatorTest.cxx
itkSpatialObjectToPointSetFilterTest.cxx
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
itkThresholdLabelerImageFilterTest.cxx
itkTileImageFilterTest.cxx
itkTobogganImageFilterTest.cxx
itkTransformMeshFilterTest.cxx
itkTriangleMeshToSimplexMeshFilterTest.cxx
itkTriangleMeshToSimplexMeshFilter2Test.cxx
itkTwoOutputExampleImageFilterTest.cxx
)

SET(BasicFilters3_SRCS
itkCheckerBoardImageFilterTest.cxx
itkVectorAnisotropicDiffusionImageFilterTest.cxx
itkVectorExpandImageFilterTest.cxx
itkVectorGradientMagnitudeImageFilterTest1.cxx
itkVectorGradientMagnitudeImageFilterTest2.cxx
itkVectorNeighborhoodOperatorImageFilterTest.cxx
itkVectorRescaleIntensityImageFilterTest.cxx 
itkVectorResampleImageFilterTest.cxx 
itkVectorConfidenceConnectedImageFilterTest.cxx
itkVotingBinaryImageFilterTest.cxx
itkVotingBinaryHoleFillingImageFilterTest.cxx
itkVotingBinaryIterativeHoleFillingImageFilterTest.cxx
itkWarpImageFilterTest.cxx
itkWarpMeshFilterTest.cxx
itkWarpVectorImageFilterTest.cxx
itkWeightedAddImageFilterTest.cxx
itkWrapPadImageTest.cxx
itkXorImageFilterTest.cxx
itkZeroCrossingBasedEdgeDetectionImageFilterTest.cxx
itkZeroCrossingImageFilterTest.cxx
)

ADD_TEST(itkBasicFiltersHeaderTest ${BASIC_FILTERS_HEADER_TEST})
ADD_EXECUTABLE(itkBasicFiltersHeaderTest itkBasicFiltersHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkBasicFiltersHeaderTest ITKIO ITKNumerics ITKBasicFilters)
ADD_EXECUTABLE(itkBasicFiltersTests itkBasicFiltersTests.cxx ${BasicFilters_SRCS})
TARGET_LINK_LIBRARIES(itkBasicFiltersTests ITKIO ITKNumerics ITKBasicFilters)
ADD_EXECUTABLE(itkBasicFiltersTests2 itkBasicFiltersTests2.cxx ${BasicFilters2_SRCS})
TARGET_LINK_LIBRARIES(itkBasicFiltersTests2 ITKIO ITKNumerics ITKBasicFilters)
ADD_EXECUTABLE(itkBasicFiltersTests3 itkBasicFiltersTests3.cxx ${BasicFilters3_SRCS})
TARGET_LINK_LIBRARIES(itkBasicFiltersTests3 ITKIO ITKNumerics ITKBasicFilters)

ENDIF( NOT ITK_DISABLE_CXX_TESTING )


IF( NOT ITK_DISABLE_TCL_TESTING )
IF(ITK_CSWIG_TCL)

  ADD_TEST(CannyEdgeDetectionImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CannyEdgeDetectionImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CannyEdgeDetectionImageFilterTclTest.png 
           5 0.125)

  ADD_TEST(CastImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CastImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CastImageFilterTclTest.png )

  ADD_TEST(CurvatureFlowImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CurvatureFlowImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CurvatureFlowImageFilterTclTest.png 
           5 0.125)

  ADD_TEST(CurvatureAnisotropicDiffusionImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CurvatureAnisotropicDiffusionImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CurvatureAnisotropicDiffusionImageFilterTclTest.png 
           5 0.125 3.0)

  ADD_TEST(GradientAnisotropicDiffusionImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/GradientAnisotropicDiffusionImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterTclTest.png 
           5 0.125 3.0)

 ADD_TEST(BinaryDilateImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/BinaryDilateImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryDilateImageFilterTclTest.png )

  ADD_TEST(BinaryErodeImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/BinaryErodeImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryErodeImageFilterTclTest.png )

  ADD_TEST(MeanImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/MeanImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/MeanImageFilterTclTest.png
           2 )

  ADD_TEST(MedianImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/MedianImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/MedianImageFilterTclTest.png
           1 )

  ADD_TEST(SigmoidImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/SigmoidImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/SigmoidImageFilterTclTest.png
           0 255  1.0  128  )

  ADD_TEST(ThresholdImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/ThresholdImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/ThresholdImageFilterTclTest.png
           0 150 )

  ADD_TEST(BinaryThresholdImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/BinaryThresholdImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTclTest.png
           0 255 150 180)

ENDIF(ITK_CSWIG_TCL)
ENDIF( NOT ITK_DISABLE_TCL_TESTING )



IF( NOT ITK_DISABLE_PYTHON_TESTING )
IF(ITK_CSWIG_PYTHON)

  ADD_TEST(CannyEdgeDetectionImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CannyEdgeDetectionImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CannyEdgeDetectionImageFilterPythonTest.png  
           5 0.125)

  ADD_TEST(CastImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CastImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CastImageFilterPythonTest.png )

  ADD_TEST(CurvatureFlowImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CurvatureFlowImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CurvatureFlowImageFilterPythonTest.png 
           5 0.125)

  ADD_TEST(CurvatureAnisotropicDiffusionImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CurvatureAnisotropicDiffusionImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CurvatureAnisotropicDiffusionImageFilterPythonTest.png 
           5 0.125 3.0)

  ADD_TEST(GradientAnisotropicDiffusionImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/GradientAnisotropicDiffusionImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterPythonTest.png 
           5 0.125 3.0)

 ADD_TEST(BinaryErodeImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/BinaryErodeImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryErodeImageFilterPythonTest.png )

  ADD_TEST(BinaryDilateImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/BinaryDilateImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryDilateImageFilterPythonTest.png )

 ADD_TEST(LaplacianImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/LaplacianImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/LaplacianImageFilterPythonTest.png )

 ADD_TEST(MeanImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/MeanImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/MeanImageFilterPythonTest.png
           2 )

  ADD_TEST(MedianImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/MedianImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/MedianImageFilterPythonTest.png
           1 )

  ADD_TEST(SigmoidImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/SigmoidImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/SigmoidImageFilterPythonTest.png
           0 255  1.0  128  )

  ADD_TEST(ThresholdImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/ThresholdImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/ThresholdImageFilterPythonTest.png
           0 150 )

  ADD_TEST(BinaryThresholdImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/BinaryThresholdImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterPythonTest.png
           0 255 150 180)

ENDIF(ITK_CSWIG_PYTHON)
ENDIF( NOT ITK_DISABLE_PYTHON_TESTING )

IF( NOT ITK_DISABLE_JAVA_TESTING )
IF(ITK_CSWIG_JAVA AND ITK_BINARY_DIR)
  SET(JAVACLASSPATH ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit.jar)
  FOREACH(cfgtype ${CMAKE_CONFIGURATION_TYPES})
    SET(JAVACLASSPATH "${JAVACLASSPATH}\\;${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${cfgtype}/InsightToolkit.jar")
  ENDFOREACH(cfgtype)

  FOREACH( javatestfile 
     BinaryThresholdImageFilter
     BinaryDilateImageFilter
     BinaryErodeImageFilter
     CannyEdgeDetectionImageFilter
     CastImageFilter
     CurvatureFlowImageFilter
     CurvatureAnisotropicDiffusionImageFilter
     GradientAnisotropicDiffusionImageFilter
     MeanImageFilter
     MedianImageFilter
     ThresholdImageFilter
     SigmoidImageFilter
     )

  ADD_CUSTOM_TARGET(${javatestfile}Java ALL  DEPENDS ${ITK_BINARY_DIR}/Examples/Filtering/${javatestfile}.class)

  ADD_CUSTOM_COMMAND(
    OUTPUT  ${ITK_BINARY_DIR}/Examples/Filtering/${javatestfile}.class
    DEPENDS ${ITK_SOURCE_DIR}/Examples/Filtering/${javatestfile}.java
    COMMAND ${JAVA_COMPILE}
    ARGS -classpath "${JAVACLASSPATH}" -d "${ITK_BINARY_DIR}/Examples/Filtering"
    ${ITK_SOURCE_DIR}/Examples/Filtering/${javatestfile}.java
    COMMENT "Java Class")

  ENDFOREACH( javatestfile )

  IF(WIN32)
    SET(TMPJAVACLASSPATH ${ITK_BINARY_DIR}/Examples/Filtering/)
    FOREACH(javadirectory ${JAVACLASSPATH})
      # Set path separator for Windows
      SET(TMPJAVACLASSPATH "${TMPJAVACLASSPATH}\\;${javadirectory}")
    ENDFOREACH(javadirectory)
    SET(JAVACLASSPATH ${TMPJAVACLASSPATH})
  ELSE(WIN32)
    SET(TMPJAVACLASSPATH ${ITK_BINARY_DIR}/Examples/Filtering/)
    FOREACH(javadirectory ${JAVACLASSPATH})
      # Set path separator for UNIX
      SET(TMPJAVACLASSPATH "${TMPJAVACLASSPATH}:${javadirectory}")
    ENDFOREACH(javadirectory)
    SET(JAVACLASSPATH ${TMPJAVACLASSPATH})
  ENDIF(WIN32)
 
  ADD_TEST(MeanImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           MeanImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/MeanImageFilterJavaTest.png  
           1 )  

  ADD_TEST(MedianImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           MedianImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/MedianImageFilterJavaTest.png  
           1 )  

  ADD_TEST(CannyEdgeDetectionImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           CannyEdgeDetectionImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CannyEdgeDetectionImageFilterJavaTest.png 
           5 0.125 )  

  ADD_TEST(CastImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           CastImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CastImageFilterJavaTest.png )  

 ADD_TEST(CurvatureFlowImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           CurvatureFlowImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CurvatureFlowImageFilterJavaTest.png 
           5 0.125 )  

  ADD_TEST(CurvatureAnisotropicDiffusionImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           CurvatureAnisotropicDiffusionImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CurvatureAnisotropicDiffusionImageFilterJavaTest.png 
           5 0.125 3.0)  

  ADD_TEST(GradientAnisotropicDiffusionImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           GradientAnisotropicDiffusionImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterJavaTest.png 
           5 0.125 3.0)  

  ADD_TEST(ThresholdImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           ThresholdImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/ThresholdImageFilterJavaTest.png  
           0 150)  

  ADD_TEST(BinaryThresholdImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           BinaryThresholdImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterJavaTest.png 
           150 180 0 255 )  

  ADD_TEST(BinaryDilateImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           BinaryDilateImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryDilateImageFilterJavaTest.png )  

  ADD_TEST(BinaryErodeImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           BinaryErodeImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryErodeImageFilterJavaTest.png )  

  ADD_TEST(SigmoidImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           SigmoidImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/SigmoidImageFilterJavaTest.png  
           0 255  1.0  128  )

ENDIF(ITK_CSWIG_JAVA AND ITK_BINARY_DIR)
ENDIF( NOT ITK_DISABLE_JAVA_TESTING )

