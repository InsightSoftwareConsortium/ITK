include_regular_expression("^.*$")

if( NOT ITK_DISABLE_CXX_TESTING )
if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-BasicFilters ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/BasicFilters)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)


# Define some convenient locations
set(BASIC_FILTERS_HEADER_TEST ${CXX_TEST_PATH}/itkBasicFiltersHeaderTest)
set(BASELINE ${ITK_DATA_ROOT}/Baseline/BasicFilters)
set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(TEMP ${ITKTesting_BINARY_DIR}/Temporary)

############################################################
# CXX Testing
#
set(KIT BasicFilters)
set(KIT_LIBS ITKAlgorithms ITKFEM ITKNumerics ITKStatistics)
set(KitTests
  itkAbsImageFilterAndAdaptorTest
  itkAbsoluteValueDifferenceImageFilterTest
  itkAccumulateImageFilterTest
  itkAcosImageFilterAndAdaptorTest
  itkAdaptImageFilterTest
  itkAdaptImageFilterTest2
  itkAdaptiveHistogramEqualizationImageFilterTest
  itkAddImageFilterTest
  itkAndImageFilterTest
  itkApproximateSignedDistanceMapImageFilterTest
  itkAsinImageFilterAndAdaptorTest
  itkAtan2ImageFilterTest
  itkAtanImageFilterAndAdaptorTest
  itkBSplineDecompositionImageFilterTest
  itkBSplineDownsampleImageFilterTest
  itkBSplineInterpolateImageFunctionTest
  itkBSplineResampleImageFilterTest
  itkBSplineResampleImageFunctionTest
  itkBSplineUpsampleImageFilterTest
  itkBasicArchitectureTest
  itkBasicFiltersPrintTest
  itkBasicFiltersPrintTest2
  itkBilateralImageFilterTest
  itkBilateralImageFilterTest2
  itkBilateralImageFilterTest3
  itkBinaryDilateImageFilterTest
  itkBinaryDilateImageFilterTest2
  itkBinaryDilateImageFilterTest3
  itkBinaryErodeImageFilterTest
  itkBinaryErodeImageFilterTest3
  itkBinaryMagnitudeImageFilterTest
  itkBinaryMaskToNarrowBandPointSetFilterTest
  itkBinaryMedianImageFilterTest
  itkBinaryProjectionImageFilterTest
  itkBinaryThresholdImageFilterTest
  itkBinaryThresholdImageFilterTest2
  itkBinaryThresholdProjectionImageFilterTest
  itkBloxBoundaryPointImageTest
  itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest
  itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest
  itkBloxCoreAtomTest
  itkCannyEdgeDetectionImageFilterTest
  itkCannyEdgeDetectionImageFilterTest2
  itkChainCodeToFourierSeriesPathFilterTest
  itkChangeInformationImageFilterTest
  itkChangeLabelImageFilterTest
  itkCheckerBoardImageFilterTest
  itkClosingByReconstructionImageFilterTest
  itkComplexToImaginaryFilterAndAdaptorTest
  itkComplexToModulusFilterAndAdaptorTest
  itkComplexToPhaseFilterAndAdaptorTest
  itkComplexToRealFilterAndAdaptorTest
  itkCompose2DCovariantVectorImageFilterTest
  itkCompose2DVectorImageFilterTest
  itkCompose3DCovariantVectorImageFilterTest
  itkCompose3DVectorImageFilterTest
  itkComposeRGBImageFilterTest
  itkConfidenceConnectedImageFilterTest
  itkConnectedComponentImageFilterTest
  itkConnectedComponentImageFilterTestRGB
  itkConnectedComponentImageFilterTooManyObjectsTest
  itkConnectedThresholdImageFilterTest
  itkConstantPadImageTest
  itkConstrainedValueAdditionImageFilterTest
  itkConstrainedValueDifferenceImageFilterTest
  itkContourDirectedMeanDistanceImageFilterTest
  itkContourMeanDistanceImageFilterTest
  itkCosImageFilterAndAdaptorTest
  itkCropImageFilterTest
  itkCurvatureAnisotropicDiffusionImageFilterTest
  itkCyclicReferences
  itkDanielssonDistanceMapImageFilterTest
  itkDeformationFieldJacobianDeterminantFilterTest
  itkDeformationFieldSourceTest
  itkDerivativeImageFilterTest
  itkDifferenceOfGaussiansGradientTest
  itkDiffusionTensor3DReconstructionImageFilterTest
  itkDiscreteGaussianImageFilterTest
  itkDiscreteGaussianImageFilterTest2
  itkDisplacementFieldJacobianDeterminantFilterTest
  itkDivideImageFilterTest
  itkDivideImageFilterTest2
  itkDoubleThresholdImageFilterTest
  itkEdgePotentialImageFilterTest
  itkEigenAnalysis2DImageFilterTest
  itkExpImageFilterAndAdaptorTest
  itkExpNegativeImageFilterAndAdaptorTest
  itkExpandImageFilterTest
  itkExtractImageTest
  itkExtractImage3Dto2DTest
  itkExtractOrthogonalSwath2DImageFilterTest
  itkFilterDispatchTest
  itkFlipImageFilterTest
  itkFloodFillIteratorTest
  itkGaussianImageSourceTest
  itkGetAverageSliceImageFilterTest
  itkGradientAnisotropicDiffusionImageFilterTest
  itkGradientAnisotropicDiffusionImageFilterTest2
  itkGradientImageFilterTest
  itkGradientMagnitudeImageFilterTest
  itkGradientMagnitudeRecursiveGaussianFilterTest
  itkGradientRecursiveGaussianFilterTest
  itkGradientRecursiveGaussianFilterTest2
  itkGradientToMagnitudeImageFilterTest
  itkGrayscaleConnectedClosingImageFilterTest
  itkGrayscaleConnectedOpeningImageFilterTest
  itkGrayscaleFillholeImageFilterTest
  itkGrayscaleFunctionDilateImageFilterTest
  itkGrayscaleFunctionErodeImageFilterTest
  itkGrayscaleGeodesicErodeDilateImageFilterTest
  itkGrayscaleMorphologicalClosingImageFilterTest
  itkGrayscaleMorphologicalOpeningImageFilterTest
  itkHConvexConcaveImageFilterTest
  itkHMaximaMinimaImageFilterTest
  itkHardConnectedComponentImageFilterTest
  itkHausdorffDistanceImageFilterTest
  itkHessian3DToVesselnessMeasureImageFilterTest
  itkHessianRecursiveGaussianFilterScaleSpaceTest
  itkHessianRecursiveGaussianFilterTest
  itkHoughTransform2DCirclesImageTest
  itkHoughTransform2DLinesImageTest
  itkImageAdaptorNthElementTest
  itkImageAdaptorPipeLineTest
  itkImageToMeshFilterTest
  itkImageToParametricSpaceFilterTest
  itkImageToVectorImageFilterTest
  itkImplicitManifoldNormalVectorFilterTest
  itkImportImageTest
  itkIntensityWindowingImageFilterTest
  itkInteriorExteriorMeshFilterTest
  itkInterpolateImageFilterTest
  itkInterpolateImagePointsFilterTest
  itkInverseDeformationFieldImageFilterTest
  itkInvertIntensityImageFilterTest
  itkIsolatedConnectedImageFilterTest
  itkIterativeInverseDeformationFieldImageFilterTest
  itkJoinImageFilterTest
  itkJoinSeriesImageFilterStreamingTest
  itkJoinSeriesImageFilterTest
  itkLabelStatisticsImageFilterTest
  itkLaplacianImageFilterTest
  itkLaplacianRecursiveGaussianImageFilterTest
  itkLog10ImageFilterAndAdaptorTest
  itkLogImageFilterAndAdaptorTest
  itkMaskConnectedComponentImageFilterTest
  itkMaskImageFilterTest
  itkMaskNegatedImageFilterTest
  itkMaskNeighborhoodOperatorImageFilterTest
  itkMathematicalMorphologyImageFilterTest
  itkMatrixIndexSelectionImageFilterTest
  itkMaximumImageFilterTest
  itkMaximumProjectionImageFilterTest
  itkMaximumProjectionImageFilterTest2
  itkMaximumProjectionImageFilterTest3
  itkMeanImageFilterTest
  itkMeanProjectionImageFilterTest
  itkMedianImageFilterTest
  itkMedianProjectionImageFilterTest
  itkMinimumImageFilterTest
  itkMinimumMaximumImageCalculatorTest
  itkMinimumMaximumImageFilterTest
  itkMinimumProjectionImageFilterTest
  itkMirrorPadImageTest
  itkModulusImageFilterTest
  itkMorphologicalGradientImageFilterTest
  itkMultiplyImageFilterTest
  itkNarrowBandImageFilterBaseTest
  itkNarrowBandTest
  itkNaryAddImageFilterTest
  itkNaryMaximumImageFilterTest
  itkNeighborhoodConnectedImageFilterTest
  itkNeighborhoodOperatorImageFilterTest
  itkNoiseImageFilterTest
  itkNonThreadedShrinkImageTest
  itkNormalizeImageFilterTest
  itkNormalizedCorrelationImageFilterTest
  itkNotImageFilterTest
  itkObjectMorphologyImageFilterTest
  itkOpeningByReconstructionImageFilterTest
  itkOrImageFilterTest
  itkOrientImageFilterTest
  itkOrientImageFilterTest2
  itkParallelSparseFieldLevelSetImageFilterTest
  itkParametricSpaceToImageSpaceMeshFilterTest
  itkPasteImageFilterTest
  itkPathToChainCodePathFilterTest
  itkPathToImageFilterTest
  itkPermuteAxesImageFilterTest
  itkPolylineMask2DImageFilterTest
  itkPolylineMaskImageFilterTest
  itkProjectionImageFilterTest
  itkPromoteDimensionImageTest
  itkPushPopTileImageFilterTest
  itkRGBToLuminanceImageFilterAndAdaptorTest
  itkRGBToVectorAdaptImageFilterTest
  itkRecursiveGaussianImageFiltersOnTensorsTest
  itkRecursiveGaussianImageFiltersOnVectorImageTest
  itkRecursiveGaussianImageFiltersTest
  itkRecursiveGaussianScaleSpaceTest1
  itkReflectImageFilterTest
  itkReflectiveImageRegionIteratorTest
  itkRegionOfInterestImageFilterTest
  itkRelabelComponentImageFilterTest
  itkReleaseDataFilterTest
  itkRemoveBoundaryObjectsTest
  itkRemoveBoundaryObjectsTest2
  itkResampleImageTest
  itkResampleImageTest2
  itkResamplePhasedArray3DSpecialCoordinatesImageTest
  itkRescaleIntensityImageFilterTest
  itkScalarConnectedComponentImageFilterTest
  itkShiftScaleImageFilterTest
  itkShiftScaleInPlaceImageFilterTest
  itkShrinkImagePreserveObjectPhysicalLocations
  itkShrinkImageStreamingTest
  itkShrinkImageTest
  itkSigmoidImageFilterTest
  itkSignedDanielssonDistanceMapImageFilterTest
  itkSignedMaurerDistanceMapImageFilterTest
  itkSimilarityIndexImageFilterTest
  itkSimpleContourExtractorImageFilterTest
  itkSimplexMeshAdaptTopologyFilterTest
  itkSimplexMeshToTriangleMeshFilterTest
  itkSimplexMeshWithFloatCoordRepTest
  itkSinImageFilterAndAdaptorTest
  itkSmoothingRecursiveGaussianImageFilterTest
  itkSobelEdgeDetectionImageFilterTest
  itkSparseFieldFourthOrderLevelSetImageFilterTest
  itkSparseFieldLayerTest
  itkSpatialFunctionImageEvaluatorFilterTest
  itkSpatialObjectToImageFilterTest
  itkSpatialObjectToImageStatisticsCalculatorTest
  itkSpatialObjectToPointSetFilterTest
  itkSqrtImageFilterAndAdaptorTest
  itkSquareImageFilterTest
  itkSquaredDifferenceImageFilterTest
  itkStandardDeviationProjectionImageFilterTest
  itkStatisticsImageFilterTest
  itkStreamingImageFilterTest
  itkStreamingImageFilterTest2
  itkStreamingImageFilterTest3
  itkSubtractImageFilterTest
  itkSumProjectionImageFilterTest
  itkSymmetricEigenAnalysisImageFilterTest
  itkTanImageFilterAndAdaptorTest
  itkTensorFractionalAnisotropyImageFilterTest
  itkTensorRelativeAnisotropyImageFilterTest
  itkTernaryMagnitudeImageFilterTest
  itkTernaryMagnitudeSquaredImageFilterTest
  itkThresholdImageFilterTest
  itkThresholdLabelerImageFilterTest
  itkTileImageFilterTest
  itkTobogganImageFilterTest
  itkTopHatImageFilterTest
  itkTransformMeshFilterTest
  itkTriangleMeshToBinaryImageFilterTest
  itkTriangleMeshToBinaryImageFilterTest2
  itkTriangleMeshToSimplexMeshFilter2Test
  itkTriangleMeshToSimplexMeshFilterTest
  itkTwoOutputExampleImageFilterTest
  itkVectorAnisotropicDiffusionImageFilterTest
  itkVectorConfidenceConnectedImageFilterTest
  itkVectorConnectedComponentImageFilterTest
  itkVectorExpandImageFilterTest
  itkVectorGradientMagnitudeImageFilterTest1
  itkVectorGradientMagnitudeImageFilterTest2
  itkVectorGradientMagnitudeImageFilterTest3
  itkVectorIndexSelectionCastImageFilterTest
  itkVectorNeighborhoodOperatorImageFilterTest
  itkVectorResampleImageFilterTest
  itkVectorRescaleIntensityImageFilterTest
  itkVotingBinaryHoleFillingImageFilterTest
  itkVotingBinaryImageFilterTest
  itkVotingBinaryIterativeHoleFillingImageFilterTest
  itkWarpImageFilterTest
  itkWarpImageFilterTest2
  itkWarpMeshFilterTest
  itkWarpVectorImageFilterTest
  itkWeightedAddImageFilterTest
  itkWrapPadImageTest
  itkXorImageFilterTest
  itkZeroCrossingBasedEdgeDetectionImageFilterTest
  itkZeroCrossingImageFilterTest
)

# Given KIT, KIT_LIBS and KitTests, build TestDriver
CreateTestDriver(${KIT} "${KIT_LIBS}" "${KitTests}" TestDriver)

#############################################################
# Tests that use the Test Driver
#
add_test(itkBasicFiltersPrintTest ${TestDriver} itkBasicFiltersPrintTest)
add_test(itkBasicFiltersPrintTest2 ${TestDriver} itkBasicFiltersPrintTest2)

add_test(itkAcosImageFilterAndAdaptorTest ${TestDriver} itkAcosImageFilterAndAdaptorTest)
add_test(itkAbsImageFilterAndAdaptorTest ${TestDriver} itkAbsImageFilterAndAdaptorTest)
add_test(itkAbsoluteValueDifferenceImageFilterTest ${TestDriver} itkAbsoluteValueDifferenceImageFilterTest)
add_test(itkAdaptImageFilterTest ${TestDriver} itkAdaptImageFilterTest)
add_test(itkAdaptImageFilterTest2 ${TestDriver} itkAdaptImageFilterTest2)
add_test(itkAddImageFilterTest ${TestDriver} itkAddImageFilterTest)
add_test(itkAndImageFilterTest ${TestDriver} itkAndImageFilterTest)
add_test(itkApproximateSignedDistanceMapImageFilterTest ${TestDriver}
  --compare ${BASELINE}/itkApproximateSignedDistanceMapImageFilterTest.png
            ${TEMP}/itkApproximateSignedDistanceMapImageFilterTest.png
  itkApproximateSignedDistanceMapImageFilterTest
             ${TEMP}/itkApproximateSignedDistanceMapImageFilterTest.png)
add_test(itkAsinImageFilterAndAdaptorTest ${TestDriver} itkAsinImageFilterAndAdaptorTest)
add_test(itkAtanImageFilterAndAdaptorTest ${TestDriver} itkAtanImageFilterAndAdaptorTest)
add_test(itkAtan2ImageFilterTest ${TestDriver} itkAtan2ImageFilterTest)
add_test(itkBSplineDecompositionImageFilterTest ${TestDriver} itkBSplineDecompositionImageFilterTest)
add_test(itkBSplineInterpolateImageFunctionTest ${TestDriver} itkBSplineInterpolateImageFunctionTest)
add_test(itkBSplineResampleImageFilterTest ${TestDriver} itkBSplineResampleImageFilterTest)
add_test(itkBSplineResampleImageFunctionTest ${TestDriver} itkBSplineResampleImageFunctionTest)
add_test(itkBasicArchitectureTest ${TestDriver} itkBasicArchitectureTest)
add_test(itkBilateralImageFilterTest ${TestDriver} itkBilateralImageFilterTest)
add_test(itkBinaryDilateImageFilterTest ${TestDriver} itkBinaryDilateImageFilterTest)
add_test(itkBinaryDilateImageFilterTest2 ${TestDriver} itkBinaryDilateImageFilterTest2)

add_test(itkBinaryErodeImageFilterTest ${TestDriver} itkBinaryErodeImageFilterTest)
add_test(itkBinaryMagnitudeImageFilterTest ${TestDriver} itkBinaryMagnitudeImageFilterTest)
add_test(itkBinaryMaskToNarrowBandPointSetFilterTest ${TestDriver} itkBinaryMaskToNarrowBandPointSetFilterTest)
add_test(itkBinaryMedianImageFilterTest ${TestDriver} itkBinaryMedianImageFilterTest)
add_test(itkVotingBinaryImageFilterTest ${TestDriver} itkVotingBinaryImageFilterTest)
add_test(itkVotingBinaryHoleFillingImageFilterTest ${TestDriver} itkVotingBinaryHoleFillingImageFilterTest)
add_test(itkVotingBinaryIterativeHoleFillingImageFilterTest ${TestDriver} itkVotingBinaryIterativeHoleFillingImageFilterTest)
add_test(itkBinaryThresholdImageFilterTest ${TestDriver} itkBinaryThresholdImageFilterTest)
add_test(itkBloxBoundaryPointImageTest ${TestDriver} itkBloxBoundaryPointImageTest)
add_test(itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest ${TestDriver} itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest)
add_test(itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest ${TestDriver} itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest)
add_test(itkBloxCoreAtomTest ${TestDriver} itkBloxCoreAtomTest)
add_test(itkChainCodeToFourierSeriesPathFilterTest ${TestDriver} itkChainCodeToFourierSeriesPathFilterTest)
add_test(itkChangeInformationImageFilterTest ${TestDriver} itkChangeInformationImageFilterTest)
add_test(itkChangeLabelImageFilterTest ${TestDriver} itkChangeLabelImageFilterTest)
add_test(itkCheckerBoardImageFilterTest ${TestDriver} itkCheckerBoardImageFilterTest)
add_test(itkComposeRGBImageFilterTest ${TestDriver} itkComposeRGBImageFilterTest)
add_test(itkCompose2DCovariantVectorImageFilterTest ${TestDriver} itkCompose2DCovariantVectorImageFilterTest)
add_test(itkCompose3DCovariantVectorImageFilterTest ${TestDriver} itkCompose3DCovariantVectorImageFilterTest)
add_test(itkCompose2DVectorImageFilterTest ${TestDriver} itkCompose2DVectorImageFilterTest)
add_test(itkCompose3DVectorImageFilterTest ${TestDriver} itkCompose3DVectorImageFilterTest)
add_test(itkComplexToRealFilterAndAdaptorTest ${TestDriver} itkComplexToRealFilterAndAdaptorTest)
add_test(itkComplexToImaginaryFilterAndAdaptorTest ${TestDriver} itkComplexToImaginaryFilterAndAdaptorTest)
add_test(itkComplexToModulusFilterAndAdaptorTest ${TestDriver} itkComplexToModulusFilterAndAdaptorTest)
add_test(itkComplexToPhaseFilterAndAdaptorTest ${TestDriver} itkComplexToPhaseFilterAndAdaptorTest)
add_test(itkContourDirectedMeanDistanceImageFilterTest ${TestDriver} itkContourDirectedMeanDistanceImageFilterTest)
add_test(itkContourMeanDistanceImageFilterTest ${TestDriver} itkContourMeanDistanceImageFilterTest)
add_test(itkConstantPadImageTest ${TestDriver} itkConstantPadImageTest)
add_test(itkConstrainedValueAdditionImageFilterTest ${TestDriver} itkConstrainedValueAdditionImageFilterTest)
add_test(itkConstrainedValueDifferenceImageFilterTest ${TestDriver} itkConstrainedValueDifferenceImageFilterTest)
add_test(itkCosImageFilterAndAdaptorTest ${TestDriver} itkCosImageFilterAndAdaptorTest)
add_test(itkCropImageFilterTest ${TestDriver} itkCropImageFilterTest)
add_test(itkCurvatureAnisotropicDiffusionImageFilterTest ${TestDriver} itkCurvatureAnisotropicDiffusionImageFilterTest)
add_test(itkCyclicReferences ${TestDriver} itkCyclicReferences)
add_test(itkDanielssonDistanceMapImageFilterTest ${TestDriver} itkDanielssonDistanceMapImageFilterTest)
add_test(itkSignedDanielssonDistanceMapImageFilterTest ${TestDriver} itkSignedDanielssonDistanceMapImageFilterTest)
add_test(itkDiffusionTensor3DReconstructionImageFilterTest ${TestDriver} itkDiffusionTensor3DReconstructionImageFilterTest)
add_test(itkInverseDeformationFieldImageFilterTest ${TestDriver} itkInverseDeformationFieldImageFilterTest
           ${TEMP}/itkInverseDeformationFieldImageFilterTest.mha
           )
add_test(itkIterativeInverseDeformationFieldImageFilterTest ${TestDriver} itkIterativeInverseDeformationFieldImageFilterTest
           ${TEMP}/itkIterativeInverseDeformationFieldImageFilterTest.mha
           )
add_test(itkDeformationFieldSourceTest ${TestDriver} itkDeformationFieldSourceTest
           ${TEMP}/itkDeformationFieldSourceTestLandmarks.txt
           ${TEMP}/itkDeformationFieldSourceTest.mha
           )
add_test(itkDeformationFieldJacobianDeterminantFilterTest ${TestDriver} itkDeformationFieldJacobianDeterminantFilterTest)
add_test(itkDisplacementFieldJacobianDeterminantFilterTest ${TestDriver} itkDisplacementFieldJacobianDeterminantFilterTest)
add_test(itkDifferenceOfGaussiansGradientTest ${TestDriver} itkDifferenceOfGaussiansGradientTest)
add_test(itkDiscreteGaussianImageFilterTest ${TestDriver} itkDiscreteGaussianImageFilterTest)
add_test(itkDivideImageFilterTest ${TestDriver} itkDivideImageFilterTest)
add_test(itkEdgePotentialImageFilterTest ${TestDriver} itkEdgePotentialImageFilterTest)
add_test(itkEigenAnalysis2DImageFilterTest ${TestDriver} itkEigenAnalysis2DImageFilterTest)
add_test(itkExpImageFilterAndAdaptorTest ${TestDriver} itkExpImageFilterAndAdaptorTest)
add_test(itkExpNegativeImageFilterAndAdaptorTest ${TestDriver} itkExpNegativeImageFilterAndAdaptorTest)
add_test(itkExpandImageFilterTest ${TestDriver} itkExpandImageFilterTest)
add_test(itkExtractImageTest ${TestDriver} itkExtractImageTest)
add_test(itkExtractImage3Dto2DTest ${TestDriver} itkExtractImage3Dto2DTest)
add_test(itkFilterDispatchTest ${TestDriver} itkFilterDispatchTest)
add_test(itkFlipImageFilterTest ${TestDriver} itkFlipImageFilterTest)
add_test(itkFloodFillIteratorTest ${TestDriver} itkFloodFillIteratorTest)
add_test(itkGaussianImageSourceTest ${TestDriver} itkGaussianImageSourceTest)
add_test(itkGradientAnisotropicDiffusionImageFilterTest ${TestDriver} itkGradientAnisotropicDiffusionImageFilterTest)
add_test(itkGradientImageFilterTest ${TestDriver} itkGradientImageFilterTest)
add_test(itkGradientMagnitudeImageFilterTest ${TestDriver} itkGradientMagnitudeImageFilterTest)
add_test(itkGradientMagnitudeRecursiveGaussianFilterTest ${TestDriver} itkGradientMagnitudeRecursiveGaussianFilterTest)
add_test(itkGradientRecursiveGaussianFilterTest ${TestDriver} itkGradientRecursiveGaussianFilterTest)
add_test(itkGradientRecursiveGaussianFilterTest2 ${TestDriver} itkGradientRecursiveGaussianFilterTest2)
add_test(itkGrayscaleFunctionDilateImageFilterTest ${TestDriver}
  --compare ${BASELINE}/itkGrayscaleFunctionDilateImageFilterTest.mha
            ${TEMP}/itkGrayscaleFunctionDilateImageFilterTest.mha
  itkGrayscaleFunctionDilateImageFilterTest
            ${TEMP}/itkGrayscaleFunctionDilateImageFilterTest.mha
)
add_test(itkGrayscaleFunctionErodeImageFilterTest ${TestDriver}
  --compare ${BASELINE}/itkGrayscaleFunctionErodeImageFilterTest.mha
            ${TEMP}/itkGrayscaleFunctionErodeImageFilterTest.mha
  itkGrayscaleFunctionErodeImageFilterTest
            ${TEMP}/itkGrayscaleFunctionErodeImageFilterTest.mha
)

add_test(itkGradientToMagnitudeImageFilterTest ${TestDriver} itkGradientToMagnitudeImageFilterTest)
add_test(itkHardConnectedComponentImageFilterTest ${TestDriver} itkHardConnectedComponentImageFilterTest)
add_test(itkHausdorffDistanceImageFilterTest ${TestDriver} itkHausdorffDistanceImageFilterTest)
add_test(itkHessianRecursiveGaussianFilterTest ${TestDriver} itkHessianRecursiveGaussianFilterTest)
add_test(itkHessianRecursiveGaussianFilterScaleSpaceTest ${TestDriver} itkHessianRecursiveGaussianFilterScaleSpaceTest)
add_test(itkHessian3DToVesselnessMeasureImageFilterTest ${TestDriver} itkHessian3DToVesselnessMeasureImageFilterTest)

add_test(itkSymmetricEigenAnalysisImageFilterTest ${TestDriver} itkSymmetricEigenAnalysisImageFilterTest)
add_test(itkTensorFractionalAnisotropyImageFilterTest ${TestDriver} itkTensorFractionalAnisotropyImageFilterTest)
add_test(itkTensorRelativeAnisotropyImageFilterTest ${TestDriver} itkTensorRelativeAnisotropyImageFilterTest)
add_test(itkHoughTransform2DCirclesImageTest ${TestDriver} itkHoughTransform2DCirclesImageTest)
add_test(itkHoughTransform2DLinesImageTest ${TestDriver} itkHoughTransform2DLinesImageTest)
add_test(itkImageAdaptorNthElementTest ${TestDriver} itkImageAdaptorNthElementTest)
add_test(itkImageAdaptorPipeLineTest ${TestDriver} itkImageAdaptorPipeLineTest)
add_test(itkImageToParametricSpaceFilterTest ${TestDriver} itkImageToParametricSpaceFilterTest)
add_test(itkImageToMeshFilterTest ${TestDriver} itkImageToMeshFilterTest)
add_test(itkImplicitManifoldNormalVectorFilterTest ${TestDriver} itkImplicitManifoldNormalVectorFilterTest)
add_test(itkImportImageTest ${TestDriver} itkImportImageTest)
add_test(itkIntensityWindowingImageFilterTest ${TestDriver} itkIntensityWindowingImageFilterTest )
add_test(itkInteriorExteriorMeshFilterTest ${TestDriver} itkInteriorExteriorMeshFilterTest )
add_test(itkInterpolateImageFilterTest ${TestDriver} itkInterpolateImageFilterTest )
add_test(itkInterpolateImagePointsFilterTest ${TestDriver} itkInterpolateImagePointsFilterTest )
add_test(itkJoinImageFilterTest ${TestDriver} itkJoinImageFilterTest)
add_test(itkJoinSeriesImageFilterTest ${TestDriver} itkJoinSeriesImageFilterTest)
add_test(itkLaplacianImageFilterTest ${TestDriver} itkLaplacianImageFilterTest)
add_test(itkLog10ImageFilterAndAdaptorTest ${TestDriver} itkLog10ImageFilterAndAdaptorTest)
add_test(itkLogImageFilterAndAdaptorTest ${TestDriver} itkLogImageFilterAndAdaptorTest)

add_test(itkMaskImageFilterTest ${TestDriver} itkMaskImageFilterTest)
add_test(itkPolylineMaskImageFilterTest ${TestDriver} itkPolylineMaskImageFilterTest)

add_test(itkMaskNegatedImageFilterTest ${TestDriver} itkMaskNegatedImageFilterTest)
add_test(itkMathematicalMorphologyImageFilterTest ${TestDriver} itkMathematicalMorphologyImageFilterTest)
add_test(itkMaximumImageFilterTest ${TestDriver} itkMaximumImageFilterTest)
add_test(itkMeanImageFilterTest ${TestDriver} itkMeanImageFilterTest)
add_test(itkMedianImageFilterTest ${TestDriver} itkMedianImageFilterTest)
add_test(itkMinimumImageFilterTest ${TestDriver} itkMinimumImageFilterTest)
add_test(itkMinimumMaximumImageCalculatorTest ${TestDriver} itkMinimumMaximumImageCalculatorTest)
add_test(itkMinimumMaximumImageFilterTest ${TestDriver} itkMinimumMaximumImageFilterTest)
add_test(itkMirrorPadImageTest ${TestDriver} itkMirrorPadImageTest)
add_test(itkMultiplyImageFilterTest ${TestDriver} itkMultiplyImageFilterTest)
add_test(itkNaryAddImageFilterTest ${TestDriver} itkNaryAddImageFilterTest)
add_test(itkNaryMaximumImageFilterTest ${TestDriver} itkNaryMaximumImageFilterTest)
add_test(itkNarrowBandTest ${TestDriver} itkNarrowBandTest)
#
# The next test uses itkRandomSource which does not produce deterministic
# results on 64 bit systems. No regression is performed for 64 bit systems.
#
if(${CMAKE_SIZEOF_LONG} EQUAL 4)
  add_test(itkNarrowBandImageFilterBaseTest ${TestDriver}
    --compare ${BASELINE}/itkNarrowBandImageFilterBaseTest.png
              ${TEMP}/itkNarrowBandImageFilterBaseTest.png
    --compareNumberOfPixelsTolerance 5
    itkNarrowBandImageFilterBaseTest
               ${TEMP}/itkNarrowBandImageFilterBaseTest.png)
else(${CMAKE_SIZEOF_LONG} EQUAL 4)
  add_test(itkNarrowBandImageFilterBaseTest ${TestDriver}
    itkNarrowBandImageFilterBaseTest
               ${TEMP}/itkNarrowBandImageFilterBaseTest.png)
endif(${CMAKE_SIZEOF_LONG} EQUAL 4)

add_test(itkNeighborhoodOperatorImageFilterTest ${TestDriver} itkNeighborhoodOperatorImageFilterTest)
add_test(itkNonThreadedShrinkImageTest ${TestDriver} itkNonThreadedShrinkImageTest)
add_test(itkNormalizeImageFilterTest ${TestDriver} itkNormalizeImageFilterTest)
add_test(itkObjectMorphologyImageFilterTest ${TestDriver} itkObjectMorphologyImageFilterTest)
add_test(itkOrImageFilterTest ${TestDriver} itkOrImageFilterTest)
add_test(itkPathToChainCodePathFilterTest ${TestDriver} itkPathToChainCodePathFilterTest)
add_test(itkPathToImageFilterTest ${TestDriver} itkPathToImageFilterTest)
add_test(itkPermuteAxesImageFilterTest ${TestDriver} itkPermuteAxesImageFilterTest)
add_test(itkRGBToVectorAdaptImageFilterTest ${TestDriver} itkRGBToVectorAdaptImageFilterTest)
add_test(itkRGBToLuminanceImageFilterAndAdaptorTest ${TestDriver} itkRGBToLuminanceImageFilterAndAdaptorTest)
add_test(itkRecursiveGaussianImageFiltersTest ${TestDriver} itkRecursiveGaussianImageFiltersTest)
add_test(itkRecursiveGaussianImageFiltersOnTensorsTest ${TestDriver} itkRecursiveGaussianImageFiltersOnTensorsTest)
add_test(itkRecursiveGaussianImageFiltersOnVectorImageTest ${TestDriver} itkRecursiveGaussianImageFiltersOnVectorImageTest)
add_test(itkRecursiveGaussianScaleSpaceTest1 ${TestDriver} itkRecursiveGaussianScaleSpaceTest1)
add_test(itkReflectImageFilterTest ${TestDriver} itkReflectImageFilterTest)
add_test(itkReflectiveImageRegionIteratorTest ${TestDriver}  itkReflectiveImageRegionIteratorTest )
add_test(itkRegionOfInterestImageFilterTest ${TestDriver}  itkRegionOfInterestImageFilterTest )
add_test(itkResampleImageTest ${TestDriver} itkResampleImageTest)
add_test(itkResamplePhasedArray3DSpecialCoordinatesImageTest ${TestDriver} itkResamplePhasedArray3DSpecialCoordinatesImageTest)
add_test(itkRescaleIntensityImageFilterTest ${TestDriver} itkRescaleIntensityImageFilterTest)
add_test(itkShiftScaleImageFilterTest ${TestDriver} itkShiftScaleImageFilterTest)
add_test(itkShiftScaleInPlaceImageFilterTest ${TestDriver} itkShiftScaleInPlaceImageFilterTest)
add_test(itkShrinkImageTest ${TestDriver} itkShrinkImageTest)
add_test(itkShrinkImagePreserveObjectPhysicalLocations ${TestDriver} itkShrinkImagePreserveObjectPhysicalLocations)
add_test(itkShrinkImageStreamingTest ${TestDriver} itkShrinkImageStreamingTest)
add_test(itkSigmoidImageFilterTest ${TestDriver} itkSigmoidImageFilterTest)
add_test(itkSimilarityIndexImageFilterTest ${TestDriver} itkSimilarityIndexImageFilterTest)
add_test(itkSimplexMeshToTriangleMeshFilterTest ${TestDriver} itkSimplexMeshToTriangleMeshFilterTest)
add_test(itkSimplexMeshAdaptTopologyFilterTest ${TestDriver} itkSimplexMeshAdaptTopologyFilterTest)
add_test(itkSinImageFilterAndAdaptorTest ${TestDriver} itkSinImageFilterAndAdaptorTest)
add_test(itkSobelEdgeDetectionImageFilterTest ${TestDriver} itkSobelEdgeDetectionImageFilterTest)
add_test(itkSmoothingRecursiveGaussianImageFilterTest ${TestDriver} itkSmoothingRecursiveGaussianImageFilterTest)
add_test(itkSparseFieldFourthOrderLevelSetImageFilterTest ${TestDriver} itkSparseFieldFourthOrderLevelSetImageFilterTest)
add_test(itkSparseFieldLayerTest ${TestDriver} itkSparseFieldLayerTest)
add_test(itkSpatialObjectToImageFilterTest ${TestDriver} itkSpatialObjectToImageFilterTest)
add_test(itkSpatialObjectToImageStatisticsCalculatorTest ${TestDriver} itkSpatialObjectToImageStatisticsCalculatorTest)
add_test(itkSpatialObjectToPointSetFilterTest ${TestDriver} itkSpatialObjectToPointSetFilterTest)
add_test(itkSpatialFunctionImageEvaluatorFilterTest ${TestDriver} itkSpatialFunctionImageEvaluatorFilterTest)
add_test(itkSqrtImageFilterAndAdaptorTest ${TestDriver} itkSqrtImageFilterAndAdaptorTest)
add_test(itkSquareImageFilterTest ${TestDriver} itkSquareImageFilterTest)
add_test(itkSquaredDifferenceImageFilterTest ${TestDriver} itkSquaredDifferenceImageFilterTest)
add_test(itkStatisticsImageFilterTest ${TestDriver} itkStatisticsImageFilterTest)
add_test(itkStreamingImageFilterTest ${TestDriver} itkStreamingImageFilterTest)
add_test(itkStreamingImageFilterTest2 ${TestDriver} itkStreamingImageFilterTest2)
add_test(itkSubtractImageFilterTest ${TestDriver} itkSubtractImageFilterTest)
add_test(itkTanImageFilterAndAdaptorTest ${TestDriver} itkTanImageFilterAndAdaptorTest)
add_test(itkTernaryMagnitudeImageFilterTest ${TestDriver} itkTernaryMagnitudeImageFilterTest)
add_test(itkTernaryMagnitudeSquaredImageFilterTest ${TestDriver} itkTernaryMagnitudeSquaredImageFilterTest)
add_test(itkThresholdImageFilterTest ${TestDriver} itkThresholdImageFilterTest)
add_test(itkThresholdLabelerImageFilterTest ${TestDriver} itkThresholdLabelerImageFilterTest)
add_test(itkTransformMeshFilterTest ${TestDriver} itkTransformMeshFilterTest)
add_test(itkTriangleMeshToSimplexMeshFilterTest ${TestDriver} itkTriangleMeshToSimplexMeshFilterTest)
add_test(itkTriangleMeshToSimplexMeshFilter2Test ${TestDriver} itkTriangleMeshToSimplexMeshFilter2Test)
add_test(itkTwoOutputExampleImageFilterTest ${TestDriver} itkTwoOutputExampleImageFilterTest)
add_test(itkVectorAnisotropicDiffusionImageFilterTest ${TestDriver} itkVectorAnisotropicDiffusionImageFilterTest)
add_test(itkVectorExpandImageFilterTest ${TestDriver} itkVectorExpandImageFilterTest)
add_test(itkVectorNeighborhoodOperatorImageFilterTest ${TestDriver} itkVectorNeighborhoodOperatorImageFilterTest)
add_test(itkVectorRescaleIntensityImageFilterTest ${TestDriver} itkVectorRescaleIntensityImageFilterTest)
add_test(itkWarpImageFilterTest ${TestDriver} itkWarpImageFilterTest)
add_test(itkWarpMeshFilterTest ${TestDriver} itkWarpMeshFilterTest)
add_test(itkWarpVectorImageFilterTest ${TestDriver} itkWarpVectorImageFilterTest)
add_test(itkWeightedAddImageFilterTest ${TestDriver} itkWeightedAddImageFilterTest)
add_test(itkWrapPadImageTest ${TestDriver} itkWrapPadImageTest)
add_test(itkXorImageFilterTest ${TestDriver} itkXorImageFilterTest)
add_test(itkZeroCrossingBasedEdgeDetectionImageFilterTest ${TestDriver} itkZeroCrossingBasedEdgeDetectionImageFilterTest)
add_test(itkZeroCrossingImageFilterTest ${TestDriver} itkZeroCrossingImageFilterTest)

add_test(itkOrientImageFilterTest ${TestDriver} itkOrientImageFilterTest)
add_test(itkOrientImageFilterTest2 ${TestDriver} itkOrientImageFilterTest2)

add_test(itkTriangleMeshToBinaryImageFilterTest ${TestDriver}
  itkTriangleMeshToBinaryImageFilterTest
  ${TEMP}/itkTriangleMeshToBinaryImageFilterTest.mha
  )

add_test(itkTriangleMeshToBinaryImageFilterTest2 ${TestDriver}
  itkTriangleMeshToBinaryImageFilterTest2
  ${TEMP}/itkTriangleMeshToBinaryImageFilterTest2.mha
  )

add_test(itkNotImageFilterTest ${TestDriver}
         itkNotImageFilterTest)

############################################################
# Some tests require data
#
if(ITK_DATA_ROOT)
add_test(itkAdaptiveHistogramEqualizationImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AdaptiveHistogramEqualizationImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest.png
  itkAdaptiveHistogramEqualizationImageFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest.png 10 0.5 0.5)

add_test(itkAccumulateImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AccumulateImageFilterTest.png

             ${ITK_TEST_OUTPUT_DIR}/AccumulateImageFilterTest.png
             itkAccumulateImageFilterTest ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR}/AccumulateImageFilterTest.png)

add_test(itkAdaptiveHistogramEqualizationImageFilterTest2 ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AdaptiveHistogramEqualizationImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest2.png
  itkAdaptiveHistogramEqualizationImageFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest2.png 10 1.0 0.25)

add_test(itkBilateralImageFilterTest2 ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/BilateralImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest2.png
  itkBilateralImageFilterTest2
            ${ITK_DATA_ROOT}/Input/cake_easy.png
            ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest2.png)

add_test(itkBilateralImageFilterTest3 ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/BilateralImageFilterTest3.png
            ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest3.png
  itkBilateralImageFilterTest3
            ${ITK_DATA_ROOT}/Input/cake_easy.png
            ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest3.png)

add_test(itkBinaryThresholdImageFilterTest2 ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/BinaryThresholdImageFilterTest2.png
             ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png
  itkBinaryThresholdImageFilterTest2
             ${ITK_DATA_ROOT}/Input/cthead1.png  ${ITK_DATA_ROOT}/Input/DicomSeries/Image0076.dcm
             ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png )

add_test(itkConfidenceConnectedImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConfidenceConnectedImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/ConfidenceConnectedImageFilterTest.png
  itkConfidenceConnectedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/ConfidenceConnectedImageFilterTest.png
             165 165)

add_test(itkConnectedComponentImageFilterTooManyObjectsTest ${TestDriver}
  itkConnectedComponentImageFilterTooManyObjectsTest)

add_test(itkConnectedComponentImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest.png
  itkConnectedComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest.png 130 145)

add_test(itkConnectedComponentImageFilterTest2 ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest2.png
  itkConnectedComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/Shapes.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest2.png 128 255 0)

add_test(itkConnectedComponentImageFilterTest3 ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest3.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest3.png
  itkConnectedComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/Shapes.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest3.png 128 255 1)

add_test(itkConnectedComponentImageFilterTestRGB ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTestRGB.png
  itkConnectedComponentImageFilterTestRGB
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTestRGB.png 130 145)

add_test(itkMaskConnectedComponentImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/MaskConnectedComponentImageFilterTest.png ${ITK_TEST_OUTPUT_DIR}/MaskConnectedComponentImageFilterTest.png
  itkMaskConnectedComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/MaskConnectedComponentImageFilterTest.png 130 145)

add_test(itkScalarConnectedComponentImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ScalarConnectedComponentImageFilterTest.png ${ITK_TEST_OUTPUT_DIR}/ScalarConnectedComponentImageFilterTest.png
  itkScalarConnectedComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/ScalarConnectedComponentImageFilterTest.png 20 1)

add_test(itkConnectedThresholdImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedThresholdImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest.png
  itkConnectedThresholdImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest.png
            165 165 0 150 )

# Exercise the fully connected feature in the connected threshold filter
if(ITK_USE_REVIEW)
add_test(itkConnectedThresholdImageFilterTest2 ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedThresholdImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest2.png
  itkConnectedThresholdImageFilterTest
            ${ITK_DATA_ROOT}/Input/8ConnectedImage.bmp
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest2.png
            29 47 200 255 1)
endif(ITK_USE_REVIEW)

add_test(itkDoubleThresholdImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/DoubleThresholdImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest.png itkDoubleThresholdImageFilterTest
            ${ITK_DATA_ROOT}/../../Examples/Data/BrainProtonDensitySlice.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest.png 217 246 255 255)

add_test(itkDoubleThresholdImageFilterTest2 ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/DoubleThresholdImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest2.png itkDoubleThresholdImageFilterTest
            ${ITK_DATA_ROOT}/../../Examples/Data/BrainProtonDensitySlice.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest2.png 150 164 164 180)

add_test(itkGradientAnisotropicDiffusionImageFilterTest2 ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GradientAnisotropicDiffusionImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterTest2.png
  itkGradientAnisotropicDiffusionImageFilterTest2
            ${ITK_DATA_ROOT}/Input/cake_easy.png
            ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterTest2.png)

add_test(itkGrayscaleMorphologicalClosingImageFilterTest ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleMorphologicalClosingImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalClosingImageFilterTest.png
   itkGrayscaleMorphologicalClosingImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalClosingImageFilterTest.png )

add_test(itkGrayscaleMorphologicalOpeningImageFilterTest ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleMorphologicalOpeningImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalOpeningImageFilterTest.png
   itkGrayscaleMorphologicalOpeningImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalOpeningImageFilterTest.png )

add_test(itkSimpleContourExtractorImageFilterTest ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/SimpleContourExtractorImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/SimpleContourExtractorImageFilterTest.png
   itkSimpleContourExtractorImageFilterTest
               ${ITK_DATA_ROOT}/Baseline/BasicFilters/BinaryThresholdImageFilterTest2.png
               ${ITK_TEST_OUTPUT_DIR}/SimpleContourExtractorImageFilterTest.png )

add_test(itkGrayscaleFillholeImageFilterTest ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleFillholeImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleFillholeImageFilterTest.png itkGrayscaleFillholeImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleFillholeImageFilterTest.png )

add_test(itkGrayscaleConnectedClosingImageFilterTest ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedClosingImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest.png itkGrayscaleConnectedClosingImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest.png
               174 214)

add_test(itkGrayscaleConnectedClosingImageFilterTest2 ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedClosingImageFilterTest2.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest2.png itkGrayscaleConnectedClosingImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest2.png
               150 169)

add_test(itkGrayscaleConnectedOpeningImageFilterTest ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedOpeningImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest.png itkGrayscaleConnectedOpeningImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest.png 174 214)

add_test(itkGrayscaleConnectedOpeningImageFilterTest2 ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedOpeningImageFilterTest2.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest2.png itkGrayscaleConnectedOpeningImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest2.png 150 169)

add_test(itkHConvexConcaveImageFilterTest ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HConvexConcaveImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/HConvexConcaveImageFilterTest.png itkHConvexConcaveImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/HConvexConcaveImageFilterTest.png 50)

add_test(itkHMaximaMinimaImageFilterTest ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest.png itkHMaximaMinimaImageFilterTest
               ${ITK_DATA_ROOT}/Input/cake_easy.png
               ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest.png 35)

add_test(itkHMaximaMinimaImageFilterTest2 ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest2.png
               ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest2.png itkHMaximaMinimaImageFilterTest
               ${ITK_DATA_ROOT}/Input/cake_hard.png
               ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest2.png 35)

add_test(itkIsolatedConnectedImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/IsolatedConnectedImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest.png
  itkIsolatedConnectedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest.png
             true
             115 85 107 110)

add_test(itkIsolatedConnectedImageFilterTest2 ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/IsolatedConnectedImageFilterTest2.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest2.png
  itkIsolatedConnectedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest2.png
             false
             175 125 100 170 176 125 101 170)

add_test(itkLaplacianRecursiveGaussianImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/LaplacianRecursiveGaussianImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/LaplacianRecursiveGaussianImageFilterTest.png
  itkLaplacianRecursiveGaussianImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/LaplacianRecursiveGaussianImageFilterTest.png)

add_test(itkNeighborhoodConnectedImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/NeighborhoodConnectedImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/NeighborhoodConnectedImageFilterTest.png
  itkNeighborhoodConnectedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/NeighborhoodConnectedImageFilterTest.png
             146 167)

add_test(itkExtractOrthogonalSwath2DImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ExtractOrthogonalSwath2DImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/ExtractOrthogonalSwath2DImageFilterTest.png
  itkExtractOrthogonalSwath2DImageFilterTest
            ${ITK_TEST_OUTPUT_DIR}/ExtractOrthogonalSwath2DImageFilterTest.png
)

add_test(itkBlackTopHatImageFilterTest ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBlackTopHatImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/itkBlackTopHatImageFilterTest.png
   itkTopHatImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/itkBlackTopHatImageFilterTest.png
               0 3)

add_test(itkWhiteTopHatImageFilterTest ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkWhiteTopHatImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/itkWhiteTopHatImageFilterTest.png
   itkTopHatImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/itkWhiteTopHatImageFilterTest.png
               1 3)

add_test(itkTileImageFilterTest ${TestDriver}
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

add_test(itkPushPopTileImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PushPopTileImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/PushPopTileImageFilterTest.png
  itkPushPopTileImageFilterTest
            ${ITK_DATA_ROOT}/Input/Shapes.png
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_DATA_ROOT}/Input/cake_easy.png
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/PushPopTileImageFilterTest.png)



add_test(itkPasteImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PasteImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/PasteImageFilterTest.png
  itkPasteImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_DATA_ROOT}/Input/cake_easy.png
            ${ITK_TEST_OUTPUT_DIR}/PasteImageFilterTest.png)

add_test(itkPromoteDimensionImageTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PromoteDimensionImageTest.mhd
            ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd
  itkPromoteDimensionImageTest
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd)

add_test(itkRelabelComponentImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RelabelComponentImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/RelabelComponentImageFilterTest.png
  itkRelabelComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/RelabelComponentImageFilterTest.png 130 145)

add_test(itkRemoveBoundaryObjectsTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RemoveBoundaryObjectsTest.png
            ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest.png
  itkRemoveBoundaryObjectsTest
            ${ITK_DATA_ROOT}/Input/Spots.png
            ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest.png)

add_test(itkRemoveBoundaryObjectsTest2 ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RemoveBoundaryObjectsTest2.png
            ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest2.png
  itkRemoveBoundaryObjectsTest2
            ${ITK_DATA_ROOT}/Input/SpotsInverted.png
            ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest2.png)

add_test(itkTobogganImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/TobogganImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/TobogganImageFilterTest.png
         itkTobogganImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/TobogganImageFilterTest.png)

add_test(itkVectorConfidenceConnectedImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorConfidenceConnectedImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest.png
  itkVectorConfidenceConnectedImageFilterTest
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest.png
            72 44 23 67 5.0 6)

add_test(itkVectorGradientMagnitudeImageFilterTest1a ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1a.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png
  itkVectorGradientMagnitudeImageFilterTest1
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png
            0)

add_test(itkVectorGradientMagnitudeImageFilterTest1b ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1b.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png
  itkVectorGradientMagnitudeImageFilterTest1
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png
            1)

add_test(itkVectorGradientMagnitudeImageFilterTest2 ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png
  itkVectorGradientMagnitudeImageFilterTest2
            ${ITK_DATA_ROOT}/Input/VHFColor.mhd
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png
            0 5)

add_test(itkVectorGradientMagnitudeImageFilterTest2b ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2b.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png
  itkVectorGradientMagnitudeImageFilterTest2
            ${ITK_DATA_ROOT}/Input/VHFColor.mhd
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png
            1 5)

add_test(itkVectorGradientMagnitudeImageFilterTest3 ${TestDriver}
  itkVectorGradientMagnitudeImageFilterTest3
            ${ITK_DATA_ROOT}/Input/VHFColor.mhd
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest3.mha
            1 )

add_test(itkVectorResampleImageFilterTest ${TestDriver}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorResampleImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png
  itkVectorResampleImageFilterTest
            ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png)

add_test(itkDivideImageFilterTest2 ${TestDriver} itkDivideImageFilterTest2)

add_test(itkGrayscaleGeodesicErodeDilateImageFilterTest ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleGeodesicErodeDilateImageFilterTest.png
   itkGrayscaleGeodesicErodeDilateImageFilterTest
               ${ITK_DATA_ROOT}/Input/cake_easy.png
               ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleGeodesicErodeDilateImageFilterTest.png 35)

add_test(itkClosingByReconstructionImageFilterTest ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ClosingByReconstructionImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTest.png
   itkClosingByReconstructionImageFilterTest
               ${ITK_DATA_ROOT}/Input/closerec1.jpg
               ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTest.png
               4
               0
               ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTestSubtract.png
)

add_test(itkClosingByReconstructionImageFilterTest2 ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ClosingByReconstructionImageFilterTest2.png
               ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTest2.png
   itkClosingByReconstructionImageFilterTest
               ${ITK_DATA_ROOT}/Input/closerec1.jpg
               ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTest2.png
               4
               1
               ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTestSubtract2.png
)

add_test(itkOpeningByReconstructionImageFilterTest ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/OpeningByReconstructionImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTest.png
   itkOpeningByReconstructionImageFilterTest
               ${ITK_DATA_ROOT}/Input/chondt.png
               ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTest.png
               4
               0
               ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTestSubtract.png
               )

add_test(itkOpeningByReconstructionImageFilterTest2 ${TestDriver}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/OpeningByReconstructionImageFilterTest2.png
               ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTest2.png
   itkOpeningByReconstructionImageFilterTest
               ${ITK_DATA_ROOT}/Input/chondt.png
               ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTest2.png
               4
               1
               ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTestSubtract2.png
               )

add_test(itkGetAverageSliceImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AccumulateImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/GetAverageSliceImageFilterTest.png
             itkGetAverageSliceImageFilterTest ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR}/GetAverageSliceImageFilterTest.png)

add_test(itkMaskNeighborhoodOperatorImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/MaskNeighborhoodOperatorImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/MaskNeighborhoodOperatorImageFilterTest.png
             itkMaskNeighborhoodOperatorImageFilterTest
                         ${INPUTDATA}/cthead1.png ${ITK_TEST_OUTPUT_DIR}/MaskNeighborhoodOperatorImageFilterTest.png)

add_test(itkVectorConnectedComponentImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorConnectedComponentImageFilterTest.png
         ${ITK_TEST_OUTPUT_DIR}/VectorConnectedComponentImageFilterTest.png
         itkVectorConnectedComponentImageFilterTest
             ${ITK_TEST_OUTPUT_DIR}/VectorConnectedComponentImageFilterTest.png)

add_test(itkMatrixIndexSelectionImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/MatrixIndexSelectionImageFilterTest.png
         ${ITK_TEST_OUTPUT_DIR}/MatrixIndexSelectionImageFilterTest.png
         itkMatrixIndexSelectionImageFilterTest
             ${ITK_TEST_OUTPUT_DIR}/MatrixIndexSelectionImageFilterTest.png)

add_test(itkInvertIntensityImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/InvertIntensityImageFilterTest.png
         ${ITK_TEST_OUTPUT_DIR}/InvertIntensityImageFilterTest.png
         itkInvertIntensityImageFilterTest
         ${INPUTDATA}/cthead1.png
         ${ITK_TEST_OUTPUT_DIR}/InvertIntensityImageFilterTest.png
         )

add_test(itkModulusImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ModulusImageFilterTest.png
         ${ITK_TEST_OUTPUT_DIR}/ModulusImageFilterTest.png
         itkModulusImageFilterTest
         ${INPUTDATA}/Spots.png
         ${ITK_TEST_OUTPUT_DIR}/ModulusImageFilterTest.png
         )

add_test(itkMorphologicalGradientImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/MorphologicalGradientImageFilterTest.png
         ${ITK_TEST_OUTPUT_DIR}/MorphologicalGradientImageFilterTest.png
         itkMorphologicalGradientImageFilterTest
         ${INPUTDATA}/cthead1.png
         ${ITK_TEST_OUTPUT_DIR}/MorphologicalGradientImageFilterTest.png
         )

add_test( itkSignedMaurerDistanceMapImageFilterTest1 ${TestDriver}
  --compare ${BASELINE}/itkSignedMaurerDistanceMapImageFilterTest1.mhd
            ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest1.mhd
          itkSignedMaurerDistanceMapImageFilterTest
          ${INPUTDATA}/SquareBinary201.png
          ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest1.mhd)

add_test( itkSignedMaurerDistanceMapImageFilterTest2 ${TestDriver}
  --compare ${BASELINE}/itkSignedMaurerDistanceMapImageFilterTest2.mhd
            ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest2.mhd
          itkSignedMaurerDistanceMapImageFilterTest
          ${INPUTDATA}/BrainSliceBinary.png
          ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest2.mhd)

add_test( itkSignedMaurerDistanceMapImageFilterTest3 ${TestDriver}
  --compare ${BASELINE}/itkSignedMaurerDistanceMapImageFilterTest3.mhd
            ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest3.mhd
          itkSignedMaurerDistanceMapImageFilterTest
          ${INPUTDATA}/LungSliceBinary.png
          ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest3.mhd)

add_test( itkVectorIndexSelectionCastImageFilterTest1 ${TestDriver}
  --compare ${BASELINE}/itkVectorIndexSelectionCastImageFilterTest1.mha
            ${TEMP}/itkVectorIndexSelectionCastImageFilterTest1.mha
          itkVectorIndexSelectionCastImageFilterTest
          ${INPUTDATA}/RGBTestImage.tif
          ${TEMP}/itkVectorIndexSelectionCastImageFilterTest1.mha 0)

add_test( itkVectorIndexSelectionCastImageFilterTest2 ${TestDriver}
  --compare ${BASELINE}/itkVectorIndexSelectionCastImageFilterTest2.mha
            ${TEMP}/itkVectorIndexSelectionCastImageFilterTest2.mha
          itkVectorIndexSelectionCastImageFilterTest
          ${INPUTDATA}/RGBTestImage.tif
          ${TEMP}/itkVectorIndexSelectionCastImageFilterTest2.mha 1)

add_test( itkVectorIndexSelectionCastImageFilterTest3 ${TestDriver}
  --compare ${BASELINE}/itkVectorIndexSelectionCastImageFilterTest3.mha
            ${TEMP}/itkVectorIndexSelectionCastImageFilterTest3.mha
          itkVectorIndexSelectionCastImageFilterTest
          ${INPUTDATA}/RGBTestImage.tif
          ${TEMP}/itkVectorIndexSelectionCastImageFilterTest3.mha 2)

add_test(itkMaximumProjectionImageFilterTest ${TestDriver}
        --compare ${BASELINE}/HeadMRVolumeMaximumProjection.tif
                  ${TEMP}/HeadMRVolumeMaximumProjection.tif
                  itkMaximumProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMaximumProjection.tif)

add_test(itkMaximumProjectionImageFilterTest2_1 ${TestDriver}
        --compare ${BASELINE}/HeadMRVolumeMaximumProjection0.tif
                  ${TEMP}/HeadMRVolumeMaximumProjection0.tif
                  itkMaximumProjectionImageFilterTest2
                  0
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMaximumProjection0.tif)

add_test(itkMaximumProjectionImageFilterTest2_2 ${TestDriver}
        --compare ${BASELINE}/HeadMRVolumeMaximumProjection1.tif
                  ${TEMP}/HeadMRVolumeMaximumProjection1.tif
                  itkMaximumProjectionImageFilterTest2
                  1
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMaximumProjection1.tif)

add_test(itkMaximumProjectionImageFilterTest3_1 ${TestDriver}
        --compare ${BASELINE}/HeadMRVolumeMaximumProjection2D0.tif
                  ${TEMP}/HeadMRVolumeMaximumProjection2D0.tif
                  itkMaximumProjectionImageFilterTest3
                  0
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMaximumProjection2D0.tif)

add_test(itkMaximumProjectionImageFilterTest3_2 ${TestDriver}
        --compare ${BASELINE}/HeadMRVolumeMaximumProjection2D1.tif
                  ${TEMP}/HeadMRVolumeMaximumProjection2D1.tif
                  itkMaximumProjectionImageFilterTest3
                  1
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMaximumProjection2D1.tif)

add_test(itkMinimumProjectionImageFilterTest ${TestDriver}
        --compare ${BASELINE}/HeadMRVolumeMinimumProjection.tif
                  ${TEMP}/HeadMRVolumeMinimumProjection.tif
                  itkMinimumProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMinimumProjection.tif)

add_test(itkMedianProjectionImageFilterTest ${TestDriver}
        --compare ${BASELINE}/HeadMRVolumeMedianProjection.tif
                  ${TEMP}/HeadMRVolumeMedianProjection.tif
                  itkMedianProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMedianProjection.tif)

add_test(itkMeanProjectionImageFilterTest ${TestDriver}
        --compare ${BASELINE}/HeadMRVolumeMeanProjection.tif
                  ${TEMP}/HeadMRVolumeMeanProjection.tif
                  itkMeanProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMeanProjection.tif)

add_test(itkStandardDeviationProjectionImageFilterTest ${TestDriver}
        --compare ${BASELINE}/HeadMRVolumeStandardDeviationProjection.tif
                  ${TEMP}/HeadMRVolumeStandardDeviationProjection.tif
                  itkStandardDeviationProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeStandardDeviationProjection.tif)

add_test(itkSumProjectionImageFilterTest ${TestDriver}
        --compare ${BASELINE}/HeadMRVolumeSumProjection.tif
                  ${TEMP}/HeadMRVolumeSumProjection.tif
                  itkSumProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeSumProjection.tif)

add_test(itkProjectionImageFilterTest1 ${TestDriver}
        --compare ${BASELINE}/HeadMRVolumeBinaryProjection100.tif
                  ${TEMP}/HeadMRVolumeProjection100.tif
                  itkProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeProjection100.tif
                  100
                  0)

add_test(itkBinaryProjectionImageFilterTest1 ${TestDriver}
        --compare ${BASELINE}/HeadMRVolumeBinaryProjection100.tif
                  ${TEMP}/HeadMRVolumeBinaryProjection100.tif
                  itkBinaryProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeBinaryProjection100.tif
                  100
                  0)

add_test(itkBinaryProjectionImageFilterTest2 ${TestDriver}
        --compare ${BASELINE}/HeadMRVolumeBinaryProjection200.tif
                  ${TEMP}/HeadMRVolumeBinaryProjection200.tif
                  itkBinaryProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeBinaryProjection200.tif
                  200
                  0)

add_test(itkBinaryThresholdProjectionImageFilterTest ${TestDriver}
        --compare ${BASELINE}/HeadMRVolumeBinaryThresholdProjection.png
                  ${TEMP}/HeadMRVolumeBinaryThresholdProjection.png
                  itkBinaryThresholdProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeBinaryThresholdProjection.png
                  100
                  255
                  0)

add_test(itkImageToVectorImageFilterTest ${TestDriver}
        --compare ${BASELINE}/ImageToVectorImageFilterTest.png
                  ${TEMP}/ImageToVectorImageFilterTest.png
                  itkImageToVectorImageFilterTest
                  ${INPUTDATA}/Spots.png
                  ${INPUTDATA}/SpotsInverted.png
                  ${INPUTDATA}/SpotsInverted.png
                  ${TEMP}/ImageToVectorImageFilterTest.png
                  )
add_test(itkLabelStatisticsImageFilterTest ${TestDriver}
         itkLabelStatisticsImageFilterTest
         ${ITK_DATA_ROOT}/Input/peppers.png
         ${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuMultipleThresholdsImageFilterTest.png
         )

add_test(itkNoiseImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkNoiseImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/itkNoiseImageFilterTest.png
   itkNoiseImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkNoiseImageFilterTest.png)
add_test(itkNormalizedCorrelationImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/NormalizedCorrelationImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/NormalizedCorrelationImageFilterTest.png
             itkNormalizedCorrelationImageFilterTest
             ${ITK_DATA_ROOT}/Input/sf4.png ${ITK_DATA_ROOT}/Input/circle.png ${ITK_TEST_OUTPUT_DIR}/NormalizedCorrelationImageFilterTest.png)
add_test(itkParallelSparseFieldLevelSetImageFilterTest ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ParallelSparseFieldLevelSetImageFilterTest.mha
             ${ITK_TEST_OUTPUT_DIR}/ParallelSparseFieldLevelSetImageFilterTest.mha
              itkParallelSparseFieldLevelSetImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/ParallelSparseFieldLevelSetImageFilterTest.mha)
add_test(itkResampleImageTest2 ${TestDriver}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ResampleImageTest2.png
             ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2.png
             itkResampleImageTest2
             ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/circle.png ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2.png)
foreach(fg 100 200)
  foreach(bg 0 150)
    foreach(border 0 1)
      add_test(itkBinaryDilateImageFilterTest-${fg}-${bg}-${border} ${TestDriver}
        --compare ${BASELINE}/itkBinaryDilateImageFilterTest-${fg}-${bg}-${border}.png
                  ${TEMP}/itkBinaryDilateImageFilterTest-${fg}-${bg}-${border}.png
        itkBinaryDilateImageFilterTest3
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryDilateImageFilterTest-${fg}-${bg}-${border}.png
                  ${fg} ${bg} ${border} 10)
      add_test(itkBinaryErodeImageFilterTest-${fg}-${bg}-${border} ${TestDriver}
        --compare ${BASELINE}/itkBinaryErodeImageFilterTest-${fg}-${bg}-${border}.png
                  ${TEMP}/itkBinaryErodeImageFilterTest-${fg}-${bg}-${border}.png
        itkBinaryErodeImageFilterTest3
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryErodeImageFilterTest-${fg}-${bg}-${border}.png
                  ${fg} ${bg} ${border} 10)
    endforeach(border)
  endforeach(bg)
endforeach(fg)

add_test(itkCannyEdgeDetectionImageFilterTest ${TestDriver}
  --compare ${BASELINE}/itkCannyEdgeDetectionImageFilterTest.png
            ${TEMP}/itkCannyEdgeDetectionImageFilterTest.png
            itkCannyEdgeDetectionImageFilterTest
            ${INPUTDATA}/cthead1.png
            ${TEMP}/itkCannyEdgeDetectionImageFilterTest.png)
add_test(itkCannyEdgeDetectionImageFilterTest2 ${TestDriver}
  --compare ${TEMP}/itkCannyEdgeDetectionImageFilterTest2_A.png
            ${TEMP}/itkCannyEdgeDetectionImageFilterTest2_B.png
            itkCannyEdgeDetectionImageFilterTest2
            ${INPUTDATA}/cthead1.png
            ${TEMP}/itkCannyEdgeDetectionImageFilterTest2_A.png
            ${TEMP}/itkCannyEdgeDetectionImageFilterTest2_B.png)
add_test(itkDerivativeImageFilterTest1x ${TestDriver}
  --compare ${BASELINE}/itkDerivativeImageFilterTest1x.png
            ${TEMP}/itkDerivativeImageFilterTest1x.png
            itkDerivativeImageFilterTest
            ${INPUTDATA}/cthead1.png
            ${TEMP}/itkDerivativeImageFilterTest1x.png 1 0)
add_test(itkDerivativeImageFilterTest1y ${TestDriver}
  --compare ${BASELINE}/itkDerivativeImageFilterTest1y.png
            ${TEMP}/itkDerivativeImageFilterTest1y.png
            itkDerivativeImageFilterTest
            ${INPUTDATA}/cthead1.png
            ${TEMP}/itkDerivativeImageFilterTest1y.png 1 1)
add_test(itkDerivativeImageFilterTest2x ${TestDriver}
  --compare ${BASELINE}/itkDerivativeImageFilterTest2x.png
            ${TEMP}/itkDerivativeImageFilterTest2x.png
            itkDerivativeImageFilterTest
            ${INPUTDATA}/cthead1.png
            ${TEMP}/itkDerivativeImageFilterTest2x.png 2 0)
add_test(itkDerivativeImageFilterTest2y ${TestDriver}
  --compare ${BASELINE}/itkDerivativeImageFilterTest2y.png
            ${TEMP}/itkDerivativeImageFilterTest2y.png
            itkDerivativeImageFilterTest
            ${INPUTDATA}/cthead1.png
            ${TEMP}/itkDerivativeImageFilterTest2y.png 2 1)
add_test(itkDiscreteGaussianImageFilterTest2 ${TestDriver}
  --compare ${BASELINE}/DiscreteGaussianImageFilterTest2_OutputA.mha
            ${TEMP}/DiscreteGaussianImageFilterTest2_OutputA.mha
  itkDiscreteGaussianImageFilterTest2 2 3
  ${INPUTDATA}/RGBTestImage.tif 3.5
  ${TEMP}/DiscreteGaussianImageFilterTest2_OutputA.mha
  )
add_test(itkJoinSeriesImageFilterStreamingTest ${TestDriver}
  --compare ${INPUTDATA}/HeadMRVolume.mhd
            ${TEMP}/itkJoinSeriesImageFilterStreamingTest.mha
  itkJoinSeriesImageFilterStreamingTest
            ${INPUTDATA}/HeadMRVolume.mhd
            ${TEMP}/itkJoinSeriesImageFilterStreamingTest.mha
   )
add_test(itkPolylineMask2DImageFilterTest
  ${TestDriver} itkPolylineMask2DImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkPolylineMask2DImageFilterTest.png
  )

endif(ITK_DATA_ROOT)

add_test(itkWarpImageFilterTest2 ${TestDriver} itkWarpImageFilterTest2)

add_test(itkSimplexMeshWithFloatCoordRepTest ${TestDriver} itkSimplexMeshWithFloatCoordRepTest)

add_test(itkBSplineDownsampleImageFilterTest
  ${TestDriver} itkBSplineDownsampleImageFilterTest
  ${INPUTDATA}/HeadMRVolume.mhd
  ${TEMP}/itkBSplineDownsampleImageFilterTest1.mha
  3  # Spline order
  )

add_test(itkBSplineUpsampleImageFilterTest
  ${TestDriver} itkBSplineUpsampleImageFilterTest
  ${INPUTDATA}/HeadMRVolume.mhd
  ${TEMP}/itkBSplineUpsampleImageFilterTest1.mha
  3  # Spline order
  )


if(ITK_OBJCXX_COMPILER_WORKS)
  set(BASICFILTERS_OBJCXX_TEST ${CXX_TEST_PATH}/itkBasicFiltersHeaderObjCxxTest)
  add_test(itkBasicFiltersHeaderObjCxxTest ${BASICFILTERS_OBJCXX_TEST})
  add_executable(itkBasicFiltersHeaderObjCxxTest itkBasicFiltersHeaderObjCxxTest.mm)
  target_link_libraries(itkBasicFiltersHeaderObjCxxTest ITKIO ITKNumerics ITKBasicFilters)
endif(ITK_OBJCXX_COMPILER_WORKS)

add_test(itkBasicFiltersHeaderTest ${BASIC_FILTERS_HEADER_TEST})

add_executable(itkBasicFiltersHeaderTest itkBasicFiltersHeaderTest.cxx)
target_link_libraries(itkBasicFiltersHeaderTest ITKIO ITKNumerics ITKBasicFilters)

endif( NOT ITK_DISABLE_CXX_TESTING )


############################################################
# TCl Testing
#
if( NOT ITK_DISABLE_TCL_TESTING )
if(ITK_CSWIG_TCL)

  add_test(CannyEdgeDetectionImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CannyEdgeDetectionImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CannyEdgeDetectionImageFilterTclTest.png
           5 .0625 0.125)

  add_test(CastImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CastImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CastImageFilterTclTest.png )

  add_test(CurvatureFlowImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CurvatureFlowImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CurvatureFlowImageFilterTclTest.png
           5 0.125 )

  add_test(CurvatureAnisotropicDiffusionImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CurvatureAnisotropicDiffusionImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CurvatureAnisotropicDiffusionImageFilterTclTest.png
           5 0.125 3.0)

  add_test(GradientAnisotropicDiffusionImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/GradientAnisotropicDiffusionImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterTclTest.png
           5 0.125 3.0)

 add_test(BinaryDilateImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/BinaryDilateImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryDilateImageFilterTclTest.png )

  add_test(BinaryErodeImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/BinaryErodeImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryErodeImageFilterTclTest.png )

  add_test(MeanImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/MeanImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/MeanImageFilterTclTest.png
           2 )

  add_test(MedianImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/MedianImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/MedianImageFilterTclTest.png
           1 )

  add_test(SigmoidImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/SigmoidImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/SigmoidImageFilterTclTest.png
           0 255  1.0  128  )

  add_test(ThresholdImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/ThresholdImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/ThresholdImageFilterTclTest.png
           0 150 )

  add_test(BinaryThresholdImageFilterTcl ${ITKWISH_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/BinaryThresholdImageFilter.tcl
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTclTest.png
           0 255 150 180)

endif(ITK_CSWIG_TCL)
endif( NOT ITK_DISABLE_TCL_TESTING )

############################################################
# Python Testing
#
if( NOT ITK_DISABLE_PYTHON_TESTING )
if(ITK_CSWIG_PYTHON)

  add_test(CannyEdgeDetectionImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CannyEdgeDetectionImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CannyEdgeDetectionImageFilterPythonTest.png
           5 .0625 0.125 )

  add_test(CastImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CastImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CastImageFilterPythonTest.png )

  add_test(CurvatureFlowImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CurvatureFlowImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CurvatureFlowImageFilterPythonTest.png
           5 0.125)

  add_test(CurvatureAnisotropicDiffusionImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/CurvatureAnisotropicDiffusionImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CurvatureAnisotropicDiffusionImageFilterPythonTest.png
           5 0.125 3.0)

  add_test(GradientAnisotropicDiffusionImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/GradientAnisotropicDiffusionImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterPythonTest.png
           5 0.125 3.0)

 add_test(BinaryErodeImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/BinaryErodeImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryErodeImageFilterPythonTest.png )

  add_test(BinaryDilateImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/BinaryDilateImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryDilateImageFilterPythonTest.png )

 add_test(LaplacianImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/LaplacianImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/LaplacianImageFilterPythonTest.png )

 add_test(MeanImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/MeanImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/MeanImageFilterPythonTest.png
           2 )

  add_test(MedianImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/MedianImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/MedianImageFilterPythonTest.png
           1 )

  add_test(SigmoidImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/SigmoidImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/SigmoidImageFilterPythonTest.png
           0 255  1.0  128  )

  add_test(ThresholdImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/ThresholdImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/ThresholdImageFilterPythonTest.png
           0 150 )

  add_test(BinaryThresholdImageFilterPython ${PYTHON_EXECUTABLE}
           ${ITK_SOURCE_DIR}/Examples/Filtering/BinaryThresholdImageFilter.py
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterPythonTest.png
           0 255 150 180)

endif(ITK_CSWIG_PYTHON)
endif( NOT ITK_DISABLE_PYTHON_TESTING )

############################################################
# Java Testing
#
if( NOT ITK_DISABLE_JAVA_TESTING )
if(ITK_CSWIG_JAVA AND ITK_BINARY_DIR)
  set(JAVACLASSPATH ${ITK_BINARY_DIR}/Wrapping/CSwig/Java/InsightToolkit.jar)
  foreach(cfgtype ${CMAKE_CONFIGURATION_TYPES})
    set(JAVACLASSPATH "${JAVACLASSPATH}\\;${ITK_BINARY_DIR}/Wrapping/CSwig/Java/${cfgtype}/InsightToolkit.jar")
  endforeach(cfgtype)

  foreach( javatestfile
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

  add_custom_target(${javatestfile}Java ALL  DEPENDS ${ITK_BINARY_DIR}/Examples/Filtering/${javatestfile}.class)

  add_custom_command(
    OUTPUT  ${ITK_BINARY_DIR}/Examples/Filtering/${javatestfile}.class
    DEPENDS ${ITK_SOURCE_DIR}/Examples/Filtering/${javatestfile}.java
    COMMAND ${JAVA_COMPILE}
    ARGS -classpath "${JAVACLASSPATH}" -d "${ITK_BINARY_DIR}/Examples/Filtering"
    ${ITK_SOURCE_DIR}/Examples/Filtering/${javatestfile}.java
    COMMENT "Java Class")

  endforeach( javatestfile )

  if(WIN32)
    set(TMPJAVACLASSPATH ${ITK_BINARY_DIR}/Examples/Filtering/)
    foreach(javadirectory ${JAVACLASSPATH})
      # Set path separator for Windows
      set(TMPJAVACLASSPATH "${TMPJAVACLASSPATH}\\;${javadirectory}")
    endforeach(javadirectory)
    set(JAVACLASSPATH ${TMPJAVACLASSPATH})
  else(WIN32)
    set(TMPJAVACLASSPATH ${ITK_BINARY_DIR}/Examples/Filtering/)
    foreach(javadirectory ${JAVACLASSPATH})
      # Set path separator for UNIX
      set(TMPJAVACLASSPATH "${TMPJAVACLASSPATH}:${javadirectory}")
    endforeach(javadirectory)
    set(JAVACLASSPATH ${TMPJAVACLASSPATH})
  endif(WIN32)

  add_test(MeanImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           MeanImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/MeanImageFilterJavaTest.png
           1 )

  add_test(MedianImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           MedianImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/MedianImageFilterJavaTest.png
           1 )

  add_test(CannyEdgeDetectionImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           CannyEdgeDetectionImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CannyEdgeDetectionImageFilterJavaTest.png
           5 .0625 0.125 )

  add_test(CastImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           CastImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CastImageFilterJavaTest.png )

 add_test(CurvatureFlowImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           CurvatureFlowImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CurvatureFlowImageFilterJavaTest.png
           5 0.125 )

  add_test(CurvatureAnisotropicDiffusionImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           CurvatureAnisotropicDiffusionImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/CurvatureAnisotropicDiffusionImageFilterJavaTest.png
           5 0.125 3.0)

  add_test(GradientAnisotropicDiffusionImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           GradientAnisotropicDiffusionImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterJavaTest.png
           5 0.125 3.0)

  add_test(ThresholdImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           ThresholdImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/ThresholdImageFilterJavaTest.png
           0 150)

  add_test(BinaryThresholdImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           BinaryThresholdImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterJavaTest.png
           150 180 0 255 )

  add_test(BinaryDilateImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           BinaryDilateImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryDilateImageFilterJavaTest.png )

  add_test(BinaryErodeImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           BinaryErodeImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/BinaryErodeImageFilterJavaTest.png )

  add_test(SigmoidImageFilterJava ${JAVA_RUNTIME} -classpath "${JAVACLASSPATH}"
           SigmoidImageFilter
           ${ITK_SOURCE_DIR}/Examples/Data/BrainT1SliceBorder20.png
           ${ITK_TEST_OUTPUT_DIR}/SigmoidImageFilterJavaTest.png
           0 255  1.0  128  )

endif(ITK_CSWIG_JAVA AND ITK_BINARY_DIR)
endif( NOT ITK_DISABLE_JAVA_TESTING )
