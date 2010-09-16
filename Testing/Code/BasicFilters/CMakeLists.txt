include_regular_expression("^.*$")

if( NOT ITK_DISABLE_CXX_TESTING )
if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-BasicFilters ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/BasicFilters)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)


set(BASIC_FILTERS_TESTS ${CXX_TEST_PATH}/itkBasicFiltersTests)
set(BASIC_FILTERS_TESTS1 ${CXX_TEST_PATH}/itkBasicFiltersTests1)
set(BASIC_FILTERS_TESTS2 ${CXX_TEST_PATH}/itkBasicFiltersTests2)
set(BASIC_FILTERS_TESTS3 ${CXX_TEST_PATH}/itkBasicFiltersTests3)
set(BASIC_FILTERS_TESTS4 ${CXX_TEST_PATH}/itkBasicFiltersTests4)
set(BASIC_FILTERS_TESTS5 ${CXX_TEST_PATH}/itkBasicFiltersTests5)

set(BASIC_FILTERS_HEADER_TEST ${CXX_TEST_PATH}/itkBasicFiltersHeaderTest)
set(BASELINE ${ITK_DATA_ROOT}/Baseline/BasicFilters)
set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(TEMP ${ITKTesting_BINARY_DIR}/Temporary)

add_test(itkBasicFiltersPrintTest ${BASIC_FILTERS_TESTS2} itkBasicFiltersPrintTest)
add_test(itkBasicFiltersPrintTest2 ${BASIC_FILTERS_TESTS2} itkBasicFiltersPrintTest2)

add_test(itkAcosImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkAcosImageFilterAndAdaptorTest)
add_test(itkAbsImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkAbsImageFilterAndAdaptorTest)
add_test(itkAbsoluteValueDifferenceImageFilterTest ${BASIC_FILTERS_TESTS} itkAbsoluteValueDifferenceImageFilterTest)
add_test(itkAdaptImageFilterTest ${BASIC_FILTERS_TESTS} itkAdaptImageFilterTest)
add_test(itkAdaptImageFilterTest2 ${BASIC_FILTERS_TESTS} itkAdaptImageFilterTest2)
add_test(itkAddImageFilterTest ${BASIC_FILTERS_TESTS} itkAddImageFilterTest)
add_test(itkAndImageFilterTest ${BASIC_FILTERS_TESTS} itkAndImageFilterTest)
add_test(itkApproximateSignedDistanceMapImageFilterTest ${BASIC_FILTERS_TESTS3}
  --compare ${BASELINE}/itkApproximateSignedDistanceMapImageFilterTest.png
            ${TEMP}/itkApproximateSignedDistanceMapImageFilterTest.png
  itkApproximateSignedDistanceMapImageFilterTest
             ${TEMP}/itkApproximateSignedDistanceMapImageFilterTest.png)
add_test(itkAsinImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkAsinImageFilterAndAdaptorTest)
add_test(itkAtanImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkAtanImageFilterAndAdaptorTest)
add_test(itkAtan2ImageFilterTest ${BASIC_FILTERS_TESTS} itkAtan2ImageFilterTest)
add_test(itkBSplineDecompositionImageFilterTest ${BASIC_FILTERS_TESTS} itkBSplineDecompositionImageFilterTest)
add_test(itkBSplineInterpolateImageFunctionTest ${BASIC_FILTERS_TESTS} itkBSplineInterpolateImageFunctionTest)
add_test(itkBSplineResampleImageFilterTest ${BASIC_FILTERS_TESTS} itkBSplineResampleImageFilterTest)
add_test(itkBSplineResampleImageFunctionTest ${BASIC_FILTERS_TESTS} itkBSplineResampleImageFunctionTest)
add_test(itkBasicArchitectureTest ${BASIC_FILTERS_TESTS} itkBasicArchitectureTest)
add_test(itkBilateralImageFilterTest ${BASIC_FILTERS_TESTS} itkBilateralImageFilterTest)
add_test(itkBinaryDilateImageFilterTest ${BASIC_FILTERS_TESTS} itkBinaryDilateImageFilterTest)
add_test(itkBinaryDilateImageFilterTest2 ${BASIC_FILTERS_TESTS} itkBinaryDilateImageFilterTest2)

foreach(fg 100 200)
  foreach(bg 0 150)
    foreach(border 0 1)
      add_test(itkBinaryDilateImageFilterTest-${fg}-${bg}-${border} ${BASIC_FILTERS_TESTS}
        --compare ${BASELINE}/itkBinaryDilateImageFilterTest-${fg}-${bg}-${border}.png
                  ${TEMP}/itkBinaryDilateImageFilterTest-${fg}-${bg}-${border}.png
        itkBinaryDilateImageFilterTest3
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryDilateImageFilterTest-${fg}-${bg}-${border}.png
                  ${fg} ${bg} ${border} 10)
      add_test(itkBinaryErodeImageFilterTest-${fg}-${bg}-${border} ${BASIC_FILTERS_TESTS}
        --compare ${BASELINE}/itkBinaryErodeImageFilterTest-${fg}-${bg}-${border}.png
                  ${TEMP}/itkBinaryErodeImageFilterTest-${fg}-${bg}-${border}.png
        itkBinaryErodeImageFilterTest3
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryErodeImageFilterTest-${fg}-${bg}-${border}.png
                  ${fg} ${bg} ${border} 10)
    endforeach(border)
  endforeach(bg)
endforeach(fg)

add_test(itkBinaryErodeImageFilterTest ${BASIC_FILTERS_TESTS} itkBinaryErodeImageFilterTest)
add_test(itkBinaryMagnitudeImageFilterTest ${BASIC_FILTERS_TESTS} itkBinaryMagnitudeImageFilterTest)
add_test(itkBinaryMaskToNarrowBandPointSetFilterTest ${BASIC_FILTERS_TESTS} itkBinaryMaskToNarrowBandPointSetFilterTest)
add_test(itkBinaryMedianImageFilterTest ${BASIC_FILTERS_TESTS} itkBinaryMedianImageFilterTest)
add_test(itkVotingBinaryImageFilterTest ${BASIC_FILTERS_TESTS3} itkVotingBinaryImageFilterTest)
add_test(itkVotingBinaryHoleFillingImageFilterTest ${BASIC_FILTERS_TESTS3} itkVotingBinaryHoleFillingImageFilterTest)
add_test(itkVotingBinaryIterativeHoleFillingImageFilterTest ${BASIC_FILTERS_TESTS3} itkVotingBinaryIterativeHoleFillingImageFilterTest)
add_test(itkBinaryThresholdImageFilterTest ${BASIC_FILTERS_TESTS} itkBinaryThresholdImageFilterTest)
add_test(itkBloxBoundaryPointImageTest ${BASIC_FILTERS_TESTS} itkBloxBoundaryPointImageTest)
add_test(itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest ${BASIC_FILTERS_TESTS} itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest)
add_test(itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest ${BASIC_FILTERS_TESTS} itkBloxBoundaryProfileImageToBloxCoreAtomImageFilterTest)
add_test(itkBloxCoreAtomTest ${BASIC_FILTERS_TESTS} itkBloxCoreAtomTest)
add_test(itkCannyEdgeDetectionImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${BASELINE}/itkCannyEdgeDetectionImageFilterTest.png
            ${TEMP}/itkCannyEdgeDetectionImageFilterTest.png
            itkCannyEdgeDetectionImageFilterTest
            ${INPUTDATA}/cthead1.png
            ${TEMP}/itkCannyEdgeDetectionImageFilterTest.png)
add_test(itkCannyEdgeDetectionImageFilterTest2 ${BASIC_FILTERS_TESTS}
  --compare ${TEMP}/itkCannyEdgeDetectionImageFilterTest2_A.png
            ${TEMP}/itkCannyEdgeDetectionImageFilterTest2_B.png
            itkCannyEdgeDetectionImageFilterTest2
            ${INPUTDATA}/cthead1.png
            ${TEMP}/itkCannyEdgeDetectionImageFilterTest2_A.png
            ${TEMP}/itkCannyEdgeDetectionImageFilterTest2_B.png)
add_test(itkChainCodeToFourierSeriesPathFilterTest ${BASIC_FILTERS_TESTS} itkChainCodeToFourierSeriesPathFilterTest)
add_test(itkChangeInformationImageFilterTest ${BASIC_FILTERS_TESTS} itkChangeInformationImageFilterTest)
add_test(itkChangeLabelImageFilterTest ${BASIC_FILTERS_TESTS} itkChangeLabelImageFilterTest)
add_test(itkCheckerBoardImageFilterTest ${BASIC_FILTERS_TESTS3} itkCheckerBoardImageFilterTest)
add_test(itkComposeRGBImageFilterTest ${BASIC_FILTERS_TESTS} itkComposeRGBImageFilterTest)
add_test(itkCompose2DCovariantVectorImageFilterTest ${BASIC_FILTERS_TESTS} itkCompose2DCovariantVectorImageFilterTest)
add_test(itkCompose3DCovariantVectorImageFilterTest ${BASIC_FILTERS_TESTS} itkCompose3DCovariantVectorImageFilterTest)
add_test(itkCompose2DVectorImageFilterTest ${BASIC_FILTERS_TESTS} itkCompose2DVectorImageFilterTest)
add_test(itkCompose3DVectorImageFilterTest ${BASIC_FILTERS_TESTS} itkCompose3DVectorImageFilterTest)
add_test(itkComplexToRealFilterAndAdaptorTest ${BASIC_FILTERS_TESTS2} itkComplexToRealFilterAndAdaptorTest)
add_test(itkComplexToImaginaryFilterAndAdaptorTest ${BASIC_FILTERS_TESTS2} itkComplexToImaginaryFilterAndAdaptorTest)
add_test(itkComplexToModulusFilterAndAdaptorTest ${BASIC_FILTERS_TESTS2} itkComplexToModulusFilterAndAdaptorTest)
add_test(itkComplexToPhaseFilterAndAdaptorTest ${BASIC_FILTERS_TESTS2} itkComplexToPhaseFilterAndAdaptorTest)
add_test(itkContourDirectedMeanDistanceImageFilterTest ${BASIC_FILTERS_TESTS2} itkContourDirectedMeanDistanceImageFilterTest)
add_test(itkContourMeanDistanceImageFilterTest ${BASIC_FILTERS_TESTS2} itkContourMeanDistanceImageFilterTest)
add_test(itkConstantPadImageTest ${BASIC_FILTERS_TESTS} itkConstantPadImageTest)
add_test(itkConstrainedValueAdditionImageFilterTest ${BASIC_FILTERS_TESTS2} itkConstrainedValueAdditionImageFilterTest)
add_test(itkConstrainedValueDifferenceImageFilterTest ${BASIC_FILTERS_TESTS2} itkConstrainedValueDifferenceImageFilterTest)
add_test(itkCosImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkCosImageFilterAndAdaptorTest)
add_test(itkCropImageFilterTest ${BASIC_FILTERS_TESTS} itkCropImageFilterTest)
add_test(itkCurvatureAnisotropicDiffusionImageFilterTest ${BASIC_FILTERS_TESTS} itkCurvatureAnisotropicDiffusionImageFilterTest)
add_test(itkCyclicReferences ${BASIC_FILTERS_TESTS} itkCyclicReferences)
add_test(itkDanielssonDistanceMapImageFilterTest ${BASIC_FILTERS_TESTS} itkDanielssonDistanceMapImageFilterTest)
add_test(itkSignedDanielssonDistanceMapImageFilterTest ${BASIC_FILTERS_TESTS2} itkSignedDanielssonDistanceMapImageFilterTest)
add_test(itkDerivativeImageFilterTest1x ${BASIC_FILTERS_TESTS}
  --compare ${BASELINE}/itkDerivativeImageFilterTest1x.png
            ${TEMP}/itkDerivativeImageFilterTest1x.png
            itkDerivativeImageFilterTest
            ${INPUTDATA}/cthead1.png
            ${TEMP}/itkDerivativeImageFilterTest1x.png 1 0)
add_test(itkDerivativeImageFilterTest1y ${BASIC_FILTERS_TESTS}
  --compare ${BASELINE}/itkDerivativeImageFilterTest1y.png
            ${TEMP}/itkDerivativeImageFilterTest1y.png
            itkDerivativeImageFilterTest
            ${INPUTDATA}/cthead1.png
            ${TEMP}/itkDerivativeImageFilterTest1y.png 1 1)
add_test(itkDerivativeImageFilterTest2x ${BASIC_FILTERS_TESTS}
  --compare ${BASELINE}/itkDerivativeImageFilterTest2x.png
            ${TEMP}/itkDerivativeImageFilterTest2x.png
            itkDerivativeImageFilterTest
            ${INPUTDATA}/cthead1.png
            ${TEMP}/itkDerivativeImageFilterTest2x.png 2 0)
add_test(itkDerivativeImageFilterTest2y ${BASIC_FILTERS_TESTS}
  --compare ${BASELINE}/itkDerivativeImageFilterTest2y.png
            ${TEMP}/itkDerivativeImageFilterTest2y.png
            itkDerivativeImageFilterTest
            ${INPUTDATA}/cthead1.png
            ${TEMP}/itkDerivativeImageFilterTest2y.png 2 1)
add_test(itkDiffusionTensor3DReconstructionImageFilterTest ${BASIC_FILTERS_TESTS} itkDiffusionTensor3DReconstructionImageFilterTest)
add_test(itkInverseDeformationFieldImageFilterTest ${BASIC_FILTERS_TESTS} itkInverseDeformationFieldImageFilterTest
           ${TEMP}/itkInverseDeformationFieldImageFilterTest.mha
           )
add_test(itkIterativeInverseDeformationFieldImageFilterTest ${BASIC_FILTERS_TESTS} itkIterativeInverseDeformationFieldImageFilterTest
           ${TEMP}/itkIterativeInverseDeformationFieldImageFilterTest.mha
           )
add_test(itkDeformationFieldSourceTest ${BASIC_FILTERS_TESTS} itkDeformationFieldSourceTest
           ${TEMP}/itkDeformationFieldSourceTestLandmarks.txt
           ${TEMP}/itkDeformationFieldSourceTest.mha
           )
add_test(itkDeformationFieldJacobianDeterminantFilterTest ${BASIC_FILTERS_TESTS2} itkDeformationFieldJacobianDeterminantFilterTest)
add_test(itkDisplacementFieldJacobianDeterminantFilterTest ${BASIC_FILTERS_TESTS2} itkDisplacementFieldJacobianDeterminantFilterTest)
add_test(itkDifferenceOfGaussiansGradientTest ${BASIC_FILTERS_TESTS} itkDifferenceOfGaussiansGradientTest)
add_test(itkDiscreteGaussianImageFilterTest ${BASIC_FILTERS_TESTS} itkDiscreteGaussianImageFilterTest)
add_test(itkDivideImageFilterTest ${BASIC_FILTERS_TESTS} itkDivideImageFilterTest)
add_test(itkEdgePotentialImageFilterTest ${BASIC_FILTERS_TESTS} itkEdgePotentialImageFilterTest)
add_test(itkEigenAnalysis2DImageFilterTest ${BASIC_FILTERS_TESTS} itkEigenAnalysis2DImageFilterTest)
add_test(itkExpImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkExpImageFilterAndAdaptorTest)
add_test(itkExpNegativeImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkExpNegativeImageFilterAndAdaptorTest)
add_test(itkExpandImageFilterTest ${BASIC_FILTERS_TESTS} itkExpandImageFilterTest)
add_test(itkExtractImageTest ${BASIC_FILTERS_TESTS} itkExtractImageTest)
add_test(itkFilterDispatchTest ${BASIC_FILTERS_TESTS} itkFilterDispatchTest)
add_test(itkFlipImageFilterTest ${BASIC_FILTERS_TESTS} itkFlipImageFilterTest)
add_test(itkFloodFillIteratorTest ${BASIC_FILTERS_TESTS} itkFloodFillIteratorTest)
add_test(itkGaussianImageSourceTest ${BASIC_FILTERS_TESTS} itkGaussianImageSourceTest)
add_test(itkGradientAnisotropicDiffusionImageFilterTest ${BASIC_FILTERS_TESTS} itkGradientAnisotropicDiffusionImageFilterTest)
add_test(itkGradientImageFilterTest ${BASIC_FILTERS_TESTS} itkGradientImageFilterTest)
add_test(itkGradientMagnitudeImageFilterTest ${BASIC_FILTERS_TESTS} itkGradientMagnitudeImageFilterTest)
add_test(itkGradientMagnitudeRecursiveGaussianFilterTest ${BASIC_FILTERS_TESTS} itkGradientMagnitudeRecursiveGaussianFilterTest)
add_test(itkGradientRecursiveGaussianFilterTest ${BASIC_FILTERS_TESTS} itkGradientRecursiveGaussianFilterTest)
add_test(itkGradientRecursiveGaussianFilterTest2 ${BASIC_FILTERS_TESTS} itkGradientRecursiveGaussianFilterTest2)
add_test(itkGrayscaleFunctionDilateImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${BASELINE}/itkGrayscaleFunctionDilateImageFilterTest.mha
            ${TEMP}/itkGrayscaleFunctionDilateImageFilterTest.mha
  itkGrayscaleFunctionDilateImageFilterTest
            ${TEMP}/itkGrayscaleFunctionDilateImageFilterTest.mha
)
add_test(itkGrayscaleFunctionErodeImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${BASELINE}/itkGrayscaleFunctionErodeImageFilterTest.mha
            ${TEMP}/itkGrayscaleFunctionErodeImageFilterTest.mha
  itkGrayscaleFunctionErodeImageFilterTest
            ${TEMP}/itkGrayscaleFunctionErodeImageFilterTest.mha
)

add_test(itkGradientToMagnitudeImageFilterTest ${BASIC_FILTERS_TESTS} itkGradientToMagnitudeImageFilterTest)
add_test(itkHardConnectedComponentImageFilterTest ${BASIC_FILTERS_TESTS} itkHardConnectedComponentImageFilterTest)
add_test(itkHausdorffDistanceImageFilterTest ${BASIC_FILTERS_TESTS} itkHausdorffDistanceImageFilterTest)
add_test(itkHessianRecursiveGaussianFilterTest ${BASIC_FILTERS_TESTS3} itkHessianRecursiveGaussianFilterTest)
add_test(itkHessianRecursiveGaussianFilterScaleSpaceTest ${BASIC_FILTERS_TESTS3} itkHessianRecursiveGaussianFilterScaleSpaceTest)
add_test(itkHessian3DToVesselnessMeasureImageFilterTest ${BASIC_FILTERS_TESTS4} itkHessian3DToVesselnessMeasureImageFilterTest)

add_test(itkLabelStatisticsImageFilterTest ${BASIC_FILTERS_TESTS3}
         itkLabelStatisticsImageFilterTest
         ${ITK_DATA_ROOT}/Input/peppers.png
         ${ITK_DATA_ROOT}/Baseline/Algorithms/OtsuMultipleThresholdsImageFilterTest.png
         )

add_test(itkSymmetricEigenAnalysisImageFilterTest ${BASIC_FILTERS_TESTS3} itkSymmetricEigenAnalysisImageFilterTest)
add_test(itkTensorFractionalAnisotropyImageFilterTest ${BASIC_FILTERS_TESTS3} itkTensorFractionalAnisotropyImageFilterTest)
add_test(itkTensorRelativeAnisotropyImageFilterTest ${BASIC_FILTERS_TESTS3} itkTensorRelativeAnisotropyImageFilterTest)
add_test(itkHoughTransform2DCirclesImageTest ${BASIC_FILTERS_TESTS} itkHoughTransform2DCirclesImageTest)
add_test(itkHoughTransform2DLinesImageTest ${BASIC_FILTERS_TESTS} itkHoughTransform2DLinesImageTest)
add_test(itkImageAdaptorNthElementTest ${BASIC_FILTERS_TESTS} itkImageAdaptorNthElementTest)
add_test(itkImageAdaptorPipeLineTest ${BASIC_FILTERS_TESTS} itkImageAdaptorPipeLineTest)
add_test(itkImageToParametricSpaceFilterTest ${BASIC_FILTERS_TESTS} itkImageToParametricSpaceFilterTest)
add_test(itkImageToMeshFilterTest ${BASIC_FILTERS_TESTS} itkImageToMeshFilterTest)
add_test(itkImplicitManifoldNormalVectorFilterTest ${BASIC_FILTERS_TESTS} itkImplicitManifoldNormalVectorFilterTest)
add_test(itkImportImageTest ${BASIC_FILTERS_TESTS} itkImportImageTest)
add_test(itkIntensityWindowingImageFilterTest ${BASIC_FILTERS_TESTS} itkIntensityWindowingImageFilterTest )
add_test(itkInteriorExteriorMeshFilterTest ${BASIC_FILTERS_TESTS} itkInteriorExteriorMeshFilterTest )
add_test(itkInterpolateImageFilterTest ${BASIC_FILTERS_TESTS} itkInterpolateImageFilterTest )
add_test(itkInterpolateImagePointsFilterTest ${BASIC_FILTERS_TESTS} itkInterpolateImagePointsFilterTest )
add_test(itkJoinImageFilterTest ${BASIC_FILTERS_TESTS} itkJoinImageFilterTest)
add_test(itkJoinSeriesImageFilterTest ${BASIC_FILTERS_TESTS2} itkJoinSeriesImageFilterTest)
add_test(itkJoinSeriesImageFilterStreamingTest ${BASIC_FILTERS_TESTS2}
  --compare ${INPUTDATA}/HeadMRVolume.mhd
            ${TEMP}/itkJoinSeriesImageFilterStreamingTest.mha
  itkJoinSeriesImageFilterStreamingTest
            ${INPUTDATA}/HeadMRVolume.mhd
            ${TEMP}/itkJoinSeriesImageFilterStreamingTest.mha
   )
add_test(itkLaplacianImageFilterTest ${BASIC_FILTERS_TESTS} itkLaplacianImageFilterTest)
add_test(itkLog10ImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkLog10ImageFilterAndAdaptorTest)
add_test(itkLogImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS} itkLogImageFilterAndAdaptorTest)

add_test(itkMaskImageFilterTest ${BASIC_FILTERS_TESTS} itkMaskImageFilterTest)
add_test(itkPolylineMaskImageFilterTest ${BASIC_FILTERS_TESTS3} itkPolylineMaskImageFilterTest)

add_test(itkPolylineMask2DImageFilterTest
  ${BASIC_FILTERS_TESTS3} itkPolylineMask2DImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkPolylineMask2DImageFilterTest.png
  )

add_test(itkMaskNegatedImageFilterTest ${BASIC_FILTERS_TESTS} itkMaskNegatedImageFilterTest)
add_test(itkMathematicalMorphologyImageFilterTest ${BASIC_FILTERS_TESTS} itkMathematicalMorphologyImageFilterTest)
add_test(itkMaximumImageFilterTest ${BASIC_FILTERS_TESTS} itkMaximumImageFilterTest)
add_test(itkMeanImageFilterTest ${BASIC_FILTERS_TESTS} itkMeanImageFilterTest)
add_test(itkMedianImageFilterTest ${BASIC_FILTERS_TESTS} itkMedianImageFilterTest)
add_test(itkMinimumImageFilterTest ${BASIC_FILTERS_TESTS} itkMinimumImageFilterTest)
add_test(itkMinimumMaximumImageCalculatorTest ${BASIC_FILTERS_TESTS} itkMinimumMaximumImageCalculatorTest)
add_test(itkMinimumMaximumImageFilterTest ${BASIC_FILTERS_TESTS} itkMinimumMaximumImageFilterTest)
add_test(itkMirrorPadImageTest ${BASIC_FILTERS_TESTS} itkMirrorPadImageTest)
add_test(itkMultiplyImageFilterTest ${BASIC_FILTERS_TESTS} itkMultiplyImageFilterTest)
add_test(itkNaryAddImageFilterTest ${BASIC_FILTERS_TESTS1} itkNaryAddImageFilterTest)
add_test(itkNaryMaximumImageFilterTest ${BASIC_FILTERS_TESTS1} itkNaryMaximumImageFilterTest)
add_test(itkNarrowBandTest ${BASIC_FILTERS_TESTS1} itkNarrowBandTest)
#
# The next test uses itkRandomSource which does not produce deterministic
# results on 64 bit systems. No regression is performed for 64 bit systems.
#
if(${CMAKE_SIZEOF_LONG} EQUAL 4)
  add_test(itkNarrowBandImageFilterBaseTest ${BASIC_FILTERS_TESTS1}
    --compare ${BASELINE}/itkNarrowBandImageFilterBaseTest.png
              ${TEMP}/itkNarrowBandImageFilterBaseTest.png
    --compareNumberOfPixelsTolerance 5
    itkNarrowBandImageFilterBaseTest
               ${TEMP}/itkNarrowBandImageFilterBaseTest.png)
else(${CMAKE_SIZEOF_LONG} EQUAL 4)
  add_test(itkNarrowBandImageFilterBaseTest ${BASIC_FILTERS_TESTS1}
    itkNarrowBandImageFilterBaseTest
               ${TEMP}/itkNarrowBandImageFilterBaseTest.png)
endif(${CMAKE_SIZEOF_LONG} EQUAL 4)

add_test(itkNeighborhoodOperatorImageFilterTest ${BASIC_FILTERS_TESTS1} itkNeighborhoodOperatorImageFilterTest)
add_test(itkNoiseImageFilterTest ${BASIC_FILTERS_TESTS1}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkNoiseImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/itkNoiseImageFilterTest.png
   itkNoiseImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkNoiseImageFilterTest.png)
add_test(itkNonThreadedShrinkImageTest ${BASIC_FILTERS_TESTS1} itkNonThreadedShrinkImageTest)
add_test(itkNormalizeImageFilterTest ${BASIC_FILTERS_TESTS1} itkNormalizeImageFilterTest)
add_test(itkNormalizedCorrelationImageFilterTest ${BASIC_FILTERS_TESTS3}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/NormalizedCorrelationImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/NormalizedCorrelationImageFilterTest.png
             itkNormalizedCorrelationImageFilterTest
             ${ITK_DATA_ROOT}/Input/sf4.png ${ITK_DATA_ROOT}/Input/circle.png ${ITK_TEST_OUTPUT_DIR}/NormalizedCorrelationImageFilterTest.png)
add_test(itkObjectMorphologyImageFilterTest ${BASIC_FILTERS_TESTS1} itkObjectMorphologyImageFilterTest)
add_test(itkOrImageFilterTest ${BASIC_FILTERS_TESTS1} itkOrImageFilterTest)
add_test(itkParallelSparseFieldLevelSetImageFilterTest ${BASIC_FILTERS_TESTS1}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ParallelSparseFieldLevelSetImageFilterTest.mha
             ${ITK_TEST_OUTPUT_DIR}/ParallelSparseFieldLevelSetImageFilterTest.mha
              itkParallelSparseFieldLevelSetImageFilterTest
              ${ITK_TEST_OUTPUT_DIR}/ParallelSparseFieldLevelSetImageFilterTest.mha)
add_test(itkPathToChainCodePathFilterTest ${BASIC_FILTERS_TESTS1} itkPathToChainCodePathFilterTest)
add_test(itkPathToImageFilterTest ${BASIC_FILTERS_TESTS1} itkPathToImageFilterTest)
add_test(itkPermuteAxesImageFilterTest ${BASIC_FILTERS_TESTS1} itkPermuteAxesImageFilterTest)
add_test(itkRGBToVectorAdaptImageFilterTest.cxx ${BASIC_FILTERS_TESTS1} itkRGBToVectorAdaptImageFilterTest)
add_test(itkRGBToLuminanceImageFilterAndAdaptorTest.cxx ${BASIC_FILTERS_TESTS1} itkRGBToLuminanceImageFilterAndAdaptorTest)
add_test(itkRecursiveGaussianImageFiltersTest ${BASIC_FILTERS_TESTS1} itkRecursiveGaussianImageFiltersTest)
add_test(itkRecursiveGaussianImageFiltersOnTensorsTest ${BASIC_FILTERS_TESTS1} itkRecursiveGaussianImageFiltersOnTensorsTest)
add_test(itkReflectImageFilterTest ${BASIC_FILTERS_TESTS1} itkReflectImageFilterTest)
add_test(itkReflectiveImageRegionIteratorTest ${BASIC_FILTERS_TESTS1}  itkReflectiveImageRegionIteratorTest )
add_test(itkRegionOfInterestImageFilterTest ${BASIC_FILTERS_TESTS1}  itkRegionOfInterestImageFilterTest )
add_test(itkResampleImageTest ${BASIC_FILTERS_TESTS1} itkResampleImageTest)
add_test(itkResampleImageTest2 ${BASIC_FILTERS_TESTS1}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ResampleImageTest2.png
             ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2.png
             itkResampleImageTest2
             ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/circle.png ${ITK_TEST_OUTPUT_DIR}/ResampleImageTest2.png)
add_test(itkResamplePhasedArray3DSpecialCoordinatesImageTest ${BASIC_FILTERS_TESTS1} itkResamplePhasedArray3DSpecialCoordinatesImageTest)
add_test(itkRescaleIntensityImageFilterTest ${BASIC_FILTERS_TESTS1} itkRescaleIntensityImageFilterTest)
add_test(itkShiftScaleImageFilterTest ${BASIC_FILTERS_TESTS2} itkShiftScaleImageFilterTest)
add_test(itkShiftScaleInPlaceImageFilterTest ${BASIC_FILTERS_TESTS2} itkShiftScaleInPlaceImageFilterTest)
add_test(itkShrinkImageTest ${BASIC_FILTERS_TESTS2} itkShrinkImageTest)
add_test(itkShrinkImagePreserveObjectPhysicalLocations ${BASIC_FILTERS_TESTS2} itkShrinkImagePreserveObjectPhysicalLocations)
add_test(itkShrinkImageStreamingTest ${BASIC_FILTERS_TESTS2} itkShrinkImageStreamingTest)
add_test(itkSigmoidImageFilterTest ${BASIC_FILTERS_TESTS2} itkSigmoidImageFilterTest)
add_test(itkSimilarityIndexImageFilterTest ${BASIC_FILTERS_TESTS2} itkSimilarityIndexImageFilterTest)
add_test(itkSimplexMeshToTriangleMeshFilterTest ${BASIC_FILTERS_TESTS2} itkSimplexMeshToTriangleMeshFilterTest)
add_test(itkSimplexMeshAdaptTopologyFilterTest ${BASIC_FILTERS_TESTS2} itkSimplexMeshAdaptTopologyFilterTest)
add_test(itkSinImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS2} itkSinImageFilterAndAdaptorTest)
add_test(itkSobelEdgeDetectionImageFilterTest ${BASIC_FILTERS_TESTS2} itkSobelEdgeDetectionImageFilterTest)
add_test(itkSmoothingRecursiveGaussianImageFilterTest ${BASIC_FILTERS_TESTS2} itkSmoothingRecursiveGaussianImageFilterTest)
add_test(itkSparseFieldFourthOrderLevelSetImageFilterTest ${BASIC_FILTERS_TESTS2} itkSparseFieldFourthOrderLevelSetImageFilterTest)
add_test(itkSparseFieldLayerTest ${BASIC_FILTERS_TESTS2} itkSparseFieldLayerTest)
add_test(itkSpatialObjectToImageFilterTest ${BASIC_FILTERS_TESTS2} itkSpatialObjectToImageFilterTest)
add_test(itkSpatialObjectToImageStatisticsCalculatorTest ${BASIC_FILTERS_TESTS2} itkSpatialObjectToImageStatisticsCalculatorTest)
add_test(itkSpatialObjectToPointSetFilterTest ${BASIC_FILTERS_TESTS2} itkSpatialObjectToPointSetFilterTest)
add_test(itkSpatialFunctionImageEvaluatorFilterTest ${BASIC_FILTERS_TESTS2} itkSpatialFunctionImageEvaluatorFilterTest)
add_test(itkSqrtImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS2} itkSqrtImageFilterAndAdaptorTest)
add_test(itkSquareImageFilterTest ${BASIC_FILTERS_TESTS2} itkSquareImageFilterTest)
add_test(itkSquaredDifferenceImageFilterTest ${BASIC_FILTERS_TESTS2} itkSquaredDifferenceImageFilterTest)
add_test(itkStatisticsImageFilterTest ${BASIC_FILTERS_TESTS2} itkStatisticsImageFilterTest)
add_test(itkStreamingImageFilterTest ${BASIC_FILTERS_TESTS2} itkStreamingImageFilterTest)
add_test(itkStreamingImageFilterTest2 ${BASIC_FILTERS_TESTS2} itkStreamingImageFilterTest2)
# MANTIS BUG 8524
# these test demostrate this bug, enable these test when patch is applied
#add_test(itkStreamingImageFilterTest3_1 ${BASIC_FILTERS_TESTS2}
#  --compare ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png
#  ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_1.png
#  itkStreamingImageFilterTest3
#  ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_1.png 10)
#add_test(itkStreamingImageFilterTest3_2 ${BASIC_FILTERS_TESTS2}
#  --compare ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png
#  ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_2.png
#  itkStreamingImageFilterTest3
#  ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_2.png 1000)
add_test(itkSubtractImageFilterTest ${BASIC_FILTERS_TESTS2} itkSubtractImageFilterTest)
add_test(itkTanImageFilterAndAdaptorTest ${BASIC_FILTERS_TESTS2} itkTanImageFilterAndAdaptorTest)
add_test(itkTernaryMagnitudeImageFilterTest ${BASIC_FILTERS_TESTS2} itkTernaryMagnitudeImageFilterTest)
add_test(itkTernaryMagnitudeSquaredImageFilterTest ${BASIC_FILTERS_TESTS2} itkTernaryMagnitudeSquaredImageFilterTest)
add_test(itkThresholdImageFilterTest ${BASIC_FILTERS_TESTS2} itkThresholdImageFilterTest)
add_test(itkThresholdLabelerImageFilterTest ${BASIC_FILTERS_TESTS2} itkThresholdLabelerImageFilterTest)
add_test(itkTransformMeshFilterTest ${BASIC_FILTERS_TESTS2} itkTransformMeshFilterTest)
add_test(itkTriangleMeshToSimplexMeshFilterTest ${BASIC_FILTERS_TESTS2} itkTriangleMeshToSimplexMeshFilterTest)
add_test(itkTriangleMeshToSimplexMeshFilter2Test ${BASIC_FILTERS_TESTS2} itkTriangleMeshToSimplexMeshFilter2Test)
add_test(itkTwoOutputExampleImageFilterTest ${BASIC_FILTERS_TESTS2} itkTwoOutputExampleImageFilterTest)
add_test(itkVectorAnisotropicDiffusionImageFilterTest ${BASIC_FILTERS_TESTS3} itkVectorAnisotropicDiffusionImageFilterTest)
add_test(itkVectorExpandImageFilterTest ${BASIC_FILTERS_TESTS3} itkVectorExpandImageFilterTest)
add_test(itkVectorNeighborhoodOperatorImageFilterTest ${BASIC_FILTERS_TESTS3} itkVectorNeighborhoodOperatorImageFilterTest)
add_test(itkVectorRescaleIntensityImageFilterTest ${BASIC_FILTERS_TESTS3} itkVectorRescaleIntensityImageFilterTest)
add_test(itkWarpImageFilterTest ${BASIC_FILTERS_TESTS3} itkWarpImageFilterTest)
add_test(itkWarpMeshFilterTest ${BASIC_FILTERS_TESTS3} itkWarpMeshFilterTest)
add_test(itkWarpVectorImageFilterTest ${BASIC_FILTERS_TESTS3} itkWarpVectorImageFilterTest)
add_test(itkWeightedAddImageFilterTest ${BASIC_FILTERS_TESTS3} itkWeightedAddImageFilterTest)
add_test(itkWrapPadImageTest ${BASIC_FILTERS_TESTS3} itkWrapPadImageTest)
add_test(itkXorImageFilterTest ${BASIC_FILTERS_TESTS3} itkXorImageFilterTest)
add_test(itkZeroCrossingBasedEdgeDetectionImageFilterTest ${BASIC_FILTERS_TESTS3} itkZeroCrossingBasedEdgeDetectionImageFilterTest)
add_test(itkZeroCrossingImageFilterTest ${BASIC_FILTERS_TESTS3} itkZeroCrossingImageFilterTest)

add_test(itkOrientImageFilterTest ${BASIC_FILTERS_TESTS1} itkOrientImageFilterTest)
add_test(itkOrientImageFilterTest2 ${BASIC_FILTERS_TESTS1} itkOrientImageFilterTest2)

add_test(itkTriangleMeshToBinaryImageFilterTest ${BASIC_FILTERS_TESTS3}
  itkTriangleMeshToBinaryImageFilterTest
  ${TEMP}/itkTriangleMeshToBinaryImageFilterTest.mha
  )

add_test(itkTriangleMeshToBinaryImageFilterTest2 ${BASIC_FILTERS_TESTS3}
  itkTriangleMeshToBinaryImageFilterTest2
  ${TEMP}/itkTriangleMeshToBinaryImageFilterTest2.mha
  )

add_test(itkNotImageFilterTest ${BASIC_FILTERS_TESTS4}
         itkNotImageFilterTest)

if(ITK_DATA_ROOT)
add_test(itkAdaptiveHistogramEqualizationImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AdaptiveHistogramEqualizationImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest.png
  itkAdaptiveHistogramEqualizationImageFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest.png 10 0.5 0.5)

add_test(itkAccumulateImageFilterTest ${BASIC_FILTERS_TESTS3}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AccumulateImageFilterTest.png

             ${ITK_TEST_OUTPUT_DIR}/AccumulateImageFilterTest.png
             itkAccumulateImageFilterTest ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR}/AccumulateImageFilterTest.png)

add_test(itkAdaptiveHistogramEqualizationImageFilterTest2 ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AdaptiveHistogramEqualizationImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest2.png
  itkAdaptiveHistogramEqualizationImageFilterTest
            ${ITK_DATA_ROOT}/Input/sf4.png
            ${ITK_TEST_OUTPUT_DIR}/AdaptiveHistogramEqualizationImageFilterTest2.png 10 1.0 0.25)

add_test(itkBilateralImageFilterTest2 ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/BilateralImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest2.png
  itkBilateralImageFilterTest2
            ${ITK_DATA_ROOT}/Input/cake_easy.png
            ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest2.png)

add_test(itkBilateralImageFilterTest3 ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/BilateralImageFilterTest3.png
            ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest3.png
  itkBilateralImageFilterTest3
            ${ITK_DATA_ROOT}/Input/cake_easy.png
            ${ITK_TEST_OUTPUT_DIR}/BilateralImageFilterTest3.png)

add_test(itkBinaryThresholdImageFilterTest2 ${BASIC_FILTERS_TESTS}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/BinaryThresholdImageFilterTest2.png
             ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png
  itkBinaryThresholdImageFilterTest2
             ${ITK_DATA_ROOT}/Input/cthead1.png  ${ITK_DATA_ROOT}/Input/DicomSeries/Image0076.dcm
             ${ITK_TEST_OUTPUT_DIR}/BinaryThresholdImageFilterTest2.png )

add_test(itkConfidenceConnectedImageFilterTest ${BASIC_FILTERS_TESTS}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConfidenceConnectedImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/ConfidenceConnectedImageFilterTest.png
  itkConfidenceConnectedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/ConfidenceConnectedImageFilterTest.png
             165 165)

add_test(itkConnectedComponentImageFilterTooManyObjectsTest ${BASIC_FILTERS_TESTS}
  itkConnectedComponentImageFilterTooManyObjectsTest)

add_test(itkConnectedComponentImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest.png
  itkConnectedComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest.png 130 145)

add_test(itkConnectedComponentImageFilterTest2 ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest2.png
  itkConnectedComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/Shapes.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest2.png 128 255 0)

add_test(itkConnectedComponentImageFilterTest3 ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest3.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest3.png
  itkConnectedComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/Shapes.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTest3.png 128 255 1)

add_test(itkConnectedComponentImageFilterTestRGB ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedComponentImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTestRGB.png
  itkConnectedComponentImageFilterTestRGB
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedComponentImageFilterTestRGB.png 130 145)

add_test(itkMaskConnectedComponentImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/MaskConnectedComponentImageFilterTest.png ${ITK_TEST_OUTPUT_DIR}/MaskConnectedComponentImageFilterTest.png
  itkMaskConnectedComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/MaskConnectedComponentImageFilterTest.png 130 145)

add_test(itkScalarConnectedComponentImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ScalarConnectedComponentImageFilterTest.png ${ITK_TEST_OUTPUT_DIR}/ScalarConnectedComponentImageFilterTest.png
  itkScalarConnectedComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/ScalarConnectedComponentImageFilterTest.png 20 1)

add_test(itkConnectedThresholdImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedThresholdImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest.png
  itkConnectedThresholdImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest.png
            165 165 0 150 )

# Exercise the fully connected feature in the connected threshold filter
if(ITK_USE_REVIEW)
add_test(itkConnectedThresholdImageFilterTest2 ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ConnectedThresholdImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest2.png
  itkConnectedThresholdImageFilterTest
            ${ITK_DATA_ROOT}/Input/8ConnectedImage.bmp
            ${ITK_TEST_OUTPUT_DIR}/ConnectedThresholdImageFilterTest2.png
            29 47 200 255 1)
endif(ITK_USE_REVIEW)

add_test(itkDoubleThresholdImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/DoubleThresholdImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest.png itkDoubleThresholdImageFilterTest
            ${ITK_DATA_ROOT}/../../Examples/Data/BrainProtonDensitySlice.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest.png 217 246 255 255)

add_test(itkDoubleThresholdImageFilterTest2 ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/DoubleThresholdImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest2.png itkDoubleThresholdImageFilterTest
            ${ITK_DATA_ROOT}/../../Examples/Data/BrainProtonDensitySlice.png
            ${ITK_TEST_OUTPUT_DIR}/DoubleThresholdImageFilterTest2.png 150 164 164 180)

add_test(itkGradientAnisotropicDiffusionImageFilterTest2 ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GradientAnisotropicDiffusionImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterTest2.png
  itkGradientAnisotropicDiffusionImageFilterTest2
            ${ITK_DATA_ROOT}/Input/cake_easy.png
            ${ITK_TEST_OUTPUT_DIR}/GradientAnisotropicDiffusionImageFilterTest2.png)

add_test(itkGrayscaleMorphologicalClosingImageFilterTest ${BASIC_FILTERS_TESTS2}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleMorphologicalClosingImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalClosingImageFilterTest.png
   itkGrayscaleMorphologicalClosingImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalClosingImageFilterTest.png )

add_test(itkGrayscaleMorphologicalOpeningImageFilterTest ${BASIC_FILTERS_TESTS2}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleMorphologicalOpeningImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalOpeningImageFilterTest.png
   itkGrayscaleMorphologicalOpeningImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleMorphologicalOpeningImageFilterTest.png )

add_test(itkSimpleContourExtractorImageFilterTest ${BASIC_FILTERS_TESTS2}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/SimpleContourExtractorImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/SimpleContourExtractorImageFilterTest.png
   itkSimpleContourExtractorImageFilterTest
               ${ITK_DATA_ROOT}/Baseline/BasicFilters/BinaryThresholdImageFilterTest2.png
               ${ITK_TEST_OUTPUT_DIR}/SimpleContourExtractorImageFilterTest.png )

add_test(itkGrayscaleFillholeImageFilterTest ${BASIC_FILTERS_TESTS}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleFillholeImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleFillholeImageFilterTest.png itkGrayscaleFillholeImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleFillholeImageFilterTest.png )

add_test(itkGrayscaleConnectedClosingImageFilterTest ${BASIC_FILTERS_TESTS}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedClosingImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest.png itkGrayscaleConnectedClosingImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest.png
               174 214)

add_test(itkGrayscaleConnectedClosingImageFilterTest2 ${BASIC_FILTERS_TESTS}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedClosingImageFilterTest2.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest2.png itkGrayscaleConnectedClosingImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedClosingImageFilterTest2.png
               150 169)

add_test(itkGrayscaleConnectedOpeningImageFilterTest ${BASIC_FILTERS_TESTS}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedOpeningImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest.png itkGrayscaleConnectedOpeningImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest.png 174 214)

add_test(itkGrayscaleConnectedOpeningImageFilterTest2 ${BASIC_FILTERS_TESTS}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/GrayscaleConnectedOpeningImageFilterTest2.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest2.png itkGrayscaleConnectedOpeningImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/GrayscaleConnectedOpeningImageFilterTest2.png 150 169)

add_test(itkHConvexConcaveImageFilterTest ${BASIC_FILTERS_TESTS}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HConvexConcaveImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/HConvexConcaveImageFilterTest.png itkHConvexConcaveImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/HConvexConcaveImageFilterTest.png 50)

add_test(itkHMaximaMinimaImageFilterTest ${BASIC_FILTERS_TESTS}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest.png itkHMaximaMinimaImageFilterTest
               ${ITK_DATA_ROOT}/Input/cake_easy.png
               ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest.png 35)

add_test(itkHMaximaMinimaImageFilterTest2 ${BASIC_FILTERS_TESTS}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest2.png
               ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest2.png itkHMaximaMinimaImageFilterTest
               ${ITK_DATA_ROOT}/Input/cake_hard.png
               ${ITK_TEST_OUTPUT_DIR}/HMaximaMinimaImageFilterTest2.png 35)

add_test(itkIsolatedConnectedImageFilterTest ${BASIC_FILTERS_TESTS}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/IsolatedConnectedImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest.png
  itkIsolatedConnectedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest.png
             true
             115 85 107 110)

add_test(itkIsolatedConnectedImageFilterTest2 ${BASIC_FILTERS_TESTS}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/IsolatedConnectedImageFilterTest2.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest2.png
  itkIsolatedConnectedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/IsolatedConnectedImageFilterTest2.png
             false
             175 125 100 170 176 125 101 170)

add_test(itkLaplacianRecursiveGaussianImageFilterTest ${BASIC_FILTERS_TESTS}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/LaplacianRecursiveGaussianImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/LaplacianRecursiveGaussianImageFilterTest.png
  itkLaplacianRecursiveGaussianImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/LaplacianRecursiveGaussianImageFilterTest.png)

add_test(itkNeighborhoodConnectedImageFilterTest ${BASIC_FILTERS_TESTS1}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/NeighborhoodConnectedImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/NeighborhoodConnectedImageFilterTest.png
  itkNeighborhoodConnectedImageFilterTest
             ${ITK_DATA_ROOT}/Input/cthead1.png
             ${ITK_TEST_OUTPUT_DIR}/NeighborhoodConnectedImageFilterTest.png
             146 167)

add_test(itkExtractOrthogonalSwath2DImageFilterTest ${BASIC_FILTERS_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ExtractOrthogonalSwath2DImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/ExtractOrthogonalSwath2DImageFilterTest.png
  itkExtractOrthogonalSwath2DImageFilterTest
            ${ITK_TEST_OUTPUT_DIR}/ExtractOrthogonalSwath2DImageFilterTest.png
)

add_test(itkBlackTopHatImageFilterTest ${BASIC_FILTERS_TESTS2}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkBlackTopHatImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/itkBlackTopHatImageFilterTest.png
   itkTopHatImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/itkBlackTopHatImageFilterTest.png
               0 3)

add_test(itkWhiteTopHatImageFilterTest ${BASIC_FILTERS_TESTS2}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/itkWhiteTopHatImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/itkWhiteTopHatImageFilterTest.png
   itkTopHatImageFilterTest
               ${ITK_DATA_ROOT}/Input/cthead1.png
               ${ITK_TEST_OUTPUT_DIR}/itkWhiteTopHatImageFilterTest.png
               1 3)

add_test(itkTileImageFilterTest ${BASIC_FILTERS_TESTS2}
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

add_test(itkPushPopTileImageFilterTest ${BASIC_FILTERS_TESTS2}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PushPopTileImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/PushPopTileImageFilterTest.png
  itkPushPopTileImageFilterTest
            ${ITK_DATA_ROOT}/Input/Shapes.png
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_DATA_ROOT}/Input/cake_easy.png
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/PushPopTileImageFilterTest.png)



add_test(itkPasteImageFilterTest ${BASIC_FILTERS_TESTS1}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PasteImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/PasteImageFilterTest.png
  itkPasteImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_DATA_ROOT}/Input/cake_easy.png
            ${ITK_TEST_OUTPUT_DIR}/PasteImageFilterTest.png)

add_test(itkPromoteDimensionImageTest ${BASIC_FILTERS_TESTS1}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/PromoteDimensionImageTest.mhd
            ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd
  itkPromoteDimensionImageTest
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/PromoteDimensionImageTest.mhd)

add_test(itkRelabelComponentImageFilterTest ${BASIC_FILTERS_TESTS1}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RelabelComponentImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/RelabelComponentImageFilterTest.png
  itkRelabelComponentImageFilterTest
            ${ITK_DATA_ROOT}/Input/cthead1.png
            ${ITK_TEST_OUTPUT_DIR}/RelabelComponentImageFilterTest.png 130 145)

add_test(itkRemoveBoundaryObjectsTest ${BASIC_FILTERS_TESTS1}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RemoveBoundaryObjectsTest.png
            ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest.png
  itkRemoveBoundaryObjectsTest
            ${ITK_DATA_ROOT}/Input/Spots.png
            ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest.png)

add_test(itkRemoveBoundaryObjectsTest2 ${BASIC_FILTERS_TESTS1}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/RemoveBoundaryObjectsTest2.png
            ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest2.png
  itkRemoveBoundaryObjectsTest2
            ${ITK_DATA_ROOT}/Input/SpotsInverted.png
            ${ITK_TEST_OUTPUT_DIR}/RemoveBoundaryObjectsTest2.png)

add_test(itkTobogganImageFilterTest ${BASIC_FILTERS_TESTS2}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/TobogganImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/TobogganImageFilterTest.png
         itkTobogganImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_TEST_OUTPUT_DIR}/TobogganImageFilterTest.png)

add_test(itkVectorConfidenceConnectedImageFilterTest ${BASIC_FILTERS_TESTS3}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorConfidenceConnectedImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest.png
  itkVectorConfidenceConnectedImageFilterTest
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest.png
            72 44 23 67 5.0 6)

add_test(itkVectorGradientMagnitudeImageFilterTest1a ${BASIC_FILTERS_TESTS3}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1a.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png
  itkVectorGradientMagnitudeImageFilterTest1
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png
            0)

add_test(itkVectorGradientMagnitudeImageFilterTest1b ${BASIC_FILTERS_TESTS3}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1b.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png
  itkVectorGradientMagnitudeImageFilterTest1
            ${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png
            1)

add_test(itkVectorGradientMagnitudeImageFilterTest2 ${BASIC_FILTERS_TESTS3}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png
  itkVectorGradientMagnitudeImageFilterTest2
            ${ITK_DATA_ROOT}/Input/VHFColor.mhd
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png
            5 0)

add_test(itkVectorGradientMagnitudeImageFilterTest2b ${BASIC_FILTERS_TESTS3}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2b.png
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png
  itkVectorGradientMagnitudeImageFilterTest2
            ${ITK_DATA_ROOT}/Input/VHFColor.mhd
            ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png
            5 1)

add_test(itkVectorResampleImageFilterTest ${BASIC_FILTERS_TESTS3}
  --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorResampleImageFilterTest.png
            ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png
  itkVectorResampleImageFilterTest
            ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png)

add_test(itkDivideImageFilterTest2 ${BASIC_FILTERS_TESTS4} itkDivideImageFilterTest2)

add_test(itkGrayscaleGeodesicErodeDilateImageFilterTest ${BASIC_FILTERS_TESTS4}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/HMaximaMinimaImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleGeodesicErodeDilateImageFilterTest.png
   itkGrayscaleGeodesicErodeDilateImageFilterTest
               ${ITK_DATA_ROOT}/Input/cake_easy.png
               ${ITK_TEST_OUTPUT_DIR}/itkGrayscaleGeodesicErodeDilateImageFilterTest.png 35)

add_test(itkClosingByReconstructionImageFilterTest ${BASIC_FILTERS_TESTS4}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ClosingByReconstructionImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTest.png
   itkClosingByReconstructionImageFilterTest
               ${ITK_DATA_ROOT}/Input/closerec1.jpg
               ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTest.png
               4
               0
               ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTestSubtract.png
)

add_test(itkClosingByReconstructionImageFilterTest2 ${BASIC_FILTERS_TESTS4}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ClosingByReconstructionImageFilterTest2.png
               ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTest2.png
   itkClosingByReconstructionImageFilterTest
               ${ITK_DATA_ROOT}/Input/closerec1.jpg
               ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTest2.png
               4
               1
               ${ITK_TEST_OUTPUT_DIR}/ClosingByReconstructionImageFilterTestSubtract2.png
)

add_test(itkOpeningByReconstructionImageFilterTest ${BASIC_FILTERS_TESTS4}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/OpeningByReconstructionImageFilterTest.png
               ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTest.png
   itkOpeningByReconstructionImageFilterTest
               ${ITK_DATA_ROOT}/Input/chondt.png
               ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTest.png
               4
               0
               ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTestSubtract.png
               )

add_test(itkOpeningByReconstructionImageFilterTest2 ${BASIC_FILTERS_TESTS4}
     --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/OpeningByReconstructionImageFilterTest2.png
               ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTest2.png
   itkOpeningByReconstructionImageFilterTest
               ${ITK_DATA_ROOT}/Input/chondt.png
               ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTest2.png
               4
               1
               ${ITK_TEST_OUTPUT_DIR}/OpeningByReconstructionImageFilterTestSubtract2.png
               )

add_test(itkGetAverageSliceImageFilterTest ${BASIC_FILTERS_TESTS4}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/AccumulateImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/GetAverageSliceImageFilterTest.png
             itkGetAverageSliceImageFilterTest ${ITK_DATA_ROOT}/Input/DicomSeries ${ITK_TEST_OUTPUT_DIR}/GetAverageSliceImageFilterTest.png)

add_test(itkMaskNeighborhoodOperatorImageFilterTest ${BASIC_FILTERS_TESTS4}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/MaskNeighborhoodOperatorImageFilterTest.png
             ${ITK_TEST_OUTPUT_DIR}/MaskNeighborhoodOperatorImageFilterTest.png
             itkMaskNeighborhoodOperatorImageFilterTest
                         ${INPUTDATA}/cthead1.png ${ITK_TEST_OUTPUT_DIR}/MaskNeighborhoodOperatorImageFilterTest.png)

add_test(itkVectorConnectedComponentImageFilterTest ${BASIC_FILTERS_TESTS4}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorConnectedComponentImageFilterTest.png
         ${ITK_TEST_OUTPUT_DIR}/VectorConnectedComponentImageFilterTest.png
         itkVectorConnectedComponentImageFilterTest
             ${ITK_TEST_OUTPUT_DIR}/VectorConnectedComponentImageFilterTest.png)

add_test(itkMatrixIndexSelectionImageFilterTest ${BASIC_FILTERS_TESTS4}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/MatrixIndexSelectionImageFilterTest.png
         ${ITK_TEST_OUTPUT_DIR}/MatrixIndexSelectionImageFilterTest.png
         itkMatrixIndexSelectionImageFilterTest
             ${ITK_TEST_OUTPUT_DIR}/MatrixIndexSelectionImageFilterTest.png)

add_test(itkInvertIntensityImageFilterTest ${BASIC_FILTERS_TESTS4}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/InvertIntensityImageFilterTest.png
         ${ITK_TEST_OUTPUT_DIR}/InvertIntensityImageFilterTest.png
         itkInvertIntensityImageFilterTest
         ${INPUTDATA}/cthead1.png
         ${ITK_TEST_OUTPUT_DIR}/InvertIntensityImageFilterTest.png
         )

add_test(itkModulusImageFilterTest ${BASIC_FILTERS_TESTS4}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/ModulusImageFilterTest.png
         ${ITK_TEST_OUTPUT_DIR}/ModulusImageFilterTest.png
         itkModulusImageFilterTest
         ${INPUTDATA}/Spots.png
         ${ITK_TEST_OUTPUT_DIR}/ModulusImageFilterTest.png
         )

add_test(itkMorphologicalGradientImageFilterTest ${BASIC_FILTERS_TESTS4}
   --compare ${ITK_DATA_ROOT}/Baseline/BasicFilters/MorphologicalGradientImageFilterTest.png
         ${ITK_TEST_OUTPUT_DIR}/MorphologicalGradientImageFilterTest.png
         itkMorphologicalGradientImageFilterTest
         ${INPUTDATA}/cthead1.png
         ${ITK_TEST_OUTPUT_DIR}/MorphologicalGradientImageFilterTest.png
         )

add_test( itkSignedMaurerDistanceMapImageFilterTest1 ${BASIC_FILTERS_TESTS4}
  --compare ${BASELINE}/itkSignedMaurerDistanceMapImageFilterTest1.mhd
            ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest1.mhd
          itkSignedMaurerDistanceMapImageFilterTest
          ${INPUTDATA}/SquareBinary201.png
          ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest1.mhd)

add_test( itkSignedMaurerDistanceMapImageFilterTest2 ${BASIC_FILTERS_TESTS4}
  --compare ${BASELINE}/itkSignedMaurerDistanceMapImageFilterTest2.mhd
            ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest2.mhd
          itkSignedMaurerDistanceMapImageFilterTest
          ${INPUTDATA}/BrainSliceBinary.png
          ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest2.mhd)

add_test( itkSignedMaurerDistanceMapImageFilterTest3 ${BASIC_FILTERS_TESTS4}
  --compare ${BASELINE}/itkSignedMaurerDistanceMapImageFilterTest3.mhd
            ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest3.mhd
          itkSignedMaurerDistanceMapImageFilterTest
          ${INPUTDATA}/LungSliceBinary.png
          ${TEMP}/itkSignedMaurerDistanceMapImageFilterTest3.mhd)

add_test( itkVectorIndexSelectionCastImageFilterTest1 ${BASIC_FILTERS_TESTS4}
  --compare ${BASELINE}/itkVectorIndexSelectionCastImageFilterTest1.mha
            ${TEMP}/itkVectorIndexSelectionCastImageFilterTest1.mha
          itkVectorIndexSelectionCastImageFilterTest
          ${INPUTDATA}/RGBTestImage.tif
          ${TEMP}/itkVectorIndexSelectionCastImageFilterTest1.mha 0)

add_test( itkVectorIndexSelectionCastImageFilterTest2 ${BASIC_FILTERS_TESTS4}
  --compare ${BASELINE}/itkVectorIndexSelectionCastImageFilterTest2.mha
            ${TEMP}/itkVectorIndexSelectionCastImageFilterTest2.mha
          itkVectorIndexSelectionCastImageFilterTest
          ${INPUTDATA}/RGBTestImage.tif
          ${TEMP}/itkVectorIndexSelectionCastImageFilterTest2.mha 1)

add_test( itkVectorIndexSelectionCastImageFilterTest3 ${BASIC_FILTERS_TESTS4}
  --compare ${BASELINE}/itkVectorIndexSelectionCastImageFilterTest3.mha
            ${TEMP}/itkVectorIndexSelectionCastImageFilterTest3.mha
          itkVectorIndexSelectionCastImageFilterTest
          ${INPUTDATA}/RGBTestImage.tif
          ${TEMP}/itkVectorIndexSelectionCastImageFilterTest3.mha 2)

add_test(itkMaximumProjectionImageFilterTest ${BASIC_FILTERS_TESTS5}
        --compare ${BASELINE}/HeadMRVolumeMaximumProjection.tif
                  ${TEMP}/HeadMRVolumeMaximumProjection.tif
                  itkMaximumProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMaximumProjection.tif)

add_test(itkMaximumProjectionImageFilterTest2_1 ${BASIC_FILTERS_TESTS5}
        --compare ${BASELINE}/HeadMRVolumeMaximumProjection0.tif
                  ${TEMP}/HeadMRVolumeMaximumProjection0.tif
                  itkMaximumProjectionImageFilterTest2
                  0
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMaximumProjection0.tif)

add_test(itkMaximumProjectionImageFilterTest2_2 ${BASIC_FILTERS_TESTS5}
        --compare ${BASELINE}/HeadMRVolumeMaximumProjection1.tif
                  ${TEMP}/HeadMRVolumeMaximumProjection1.tif
                  itkMaximumProjectionImageFilterTest2
                  1
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMaximumProjection1.tif)

add_test(itkMaximumProjectionImageFilterTest3_1 ${BASIC_FILTERS_TESTS5}
        --compare ${BASELINE}/HeadMRVolumeMaximumProjection2D0.tif
                  ${TEMP}/HeadMRVolumeMaximumProjection2D0.tif
                  itkMaximumProjectionImageFilterTest3
                  0
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMaximumProjection2D0.tif)

add_test(itkMaximumProjectionImageFilterTest3_2 ${BASIC_FILTERS_TESTS5}
        --compare ${BASELINE}/HeadMRVolumeMaximumProjection2D1.tif
                  ${TEMP}/HeadMRVolumeMaximumProjection2D1.tif
                  itkMaximumProjectionImageFilterTest3
                  1
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMaximumProjection2D1.tif)

add_test(itkMinimumProjectionImageFilterTest ${BASIC_FILTERS_TESTS5}
        --compare ${BASELINE}/HeadMRVolumeMinimumProjection.tif
                  ${TEMP}/HeadMRVolumeMinimumProjection.tif
                  itkMinimumProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMinimumProjection.tif)

add_test(itkMedianProjectionImageFilterTest ${BASIC_FILTERS_TESTS5}
        --compare ${BASELINE}/HeadMRVolumeMedianProjection.tif
                  ${TEMP}/HeadMRVolumeMedianProjection.tif
                  itkMedianProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMedianProjection.tif)

add_test(itkMeanProjectionImageFilterTest ${BASIC_FILTERS_TESTS5}
        --compare ${BASELINE}/HeadMRVolumeMeanProjection.tif
                  ${TEMP}/HeadMRVolumeMeanProjection.tif
                  itkMeanProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeMeanProjection.tif)

add_test(itkStandardDeviationProjectionImageFilterTest ${BASIC_FILTERS_TESTS5}
        --compare ${BASELINE}/HeadMRVolumeStandardDeviationProjection.tif
                  ${TEMP}/HeadMRVolumeStandardDeviationProjection.tif
                  itkStandardDeviationProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeStandardDeviationProjection.tif)

add_test(itkSumProjectionImageFilterTest ${BASIC_FILTERS_TESTS5}
        --compare ${BASELINE}/HeadMRVolumeSumProjection.tif
                  ${TEMP}/HeadMRVolumeSumProjection.tif
                  itkSumProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeSumProjection.tif)

add_test(itkProjectionImageFilterTest1 ${BASIC_FILTERS_TESTS5}
        --compare ${BASELINE}/HeadMRVolumeBinaryProjection100.tif
                  ${TEMP}/HeadMRVolumeProjection100.tif
                  itkProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeProjection100.tif
                  100
                  0)

add_test(itkBinaryProjectionImageFilterTest1 ${BASIC_FILTERS_TESTS5}
        --compare ${BASELINE}/HeadMRVolumeBinaryProjection100.tif
                  ${TEMP}/HeadMRVolumeBinaryProjection100.tif
                  itkBinaryProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeBinaryProjection100.tif
                  100
                  0)

add_test(itkBinaryProjectionImageFilterTest2 ${BASIC_FILTERS_TESTS5}
        --compare ${BASELINE}/HeadMRVolumeBinaryProjection200.tif
                  ${TEMP}/HeadMRVolumeBinaryProjection200.tif
                  itkBinaryProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeBinaryProjection200.tif
                  200
                  0)

add_test(itkBinaryThresholdProjectionImageFilterTest ${BASIC_FILTERS_TESTS5}
        --compare ${BASELINE}/HeadMRVolumeBinaryThresholdProjection.png
                  ${TEMP}/HeadMRVolumeBinaryThresholdProjection.png
                  itkBinaryThresholdProjectionImageFilterTest
                  ${INPUTDATA}/HeadMRVolume.mhd
                  ${TEMP}/HeadMRVolumeBinaryThresholdProjection.png
                  100
                  255
                  0)

add_test(itkImageToVectorImageFilterTest ${BASIC_FILTERS_TESTS5}
        --compare ${BASELINE}/ImageToVectorImageFilterTest.png
                  ${TEMP}/ImageToVectorImageFilterTest.png
                  itkImageToVectorImageFilterTest
                  ${INPUTDATA}/Spots.png
                  ${INPUTDATA}/SpotsInverted.png
                  ${INPUTDATA}/SpotsInverted.png
                  ${TEMP}/ImageToVectorImageFilterTest.png
                  )
endif(ITK_DATA_ROOT)

add_test(itkWarpImageFilterTest2 ${BASIC_FILTERS_TESTS5} itkWarpImageFilterTest2)

add_test(itkSimplexMeshWithFloatCoordRepTest ${BASIC_FILTERS_TESTS5} itkSimplexMeshWithFloatCoordRepTest)

add_test(itkBSplineDownsampleImageFilterTest
  ${BASIC_FILTERS_TESTS5} itkBSplineDownsampleImageFilterTest
  ${INPUTDATA}/HeadMRVolume.mhd
  ${TEMP}/itkBSplineDownsampleImageFilterTest1.mha
  3  # Spline order
  )

add_test(itkBSplineUpsampleImageFilterTest
  ${BASIC_FILTERS_TESTS5} itkBSplineUpsampleImageFilterTest
  ${INPUTDATA}/HeadMRVolume.mhd
  ${TEMP}/itkBSplineUpsampleImageFilterTest1.mha
  3  # Spline order
  )


set(BasicFilters_SRCS
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
itkAtan2ImageFilterTest.cxx
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
itkBinaryDilateImageFilterTest3.cxx
itkBinaryErodeImageFilterTest.cxx
itkBinaryErodeImageFilterTest3.cxx
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
itkCannyEdgeDetectionImageFilterTest2.cxx
itkChainCodeToFourierSeriesPathFilterTest.cxx
itkChangeInformationImageFilterTest.cxx
itkChangeLabelImageFilterTest.cxx
itkComposeRGBImageFilterTest.cxx
itkCompose2DCovariantVectorImageFilterTest.cxx
itkCompose3DCovariantVectorImageFilterTest.cxx
itkCompose2DVectorImageFilterTest.cxx
itkCompose3DVectorImageFilterTest.cxx
itkConfidenceConnectedImageFilterTest.cxx
itkConnectedComponentImageFilterTest.cxx
itkConnectedComponentImageFilterTooManyObjectsTest.cxx
itkConnectedComponentImageFilterTestRGB.cxx
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
itkDiffusionTensor3DReconstructionImageFilterTest.cxx
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
itkGradientRecursiveGaussianFilterTest2.cxx
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
itkMaskConnectedComponentImageFilterTest.cxx
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
itkScalarConnectedComponentImageFilterTest.cxx
)

set(BasicFilters1_SRCS
itkNarrowBandTest.cxx
itkNarrowBandImageFilterBaseTest.cxx
itkNaryAddImageFilterTest.cxx
itkNaryMaximumImageFilterTest.cxx
itkNeighborhoodConnectedImageFilterTest.cxx
itkNeighborhoodOperatorImageFilterTest.cxx
itkNoiseImageFilterTest.cxx
itkNonThreadedShrinkImageTest.cxx
itkNormalizeImageFilterTest.cxx
itkObjectMorphologyImageFilterTest.cxx
itkOrImageFilterTest.cxx
itkOrientImageFilterTest.cxx
itkOrientImageFilterTest2.cxx
itkParallelSparseFieldLevelSetImageFilterTest.cxx
itkPasteImageFilterTest.cxx
itkPathToChainCodePathFilterTest.cxx
itkPathToImageFilterTest.cxx
itkPromoteDimensionImageTest.cxx
itkPermuteAxesImageFilterTest.cxx
itkRGBToVectorAdaptImageFilterTest.cxx
itkRGBToLuminanceImageFilterAndAdaptorTest.cxx
itkRecursiveGaussianImageFiltersTest.cxx
itkRecursiveGaussianImageFiltersOnTensorsTest.cxx
itkReflectImageFilterTest.cxx
itkReflectiveImageRegionIteratorTest.cxx
itkRegionOfInterestImageFilterTest.cxx
itkRelabelComponentImageFilterTest.cxx
itkRemoveBoundaryObjectsTest.cxx
itkRemoveBoundaryObjectsTest2.cxx
itkResampleImageTest.cxx
itkResampleImageTest2.cxx
itkResamplePhasedArray3DSpecialCoordinatesImageTest.cxx
itkRescaleIntensityImageFilterTest.cxx
)

set(BasicFilters2_SRCS
itkComplexToRealFilterAndAdaptorTest.cxx
itkComplexToImaginaryFilterAndAdaptorTest.cxx
itkComplexToModulusFilterAndAdaptorTest.cxx
itkComplexToPhaseFilterAndAdaptorTest.cxx
itkContourDirectedMeanDistanceImageFilterTest.cxx
itkContourMeanDistanceImageFilterTest.cxx
itkConstrainedValueAdditionImageFilterTest.cxx
itkConstrainedValueDifferenceImageFilterTest.cxx
itkBasicFiltersPrintTest.cxx
itkBasicFiltersPrintTest2.cxx
itkDeformationFieldJacobianDeterminantFilterTest.cxx
itkDisplacementFieldJacobianDeterminantFilterTest.cxx
itkGrayscaleMorphologicalClosingImageFilterTest.cxx
itkGrayscaleMorphologicalOpeningImageFilterTest.cxx
itkJoinSeriesImageFilterTest.cxx
itkJoinSeriesImageFilterStreamingTest.cxx
itkPushPopTileImageFilterTest.cxx
itkSimpleContourExtractorImageFilterTest.cxx
itkShiftScaleImageFilterTest.cxx
itkShiftScaleInPlaceImageFilterTest.cxx
itkShrinkImageTest.cxx
itkShrinkImagePreserveObjectPhysicalLocations.cxx
itkShrinkImageStreamingTest.cxx
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
itkStreamingImageFilterTest3.cxx
itkSubtractImageFilterTest.cxx
itkTanImageFilterAndAdaptorTest.cxx
itkTernaryMagnitudeImageFilterTest.cxx
itkTernaryMagnitudeSquaredImageFilterTest.cxx
itkThresholdImageFilterTest.cxx
itkThresholdLabelerImageFilterTest.cxx
itkTileImageFilterTest.cxx
itkTobogganImageFilterTest.cxx
itkTopHatImageFilterTest.cxx
itkTransformMeshFilterTest.cxx
itkTriangleMeshToSimplexMeshFilterTest.cxx
itkTriangleMeshToSimplexMeshFilter2Test.cxx
itkTwoOutputExampleImageFilterTest.cxx
)

set(BasicFilters3_SRCS
itkAccumulateImageFilterTest.cxx
itkApproximateSignedDistanceMapImageFilterTest.cxx
itkCheckerBoardImageFilterTest.cxx
itkHessianRecursiveGaussianFilterTest.cxx
itkHessianRecursiveGaussianFilterScaleSpaceTest.cxx
itkLabelStatisticsImageFilterTest.cxx
itkPolylineMaskImageFilterTest.cxx
itkPolylineMask2DImageFilterTest.cxx
itkNormalizedCorrelationImageFilterTest.cxx
itkSymmetricEigenAnalysisImageFilterTest.cxx
itkTensorFractionalAnisotropyImageFilterTest.cxx
itkTensorRelativeAnisotropyImageFilterTest.cxx
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
itkTriangleMeshToBinaryImageFilterTest.cxx
itkTriangleMeshToBinaryImageFilterTest2.cxx
)

set(BasicFilters4_SRCS
itkClosingByReconstructionImageFilterTest
itkDivideImageFilterTest2
itkGetAverageSliceImageFilterTest
itkGrayscaleGeodesicErodeDilateImageFilterTest
itkHessian3DToVesselnessMeasureImageFilterTest
itkInvertIntensityImageFilterTest
itkMaskNeighborhoodOperatorImageFilterTest
itkMatrixIndexSelectionImageFilterTest
itkModulusImageFilterTest
itkMorphologicalGradientImageFilterTest
itkNotImageFilterTest
itkOpeningByReconstructionImageFilterTest
itkParametricSpaceToImageSpaceMeshFilterTest
itkSignedMaurerDistanceMapImageFilterTest
itkVectorIndexSelectionCastImageFilterTest
itkVectorConnectedComponentImageFilterTest
)

set(BasicFilters5_SRCS
  itkSumProjectionImageFilterTest.cxx
  itkMaximumProjectionImageFilterTest.cxx
  itkMaximumProjectionImageFilterTest2.cxx
  itkMaximumProjectionImageFilterTest3.cxx
  itkMeanProjectionImageFilterTest.cxx
  itkMedianProjectionImageFilterTest.cxx
  itkMinimumProjectionImageFilterTest.cxx
  itkStandardDeviationProjectionImageFilterTest.cxx
  itkBinaryProjectionImageFilterTest.cxx
  itkBinaryThresholdProjectionImageFilterTest.cxx
  itkProjectionImageFilterTest.cxx
  itkImageToVectorImageFilterTest.cxx
  itkSimplexMeshWithFloatCoordRepTest.cxx
  itkReleaseDataFilterTest.cxx
  itkWarpImageFilterTest2.cxx
  itkBSplineDownsampleImageFilterTest.cxx
  itkBSplineUpsampleImageFilterTest.cxx
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

add_executable(itkBasicFiltersTests itkBasicFiltersTests.cxx ${BasicFilters_SRCS})
target_link_libraries(itkBasicFiltersTests ITKIO ITKNumerics ITKBasicFilters)

add_executable(itkBasicFiltersTests1 itkBasicFiltersTests1.cxx ${BasicFilters1_SRCS})
target_link_libraries(itkBasicFiltersTests1 ITKIO ITKNumerics ITKBasicFilters ITKStatistics)

add_executable(itkBasicFiltersTests2 itkBasicFiltersTests2.cxx ${BasicFilters2_SRCS})
target_link_libraries(itkBasicFiltersTests2 ITKIO ITKNumerics ITKBasicFilters)

add_executable(itkBasicFiltersTests3 itkBasicFiltersTests3.cxx ${BasicFilters3_SRCS})
target_link_libraries(itkBasicFiltersTests3 ITKStatistics ITKIO ITKNumerics ITKBasicFilters)

add_executable(itkBasicFiltersTests4 itkBasicFiltersTests4.cxx ${BasicFilters4_SRCS})
target_link_libraries(itkBasicFiltersTests4 ITKStatistics ITKIO ITKNumerics ITKBasicFilters)

add_executable(itkBasicFiltersTests5 itkBasicFiltersTests5.cxx ${BasicFilters5_SRCS})
target_link_libraries(itkBasicFiltersTests5 ITKStatistics ITKIO ITKNumerics ITKBasicFilters)
endif( NOT ITK_DISABLE_CXX_TESTING )


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

