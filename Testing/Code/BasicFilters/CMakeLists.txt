SET(KIT BasicFilters)
CREATE_TEST_SOURCELIST(Tests itk${KIT}Tests.cxx 
  itkChangeInformationImageFilterTest.cxx
  itkCannyEdgeDetectionImageFilterTest.cxx
  itkZeroCrossingBasedEdgeDetectionImageFilterTest.cxx
  itkSobelEdgeDetectionImageFilterTest.cxx
  itkDivideImageFilterTest.cxx
  itkLaplacianImageFilterTest.cxx
  itkZeroCrossingImageFilterTest.cxx
  itkAsinImageFilterAndAdaptorTest.cxx
  itkAcosImageFilterAndAdaptorTest.cxx
  itkAtanImageFilterAndAdaptorTest.cxx
  itkAdaptImageFilterTest.cxx
  itkAdaptImageFilterTest2.cxx
  itkBasicArchitectureTest.cxx
  itkBilateralImageFilterTest.cxx
  itkBilateralImageFilterTest2.cxx
  itkBilateralImageFilterTest3.cxx
  itkBinaryDilateImageFilterTest.cxx
  itkBinaryMagnitudeImageFilterTest.cxx
  itkBinaryMedianImageFilterTest.cxx
  itkBinaryThresholdImageFilterTest.cxx
  itkBSplineDecompositionImageFilterTest.cxx
  itkBSplineInterpolateImageFunctionTest.cxx
  itkBSplineResampleImageFilterTest.cxx
  itkBSplineResampleImageFunctionTest.cxx
  itkBloxBoundaryPointImageTest.cxx
  itkBloxCoreAtomTest.cxx
  itkConfidenceConnectedImageFilterTest.cxx
  itkConnectedThresholdImageFilterTest.cxx
  itkCosImageFilterAndAdaptorTest.cxx
  itkCurvatureAnisotropicDiffusionImageFilterTest.cxx
  itkCyclicReferences.cxx
  itkDanielssonDistanceMapImageFilterTest.cxx
  itkDifferenceOfGaussiansGradientTest.cxx
  itkEdgePotentialImageFilterTest.cxx
  itkEigenAnalysis2DImageFilterTest.cxx
  itkIsolatedConnectedImageFilterTest.cxx
  itkExpImageFilterAndAdaptorTest.cxx
  itkExpNegativeImageFilterAndAdaptorTest.cxx
  itkExpandImageFilterTest.cxx
  itkVectorExpandImageFilterTest.cxx
  itkFilterDispatchTest.cxx
  itkAddImageFilterTest.cxx
  itkFlipImageFilterTest.cxx
  itkFloodFillIteratorTest.cxx
  itkGaussianImageSourceTest.cxx
  itkHausdorffDistanceImageFilterTest.cxx
  itkImageAdaptorNthElementTest.cxx
  itkImageAdaptorPipeLineTest.cxx
  itkImageToParametricSpaceFilterTest.cxx
  itkImportImageTest.cxx
  itkInteriorExteriorMeshFilterTest.cxx
  itkInterpolateImageFilterTest.cxx
  itkInterpolateImagePointsFilterTest.cxx
  itkJoinImageFilterTest.cxx
  itkLog10ImageFilterAndAdaptorTest.cxx
  itkLogImageFilterAndAdaptorTest.cxx
  itkMaskImageFilterTest.cxx
  itkMathematicalMorphologyImageFilterTest.cxx
  itkMaximumImageFilterTest.cxx
  itkMinimumImageFilterTest.cxx
  itkMeanImageFilterTest.cxx
  itkMedianImageFilterTest.cxx
  itkMultiplyImageFilterTest.cxx
  itkMinimumMaximumImageFilterTest.cxx
  itkMinimumMaximumImageCalculatorTest.cxx
  itkNeighborhoodConnectedImageFilterTest.cxx
  itkNormalizeImageFilterTest.cxx
  itkPermuteAxesImageFilterTest.cxx
  itkPlaheImageFilterTest.cxx
  itkRecursiveGaussianImageFiltersTest.cxx
  itkReflectImageFilterTest.cxx
  itkReflectiveImageRegionIteratorTest.cxx
  itkResampleImageTest.cxx
  itkRescaleIntensityImageFilterTest.cxx
  itkRGBToVectorAdaptImageFilterTest.cxx
  itkSimilarityIndexImageFilterTest.cxx
  itkSigmoidImageFilterTest.cxx
  itkShiftScaleImageFilterTest.cxx
  itkShrinkImageTest.cxx
  itkSparseFieldLayerTest.cxx
  itkStatisticsImageFilterTest.cxx
  itkConstantPadImageTest.cxx
  itkWrapPadImageTest.cxx
  itkMirrorPadImageTest.cxx
  itkExtractImageTest.cxx
  itkCropImageFilterTest.cxx
  itkSpatialFunctionImageEvaluatorFilterTest.cxx
  itkStreamingImageFilterTest.cxx
  itkStreamingImageFilterTest2.cxx
  itkSinImageFilterAndAdaptorTest.cxx
  itkSqrtImageFilterAndAdaptorTest.cxx
  itkSquaredDifferenceImageFilterTest.cxx
  itkSubtractImageFilterTest.cxx
  itkTanImageFilterAndAdaptorTest.cxx
  itkTernaryMagnitudeImageFilterTest.cxx
  itkThresholdImageFilterTest.cxx
  itkTransformMeshFilterTest.cxx
  itkTwoOutputExampleImageFilterTest.cxx
  itkVectorAnisotropicDiffusionImageFilterTest.cxx
  itkGradientAnisotropicDiffusionImageFilterTest.cxx
  itkGradientAnisotropicDiffusionImageFilterTest2.cxx
  itkGradientMagnitudeImageFilterTest.cxx
  itkGradientToMagnitudeImageFilterTest.cxx
  itkGradientImageFilterTest.cxx
  itkGradientRecursiveGaussianFilterTest.cxx
  itkGradientMagnitudeRecursiveGaussianFilterTest.cxx
  itkNeighborhoodOperatorImageFilterTest.cxx
  itkVectorNeighborhoodOperatorImageFilterTest.cxx
  itkNaryAddImageFilterTest.cxx
  itkDerivativeImageFilterTest.cxx
  itkDiscreteGaussianImageFilterTest.cxx
  itkWarpImageFilterTest.cxx
  itkHardConnectedComponentImageFilterTest.cxx
  itkBloxBoundaryPointImageToBloxBoundaryProfileImageFilterTest.cxx
  itkTobogganImageFilterTest.cxx
  itkObjectMorphologyImageFilterTest.cxx
  itkVectorGradientMagnitudeImageFilterTest1.cxx
  itkVectorGradientMagnitudeImageFilterTest2.cxx
)

ADD_EXECUTABLE(itk${KIT}Tests ${Tests})
TARGET_LINK_LIBRARIES(itk${KIT}Tests ITKIO ITKNumerics ITKBasicFilters)

SET (TestsToRun ${Tests})
REMOVE (TestsToRun itk${KIT}Tests.cxx)

FOREACH (test ${TestsToRun})
  GET_FILENAME_COMPONENT(TName ${test} NAME_WE)
  ADD_TEST(${TName} ${CXX_TEST_PATH}/itk${KIT}Tests ${TName}) 
ENDFOREACH (test)

# Add PrintSelf test.
IF (TCL_TCLSH)
  ADD_TEST(PrintSelf-${KIT} ${TCL_TCLSH}
           ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
           ${ITK_SOURCE_DIR}/Code/${KIT})
ENDIF (TCL_TCLSH)

# Add header test.
ADD_EXECUTABLE(itk${KIT}HeaderTest itk${KIT}HeaderTest.cxx)
TARGET_LINK_LIBRARIES(itk${KIT}HeaderTest ITKIO ITKNumerics ITKBasicFilters)
ADD_TEST(itk${KIT}HeaderTest ${CXX_TEST_PATH}/itk${KIT}HeaderTest)
