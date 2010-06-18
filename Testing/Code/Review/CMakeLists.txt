IF( NOT ITK_DISABLE_CXX_TESTING )
IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-Review ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/Review)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)


SET(REVIEW_TESTS ${CXX_TEST_PATH}/itkReviewTests)
SET(REVIEW_TESTS2 ${CXX_TEST_PATH}/itkReviewTests2)
SET(REVIEW_TESTS3 ${CXX_TEST_PATH}/itkReviewTests3)
SET(REVIEW_TESTS4 ${CXX_TEST_PATH}/itkReviewTests4)
SET(REVIEW_TESTS5 ${CXX_TEST_PATH}/itkReviewTests5)
SET(REVIEW_TESTS6 ${CXX_TEST_PATH}/itkReviewTests6)
SET(REVIEW_PRINT_TEST ${CXX_TEST_PATH}/itkReviewPrintTest)
SET(REVIEW_HEADER_TESTS ${CXX_TEST_PATH}/itkReviewHeaderTest)

ADD_TEST(itkReviewPrintTest ${REVIEW_PRINT_TEST})

SET(BASELINE ${ITK_DATA_ROOT}/Baseline/Review)
SET(INPUTDATA ${ITK_DATA_ROOT}/Input)
SET(BASELINE_ROOT ${ITK_DATA_ROOT}/Baseline)
SET(TEMP ${ITKTesting_BINARY_DIR}/Temporary)

INCLUDE_DIRECTORIES(
  # This directory is needed to find the itkFilterWatcher.h
  ${ITK_SOURCE_DIR}/Testing/Code/Algorithms
  # This directory is needed to find the itkTestingMacros.h
  ${ITK_SOURCE_DIR}/Testing/Code/Review
  )

#
#  List of Tests in this directory
#

SET(ReviewTest_SRCS
  itkFlatStructuringElementTest.cxx

  itkLabelToRGBImageFilterTest.cxx
  itkLabelOverlayImageFilterTest.cxx

  itkValuedRegionalMinimaImageFilterTest.cxx
  itkValuedRegionalMaximaImageFilterTest.cxx
  itkRegionalMaximaImageFilterTest.cxx
  itkRegionalMaximaImageFilterTest2.cxx
  itkRegionalMinimaImageFilterTest.cxx
  itkRegionalMinimaImageFilterTest2.cxx

  itkNeuralNetworkIOTest.cxx

  itkConformalFlatteningMeshFilterTest.cxx
  itkConformalFlatteningQuadEdgeMeshFilterTest.cxx

  itkVTKPolyDataIOQuadEdgeMeshTest.cxx
  itkVTKPolyDataReaderQuadEdgeMeshTest.cxx
  itkVTKPolyDataReaderTest.cxx
  itkVTKPolyDataWriterTest.cxx
  itkVTKPolyDataWriterTest2.cxx

  itkMorphologicalWatershedImageFilterTest.cxx
  itkMorphologicalWatershedFromMarkersImageFilterTest.cxx

  itkBinaryMorphologicalClosingImageFilterTest.cxx
  itkBinaryMorphologicalOpeningImageFilterTest.cxx

  itkOptImageToImageMetricsTest.cxx
  itkOptMattesMutualInformationImageToImageMetricThreadsTest1.cxx

  itkTimeAndMemoryProbeTest.cxx

  itkBruker2DSEQImageIOTest.cxx
  itkPhilipsRECImageIOTest.cxx

  itkVoxBoCUBImageIOTest.cxx

  itkSliceBySliceImageFilterTest.cxx

  itkDiffeomorphicDemonsRegistrationFilterTest.cxx
  itkDiffeomorphicDemonsRegistrationFilterTest2.cxx

  itkMultiplyByConstantImageFilterTest.cxx
  itkDivideByConstantImageFilterTest.cxx
  itkAddConstantToImageFilterTest.cxx
  itkSubtractConstantFromImageFilterTest.cxx

  itkImageReadComplexWriteMagnitudeAndPhaseTest.cxx
  itkImageReadMagnitudeAndPhaseWriteComplexTest.cxx
  itkImageReadRealAndImaginaryWriteComplexTest.cxx

  itkFFTComplexToComplexImageFilterTest01.cxx
  itkFFTComplexToComplexImageFilterTest02.cxx

  itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunctionTest.cxx
  itkVectorCentralDifferenceImageFunctionTest.cxx
  itkExponentialDeformationFieldImageFilterTest.cxx
  itkFastSymmetricForcesDemonsRegistrationFilterTest.cxx
  itkGridForwardWarpImageFilterTest.cxx
  itkWarpHarmonicEnergyCalculatorTest.cxx

  itkBSplineScatteredDataPointSetToImageFilterTest.cxx
  itkBSplineScatteredDataPointSetToImageFilterTest2.cxx
  itkBSplineScatteredDataPointSetToImageFilterTest3.cxx

  itkCoxDeBoorBSplineKernelFunctionTest.cxx
  itkCoxDeBoorBSplineKernelFunctionTest2.cxx

  itkKappaSigmaThresholdImageCalculatorTest.cxx
  itkKappaSigmaThresholdImageFilterTest.cxx

  itkComposeRGBAImageFilterTest.cxx

  itkTransformToDeformationFieldSourceTest.cxx
  itkTransformToDeformationFieldSourceTest1.cxx
  
  itkBoxMeanImageFilterTest.cxx
  itkBoxSigmaImageFilterTest.cxx
  itkRankImageFilterTest.cxx
  itkMapRankImageFilterTest.cxx
  itkMaskedRankImageFilterTest.cxx
  itkMapMaskedRankImageFilterTest.cxx
  itkFastApproximateRankImageFilterTest.cxx
  
  itkDiscreteGaussianDerivativeImageFunctionTest.cxx
  itkDiscreteGradientMagnitudeGaussianImageFunctionTest.cxx
  itkDiscreteHessianGaussianImageFunctionTest.cxx
  
  itkBinaryContourImageFilterTest.cxx
  itkLabelContourImageFilterTest.cxx
  
  itkFFTShiftImageFilterTest.cxx

  itkConvolutionImageFilterTest.cxx
  itkConvolutionImageFilterTestInt.cxx

  itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1.cxx
  itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2.cxx
  itkShapedFloodFilledImageFunctionConditionalConstIteratorTest3.cxx

  itkRobustAutomaticThresholdImageFilterTest.cxx

  itkAreaOpeningImageFilterTest.cxx
  itkAreaClosingImageFilterTest.cxx

  itkHessianToObjectnessMeasureImageFilterTest.cxx
  itkMultiScaleHessianBasedMeasureImageFilterTest.cxx
)

SET(ReviewTest2_SRCS
  itkDirectFourierReconstructionImageToImageFilterTest.cxx
  itkBSplineDeformableTransformInitializerTest1.cxx
  itkBSplineDeformableTransformInitializerTest2.cxx
  itkHeavisideStepFunctionTest1.cxx
  itkLabelImageToLabelMapFilterTest.cxx 
  itkLabelMapFilterTest.cxx 
  itkLabelMapTest.cxx 
  itkLabelMapToLabelImageFilterTest.cxx 
  itkLabelObjectLineComparatorTest.cxx 
  itkLabelObjectLineTest.cxx 
  itkLabelObjectTest.cxx 
  itkMultiphaseFiniteDifferenceImageFilterTest.cxx 
  itkMultiphaseDenseFiniteDifferenceImageFilterTest.cxx 
  itkMultiphaseSparseFiniteDifferenceImageFilterTest.cxx 
  itkMultiScaleHessianBasedMeasureImageFilterTest.cxx
  itkQuadEdgem_NoPointTest.cxx
  itkQuadEdgeTest1.cxx
  itkGeometricalQuadEdgeTest1.cxx
  itkQuadEdgeMeshAddFaceTest1.cxx
  itkQuadEdgeMeshAddFaceTest2.cxx
  itkQuadEdgeMeshBasicLayerTest.cxx
  itkQuadEdgeMeshDeleteEdgeTest.cxx
  itkQuadEdgeMeshFrontIteratorTest.cxx
  itkQuadEdgeMeshIteratorTest.cxx
  itkQuadEdgeMeshPointTest1.cxx
  itkQuadEdgeMeshTest1.cxx
  itkQuadEdgeMeshTest2.cxx
  itkQuadEdgeMeshTest3.cxx
  itkQuadEdgeMeshEulerOperatorFlipTest.cxx
  itkQuadEdgeMeshEulerOperatorSplitVertexTest.cxx
  itkQuadEdgeMeshEulerOperatorJoinVertexTest.cxx
  itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest.cxx
  itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest.cxx
  itkQuadEdgeMeshEulerOperatorJoinFacetTest.cxx
  itkQuadEdgeMeshEulerOperatorSplitEdgeTest.cxx
  itkQuadEdgeMeshEulerOperatorSplitFaceTest.cxx
  itkQuadEdgeMeshCellInterfaceTest.cxx
  itkQuadEdgeMeshPolygonCellTest.cxx
  itkDynamicQuadEdgeMeshTest.cxx
  itkAutomaticTopologyQuadEdgeMeshSourceTest.cxx
  itkRegularSphereQuadEdgeMeshSourceTest.cxx
  itkQuadEdgeMeshCountingCellsTest.cxx
  itkQuadEdgeMeshDeletePointAndReorderIDsTest.cxx
  itkBinaryMask3DQuadEdgeMeshSourceTest.cxx
  itkContourExtractor2DImageFilterTest.cxx
  itkTriangleHelperTest.cxx
  itkCrossHelperTest.cxx
  itkQuadEdgeMeshLinearParameterizationTest.cxx
  itkPriorityQueueTest.cxx
  itkQuadEdgeMeshSquaredEdgeLengthDecimationTest.cxx
  itkQuadEdgeMeshQuadricDecimationTest.cxx
  itkQuadEdgeMeshCleanFilterTest.cxx
  itkQuadEdgeMeshGaussianCurvatureTest.cxx
  itkQuadEdgeMeshMaxCurvatureTest.cxx
  itkQuadEdgeMeshMeanCurvatureTest.cxx
  itkQuadEdgeMeshMinCurvatureTest.cxx
  itkQuadEdgeMeshDelaunayConformingFilterTest.cxx
  itkQuadEdgeMeshNormalFilterTest.cxx
  itkQuadEdgeMeshSmoothingTest.cxx
  itkQuadEdgeMeshScalarDataVTKPolyDataWriterTest1.cxx
  itkAtanRegularizedHeavisideStepFunctionTest1.cxx
  itkRegionBasedLevelSetFunctionTest.cxx
  itkScalarRegionBasedLevelSetFunctionTest.cxx
  itkScalarToRGBColormapImageFilterTest.cxx
  itkStochasticFractalDimensionImageFilterTest.cxx
  itkSinRegularizedHeavisideStepFunctionTest1.cxx
)

SET(ReviewTest3_SRCS
  itkAggregateLabelMapFilterTest.cxx
  itkGridImageSourceTest.cxx
  itkTriangleMeshToBinaryImageFilterTest3.cxx

  itkBinaryImageToLabelMapFilterTest.cxx
  itkLabelMapToBinaryImageFilterTest.cxx
  itkChangeLabelLabelMapFilterTest.cxx
  itkRelabelLabelMapFilterTest1.cxx
  itkShiftScaleLabelMapFilterTest1.cxx
  itkAutoCropLabelMapFilterTest1.cxx
  itkChangeRegionLabelMapFilterTest1.cxx

  itkMergeLabelMapFilterTest1.cxx
  itkCropLabelMapFilterTest1.cxx
  itkPadLabelMapFilterTest1.cxx
  itkRegionFromReferenceLabelMapFilterTest1.cxx

  itkBinaryImageToShapeLabelMapFilterTest1.cxx
  itkLabelImageToShapeLabelMapFilterTest1.cxx

  itkShapeKeepNObjectsLabelMapFilterTest1.cxx
  itkShapeOpeningLabelMapFilterTest1.cxx
  itkShapeRelabelLabelMapFilterTest1.cxx
  itkBinaryShapeOpeningImageFilterTest1.cxx
  itkShapeLabelObjectAccessorsTest1.cxx
  itkBinaryShapeKeepNObjectsImageFilterTest1.cxx
  itkShapeRelabelImageFilterTest1.cxx
  itkLabelShapeKeepNObjectsImageFilterTest1.cxx
  itkLabelShapeOpeningImageFilterTest1.cxx
  itkShapeUniqueLabelMapFilterTest1.cxx

  itkGaborImageSourceTest.cxx

  itkDiscreteGaussianDerivativeImageFilterTest
)

SET(ReviewTest4_SRCS
  itkBinaryImageToStatisticsLabelMapFilterTest1.cxx
  itkLabelImageToStatisticsLabelMapFilterTest1.cxx
  itkStatisticsOpeningLabelMapFilterTest1.cxx
  itkStatisticsKeepNObjectsLabelMapFilterTest1.cxx
  itkStatisticsRelabelLabelMapFilterTest1.cxx
  itkBinaryStatisticsOpeningImageFilterTest1.cxx
  itkBinaryStatisticsKeepNObjectsImageFilterTest1.cxx
  itkLabelStatisticsKeepNObjectsImageFilterTest1.cxx
  itkLabelStatisticsOpeningImageFilterTest1.cxx
  itkStatisticsRelabelImageFilterTest1.cxx
  itkStatisticsUniqueLabelMapFilterTest1.cxx
)

SET(ReviewTest5_SRCS
  itkLabelGeometryImageFilterTest.cxx
  itkMRCImageIOTest.cxx
  itkVTKImageIO2Test.cxx
)

SET(ReviewTest6_SRCS
  itkScalarChanAndVeseLevelSetFunctionTest1.cxx
  itkScalarChanAndVeseLevelSetFunctionTest2.cxx
  itkScalarChanAndVeseDenseLevelSetImageFilterTest1.cxx
  itkScalarChanAndVeseDenseLevelSetImageFilterTest2.cxx
  itkScalarChanAndVeseDenseLevelSetImageFilterTest3.cxx
  itkScalarChanAndVeseDenseLevelSetImageFilterTest4.cxx
  itkScalarChanAndVeseSparseLevelSetImageFilterTest1.cxx
  itkScalarChanAndVeseSparseLevelSetImageFilterTest2.cxx
)

IF(ITK_USE_TRANSFORM_IO_FACTORIES)
  SET(ReviewTest_SRCS ${ReviewTest_SRCS} itkTransformFileReaderWriterTest.cxx)

  ADD_TEST(itkTransformFileReaderWriterTest0
    ${REVIEW_TESTS} itkTransformFileReaderWriterTest
    ${INPUTDATA}/itkAffineTransform00.txt
    ${TEMP}/itkTransformFileReaderWriterTest0.txt
    )

  ADD_TEST(itkTransformFileReaderWriterTest1
    ${REVIEW_TESTS} itkTransformFileReaderWriterTest
    ${INPUTDATA}/itkAffineTransform01.txt
    ${TEMP}/itkTransformFileReaderWriterTest1.txt
    )
  ADD_TEST(itkTransformFileReaderWriterTest2
    ${REVIEW_TESTS} itkTransformFileReaderWriterTest
    ${INPUTDATA}/itkAffineTransform01.txt
    ${TEMP}/itkTransformFileReaderWriterTest1.mat
    )
  ADD_TEST(itkTransformFileReaderWriterTest3
    ${REVIEW_TESTS} itkTransformFileReaderWriterTest
    ${INPUTDATA}/itkBSplineTransform00.txt
    ${TEMP}/itkBSplineTransform00.mat
    )
ENDIF(ITK_USE_TRANSFORM_IO_FACTORIES)

ADD_TEST(itkOptImageToImageMetricsTest01
  ${REVIEW_TESTS} itkOptImageToImageMetricsTest
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  )

ADD_TEST(itkOptImageToImageMetricsTest02
  ${REVIEW_TESTS}
  --with-threads 2
  itkOptImageToImageMetricsTest
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  )

ADD_TEST(itkOptImageToImageMetricsTest03
  ${REVIEW_TESTS}
  --without-threads
  itkOptImageToImageMetricsTest
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  )

IF(ITK_USE_OPTIMIZED_REGISTRATION_METHODS)
SET(ReviewTest_SRCS ${ReviewTest_SRCS} itkOptImageToImageMetricsTest2.cxx)
ADD_TEST(itkOptImageToImageMetricsTest04
  ${REVIEW_TESTS} itkOptImageToImageMetricsTest2
  ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
  ${ITK_SOURCE_DIR}/Testing/Data/Baseline/Common/itkBSplineDeformableTransformTest4.png
  )
ENDIF(ITK_USE_OPTIMIZED_REGISTRATION_METHODS)


ADD_TEST(itkOptMattesImageToImageMetricsTest01
  ${REVIEW_TESTS} itkOptMattesMutualInformationImageToImageMetricThreadsTest1
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  0
  100
  )


IF(ITK_USE_MINC2 AND MINC2_FOUND)
  SET(ReviewTest_SRCS ${ReviewTest_SRCS} itkMINC2ImageIOTest.cxx)
ENDIF(ITK_USE_MINC2 AND MINC2_FOUND)

ADD_TEST(itkFlatStructuringElementTest ${REVIEW_TESTS}
  itkFlatStructuringElementTest
  )

ADD_TEST(itkDirectFourierReconstructionImageToImageFilterTest 
  ${REVIEW_TESTS2} 
  --compare ${BASELINE}/reconstruct.mha
            ${TEMP}/reconstruct.mha
  itkDirectFourierReconstructionImageToImageFilterTest
  ${INPUTDATA}/sinogram-small.mha 
  ${TEMP}/reconstruct.mha 
  1 
  2 
  0 
  2 
  2 
  1
  3 
  180 
  0 
  0 
  0 
  64
  64 
  2 
  0
  )

ADD_TEST(itkBSplineDeformableTransformInitializerTest1 
  ${REVIEW_TESTS2}
  itkBSplineDeformableTransformInitializerTest1 
  ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${TEMP}/itkBSplineDeformableTransformInitializerTest1.png
  ${TEMP}/itkBSplineDeformableTransformInitializerTest1DeformationField.mhd
)

ADD_TEST(itkBSplineDeformableTransformInitializerTest2 
  ${REVIEW_TESTS2}
  itkBSplineDeformableTransformInitializerTest2 
  ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySliceBorder20.png
  ${TEMP}/itkBSplineDeformableTransformInitializerTest2.png
  ${TEMP}/itkBSplineDeformableTransformInitializerTest2DeformationField.mhd
)

ADD_TEST(itkQuadEdgem_NoPointTest ${REVIEW_TESTS2} itkQuadEdgem_NoPointTest)
ADD_TEST(itkQuadEdgeTest1 ${REVIEW_TESTS2} itkQuadEdgeTest1)
ADD_TEST(itkGeometricalQuadEdgeTest1 ${REVIEW_TESTS2}
  itkGeometricalQuadEdgeTest1)
ADD_TEST(itkQuadEdgeMeshPointTest1 ${REVIEW_TESTS2}
  itkQuadEdgeMeshPointTest1)
ADD_TEST(itkPriorityQueueTest ${REVIEW_TESTS2}
  itkPriorityQueueTest)

FOREACH( index 0 1 2 3 4 5 )
  ADD_TEST(itkQuadEdgeMeshAddFaceTest1_${index} ${REVIEW_TESTS2}
    itkQuadEdgeMeshAddFaceTest1 ${index} )
ENDFOREACH( index )

ADD_TEST(itkQuadEdgeMeshAddFaceTest2 ${REVIEW_TESTS2}
  itkQuadEdgeMeshAddFaceTest2)
ADD_TEST(itkQuadEdgeMeshDeleteEdgeTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshDeleteEdgeTest)
ADD_TEST(itkQuadEdgeMeshFrontIteratorTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshFrontIteratorTest)
ADD_TEST(itkQuadEdgeMeshTest1 ${REVIEW_TESTS2} itkQuadEdgeMeshTest1)
ADD_TEST(itkQuadEdgeMeshTest2 ${REVIEW_TESTS2} itkQuadEdgeMeshTest2)
ADD_TEST(itkQuadEdgeMeshTest3 ${REVIEW_TESTS2} itkQuadEdgeMeshTest3)
ADD_TEST(itkQuadEdgeMeshEulerOperatorFlipTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshEulerOperatorFlipTest)
ADD_TEST(itkQuadEdgeMeshEulerOperatorJoinFacetTest ${REVIEW_TESTS2} 
  itkQuadEdgeMeshEulerOperatorJoinFacetTest)
FOREACH( arguments 0 1 2 3 4 )
  ADD_TEST( itkQuadEdgeMeshEulerOperatorJoinVertexTest${arguments}
    ${REVIEW_TESTS2}
    itkQuadEdgeMeshEulerOperatorJoinVertexTest ${arguments} )
ENDFOREACH( arguments )
ADD_TEST(itkQuadEdgeMeshEulerOperatorSplitEdgeTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshEulerOperatorSplitEdgeTest)
ADD_TEST(itkQuadEdgeMeshEulerOperatorSplitVertexTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshEulerOperatorSplitVertexTest)
ADD_TEST( itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest) 
ADD_TEST( itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest)
ADD_TEST(itkQuadEdgeMeshCellInterfaceTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshCellInterfaceTest)
ADD_TEST(itkQuadEdgeMeshIteratorTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshIteratorTest)
ADD_TEST(itkQuadEdgeMeshPolygonCellTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshPolygonCellTest)
ADD_TEST(itkDynamicQuadEdgeMeshTest ${REVIEW_TESTS2}
  itkDynamicQuadEdgeMeshTest)
ADD_TEST(itkAutomaticTopologyQuadEdgeMeshSourceTest ${REVIEW_TESTS2}
  itkAutomaticTopologyQuadEdgeMeshSourceTest)
ADD_TEST(itkRegularSphereQuadEdgeMeshSourceTest ${REVIEW_TESTS2}
  itkRegularSphereQuadEdgeMeshSourceTest
  ${TEMP}/itkRegularSphereQuadEdgeMeshSourceTest.vtk)
ADD_TEST(itkRegionBasedLevelSetFunctionTest ${REVIEW_TESTS2}
  itkRegionBasedLevelSetFunctionTest)
ADD_TEST(itkScalarRegionBasedLevelSetFunctionTest ${REVIEW_TESTS2}
  itkScalarRegionBasedLevelSetFunctionTest)
ADD_TEST(itkQuadEdgeMeshCountingCellsTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshCountingCellsTest)
ADD_TEST(itkBinaryMask3DQuadEdgeMeshSourceTest ${REVIEW_TESTS2}
  itkBinaryMask3DQuadEdgeMeshSourceTest)
ADD_TEST( itkQuadEdgeMeshDeletePointAndReorderIDsTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshDeletePointAndReorderIDsTest)
ADD_TEST( itkTriangleHelperTest ${REVIEW_TESTS2}
  itkTriangleHelperTest)
ADD_TEST( itkTriangleMeshToBinaryImageFilterTest3 ${REVIEW_TESTS3}
  itkTriangleMeshToBinaryImageFilterTest3
  ${INPUTDATA}/genusZeroSurface01.vtk
  ${TEMP}/itkTriangleMeshToBinaryImageFilterTest3.mha
    140   160   180   # image size X Y Z
   -0.7  -0.8  -0.9   # image origin X Y Z
   0.01  0.01  0.01   # image spacing X Y Z
  )
ADD_TEST( itkCrossHelperTest ${REVIEW_TESTS2}
  itkCrossHelperTest)
ADD_TEST( itkQuadEdgeMeshQuadricDecimationTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshQuadricDecimationTest
  ${INPUTDATA}/mushroom.vtk
  100
  ${TEMP}/temp_QuadricDecimationResult1.vtk
  )
ADD_TEST( itkQuadEdgeMeshQuadricDecimationTetrahedronTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshQuadricDecimationTest
  ${INPUTDATA}/tetrahedron.vtk
  2
  ${TEMP}/temp_QuadricDecimationTetrahedron.vtk 
  )
ADD_TEST( itkQuadEdgeMeshSquaredEdgeLengthDecimationTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshSquaredEdgeLengthDecimationTest
  ${INPUTDATA}/mushroom.vtk
  20
  ${TEMP}/temp_SquaredEdgeLengthDecimationResult1.vtk
  )
ADD_TEST( itkQuadEdgeMeshCleanFilterTest
  ${REVIEW_TESTS2}
  itkQuadEdgeMeshCleanFilterTest
  ${INPUTDATA}/mushroom.vtk
  0.1 
  ${TEMP}/temp_CleanResult1.vtk
  )
ADD_TEST( itkQuadEdgeMeshNormalFilterTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshNormalFilterTest
  ${INPUTDATA}/mushroom.vtk
  )

ADD_TEST( itkQuadEdgeMeshSmoothingTest0 ${REVIEW_TESTS2}
  itkQuadEdgeMeshSmoothingTest
  ${INPUTDATA}/genusZeroSurface01.vtk
  10
  0.1
  0
  ${TEMP}/temp_SmoothResult0.vtk
  )

 ADD_TEST( itkQuadEdgeMeshSmoothingTest1 ${REVIEW_TESTS2}
  itkQuadEdgeMeshSmoothingTest
  ${INPUTDATA}/genusZeroSurface01.vtk
  10
  0.1
  1
  ${TEMP}/temp_SmoothResult1.vtk
  )

ADD_TEST( itkQuadEdgeMeshScalarDataVTKPolyDataWriterTest1 ${REVIEW_TESTS2}
  itkQuadEdgeMeshScalarDataVTKPolyDataWriterTest1
  ${TEMP}/sphere.vtk
  )
 
SET( CURV_TESTS Gaussian Max Mean Min )
FOREACH( loop_var ${CURV_TESTS} )
  ADD_TEST( itkQuadEdgeMesh${loop_var}CurvatureTest
    ${REVIEW_TESTS2}
    itkQuadEdgeMesh${loop_var}CurvatureTest
    ${INPUTDATA}/mushroom.vtk
    )
ENDFOREACH( loop_var )

FOREACH( border 0 1 )
  FOREACH( coeff 0 1 2 3 4 )
    ADD_TEST( itkQuadEdgeMeshLinearParameterizationTest${border}_${coeff}
    ${REVIEW_TESTS2}
    itkQuadEdgeMeshLinearParameterizationTest 
    ${INPUTDATA}/mushroom.vtk ${border} ${coeff}
    ${TEMP}/param_border${border}_coeff${coeff}.vtk
    )
  ENDFOREACH( coeff )
ENDFOREACH( border )

ADD_TEST( itkQuadEdgeMeshDelaunayConformingFilterTest ${REVIEW_TESTS2}
  itkQuadEdgeMeshDelaunayConformingFilterTest
  ${INPUTDATA}/mushroom.vtk 
  ${TEMP}/mushrom_delaunay.vtk
  )
 
FOREACH(opacity 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0)
  STRING(REGEX REPLACE "\\." "_" op ${opacity})
  ADD_TEST(itkLabelOverlayImageFilterTest-Opacity-${op} ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1-${op}Overlay.png
                  ${TEMP}/cthead1-${op}Overlay.png
                  itkLabelOverlayImageFilterTest
                  ${INPUTDATA}/cthead1.png
                  ${INPUTDATA}/cthead1Label.png
                  ${opacity}
                  ${TEMP}/cthead1-${op}Overlay.png)
ENDFOREACH(opacity)

ADD_TEST(itkLabelToRGBImageFilterTest ${REVIEW_TESTS}
      --compare ${BASELINE}/cthead1Label-color.png
                ${TEMP}/cthead1Label-color.png
                itkLabelToRGBImageFilterTest
                ${INPUTDATA}/cthead1Label.png
                ${TEMP}/cthead1Label-color.png)

ADD_TEST(itkContourExtractor2DImageFilterTest ${REVIEW_TESTS2}
                                  itkContourExtractor2DImageFilterTest
                                  ${INPUTDATA}/ContourExtractor2DTest.tif  )

ADD_TEST(itkHeavisideStepFunctionTest1 ${REVIEW_TESTS2}
  itkHeavisideStepFunctionTest1
  )

ADD_TEST(itkLabelImageToLabelMapFilterTest ${REVIEW_TESTS2}
  itkLabelImageToLabelMapFilterTest
  )

ADD_TEST(itkLabelMapFilterTest ${REVIEW_TESTS2}
  itkLabelMapFilterTest
  )

ADD_TEST(itkLabelMapTest ${REVIEW_TESTS2}
  itkLabelMapTest
  )

ADD_TEST(itkLabelMapToLabelImageFilterTest ${REVIEW_TESTS2}
  itkLabelMapToLabelImageFilterTest
  )

ADD_TEST(itkLabelObjectLineComparatorTest ${REVIEW_TESTS2}
  itkLabelObjectLineComparatorTest
  )

ADD_TEST(itkLabelObjectLineTest ${REVIEW_TESTS2}
  itkLabelObjectLineTest
  )

ADD_TEST(itkLabelObjectTest ${REVIEW_TESTS2}
  itkLabelObjectTest
  )

ADD_TEST(itkMultiphaseFiniteDifferenceImageFilterTest ${REVIEW_TESTS2}
  itkMultiphaseFiniteDifferenceImageFilterTest
  )

ADD_TEST(itkMultiphaseDenseFiniteDifferenceImageFilterTest ${REVIEW_TESTS2}
  itkMultiphaseDenseFiniteDifferenceImageFilterTest
  )

ADD_TEST(itkMultiphaseSparseFiniteDifferenceImageFilterTest ${REVIEW_TESTS2}
  itkMultiphaseSparseFiniteDifferenceImageFilterTest
  )

ADD_TEST(itkAtanRegularizedHeavisideStepFunctionTest1 ${REVIEW_TESTS2}
  itkAtanRegularizedHeavisideStepFunctionTest1
  )

ADD_TEST(itkSinRegularizedHeavisideStepFunctionTest1 ${REVIEW_TESTS2}
  itkSinRegularizedHeavisideStepFunctionTest1
  )

ADD_TEST(itkStochasticFractalDimensionImageFilterTest1 ${REVIEW_TESTS2}
  --compare ${BASELINE}/itkStochasticFractalDimensionImageFilterTest1.mha
            ${TEMP}/itkStochasticFractalDimensionImageFilterTest1.mha
  itkStochasticFractalDimensionImageFilterTest
  2                                                         # image dimension
  ${INPUTDATA}/TreeBarkTexture.png                          # input image
  ${TEMP}/itkStochasticFractalDimensionImageFilterTest1.mha # output image
  2                                                         # radius
  )

ADD_TEST(itkStochasticFractalDimensionImageFilterTest2 ${REVIEW_TESTS2}
  --compare ${BASELINE}/itkStochasticFractalDimensionImageFilterTest2.mha
            ${TEMP}/itkStochasticFractalDimensionImageFilterTest2.mha
  itkStochasticFractalDimensionImageFilterTest
  2                                                         # image dimension
  ${INPUTDATA}/TreeBarkTexture.png                          # input image
  ${TEMP}/itkStochasticFractalDimensionImageFilterTest2.mha # output image
  2                                                         # radius
  ${INPUTDATA}/circle100.png                                # mask image
  255                                                       # mask label value
  )


ADD_TEST(itkNeuralNetworkIOTest ${REVIEW_TESTS}
                  itkNeuralNetworkIOTest
                  ${INPUTDATA}/xornet.txt
                  ${INPUTDATA}/xortest.txt
                  ${TEMP}
                  )

ADD_TEST(itkConformalFlatteningMeshFilterTest1 ${REVIEW_TESTS}
                 itkConformalFlatteningMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroMesh01ConformalFlattenedToSphere.vtk
                 1
                 )

ADD_TEST(itkConformalFlatteningQuadEdgeMeshFilterTest1 ${REVIEW_TESTS}
                 itkConformalFlatteningQuadEdgeMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroQuadEdgeMesh01ConformalFlattenedToSphere.vtk
                 3
                 100.0
                 1
                 )

ADD_TEST(itkConformalFlatteningMeshFilterTest2 ${REVIEW_TESTS}
                 itkConformalFlatteningMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroMesh01ConformalFlattenedToPlane.vtk
                 0
                 )

ADD_TEST(itkConformalFlatteningQuadEdgeMeshFilterTest2 ${REVIEW_TESTS}
                 itkConformalFlatteningQuadEdgeMeshFilterTest
                 ${INPUTDATA}/genusZeroSurface01.vtk
                 ${TEMP}/genusZeroQuadEdgeMesh01ConformalFlattenedToPlane.vtk
                 3
                 100.0
                 0
                 )

ADD_TEST(itkValuedRegionalMinimaImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1ValuedRegionalMinimal-ref.png
                  ${TEMP}/cthead1ValuedRegionalMinimal.png
                  itkValuedRegionalMinimaImageFilterTest
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMinimal.png
                  ${TEMP}/cthead1ValuedRegionalMinimal-ref.png
                  )

ADD_TEST(itkValuedRegionalMinimaImageFilterTest2 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1ValuedRegionalMinimal-ref2.png
                  ${TEMP}/cthead1ValuedRegionalMinimal2.png
                  itkValuedRegionalMinimaImageFilterTest
                  0
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMinimal2.png
                  ${TEMP}/cthead1ValuedRegionalMinimal-ref2.png
                  )

ADD_TEST(itkValuedRegionalMaximaImageFilterTest ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1ValuedRegionalMaximal-ref.png
                  ${TEMP}/cthead1ValuedRegionalMaximal.png
                  itkValuedRegionalMaximaImageFilterTest
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMaximal.png
                  ${TEMP}/cthead1ValuedRegionalMaximal-ref.png
                  )

ADD_TEST(itkValuedRegionalMaximaImageFilterTest2 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1ValuedRegionalMaximal-ref2.png
                  ${TEMP}/cthead1ValuedRegionalMaximal2.png
                  itkValuedRegionalMaximaImageFilterTest
                  0
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1ValuedRegionalMaximal2.png
                  ${TEMP}/cthead1ValuedRegionalMaximal-ref2.png
                  )

ADD_TEST(itkRegionalMinimaImageFilterTest2_1 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1RegionalMinimal-ref2_1.png
                  ${TEMP}/cthead1RegionalMinimal2_1.png
                  itkRegionalMaximaImageFilterTest2
                  1
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMinimal2_1.png
                  ${TEMP}/cthead1RegionalMinimal-ref2_1.png
                  )
ADD_TEST(itkRegionalMinimaImageFilterTest2_2 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1RegionalMinimal-ref2_2.png
                  ${TEMP}/cthead1RegionalMinimal2_2.png
                  itkRegionalMaximaImageFilterTest2
                  0
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMinimal2_2.png
                  ${TEMP}/cthead1RegionalMinimal-ref2_2.png
                  )

ADD_TEST(itkRegionalMaximaImageFilterTest2_1 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1RegionalMaximal-ref2_1.png
                  ${TEMP}/cthead1RegionalMaximal2_1.png
                  itkRegionalMaximaImageFilterTest2
                  1
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMaximal2_1.png
                  ${TEMP}/cthead1RegionalMaximal-ref2_1.png
                  )
ADD_TEST(itkRegionalMaximaImageFilterTest2_2 ${REVIEW_TESTS}
        --compare ${BASELINE}/cthead1RegionalMaximal-ref2_2.png
                  ${TEMP}/cthead1RegionalMaximal2_2.png
                  itkRegionalMaximaImageFilterTest2
                  0
                  1
                  ${INPUTDATA}/cthead1.png
                  ${TEMP}/cthead1RegionalMaximal2_2.png
                  ${TEMP}/cthead1RegionalMaximal-ref2_2.png
                  )

ADD_TEST(itkVTKPolyDataIOQuadEdgeMeshTest ${REVIEW_TESTS}
  itkVTKPolyDataIOQuadEdgeMeshTest
  ${INPUTDATA}/genusZeroSurface01.vtk
  ${TEMP}/VTKPolyDataIOTest.vtk)

ADD_TEST(itkVTKPolyDataReaderQuadEdgeMeshTest ${REVIEW_TESTS}
  itkVTKPolyDataReaderQuadEdgeMeshTest
  ${INPUTDATA}/genusZeroSurface01.vtk
  )

ADD_TEST(itkVTKPolyDataReaderTest ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/genusZeroSurface01.vtk
  )

ADD_TEST(itkVTKPolyDataReaderTest2 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/sphereBinary.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest0 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad0.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest1 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad1.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest2 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad2.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest3 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad3.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest4 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad4.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest5 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad5.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest6 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad6.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest7 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad7.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest8 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad8.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest9 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad9.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest10 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad10.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest11 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad11.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest12 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad12.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest13 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad13.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest14 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad14.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest15 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad15.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest16 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad16.vtk
  )

ADD_TEST(itkVTKPolyDataReaderBadTest17 ${REVIEW_TESTS}
  itkVTKPolyDataReaderTest
  ${INPUTDATA}/vtkPolyDataBad17.vtk
  )

SET_TESTS_PROPERTIES(itkVTKPolyDataReaderTest2
  itkVTKPolyDataReaderBadTest0
  itkVTKPolyDataReaderBadTest1
  itkVTKPolyDataReaderBadTest2
  itkVTKPolyDataReaderBadTest3
  itkVTKPolyDataReaderBadTest4
  itkVTKPolyDataReaderBadTest5
  itkVTKPolyDataReaderBadTest6
  itkVTKPolyDataReaderBadTest7
  itkVTKPolyDataReaderBadTest8
  itkVTKPolyDataReaderBadTest9
  itkVTKPolyDataReaderBadTest10
  itkVTKPolyDataReaderBadTest11
  itkVTKPolyDataReaderBadTest12
  itkVTKPolyDataReaderBadTest13
  itkVTKPolyDataReaderBadTest14
  itkVTKPolyDataReaderBadTest15
  itkVTKPolyDataReaderBadTest16
  itkVTKPolyDataReaderBadTest17
  PROPERTIES WILL_FAIL true)

ADD_TEST(itkVTKPolyDataWriterTest01 ${REVIEW_TESTS}
  itkVTKPolyDataWriterTest
  ${TEMP}/VTKPolyDataWriterTest01.vtk
  )

ADD_TEST(itkVTKPolyDataWriterTest02 ${REVIEW_TESTS}
  itkVTKPolyDataWriterTest2
  ${TEMP}/VTKPolyDataWriterTest02.vtk
  )


FOREACH(M 0 1)
  FOREACH(F 0 1)
    ADD_TEST(itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F} ${REVIEW_TESTS}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/button-hole.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestButtonHoleM${M}F${F}.png
                      ${M} ${F} 0
                      )

    ADD_TEST(itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F} ${REVIEW_TESTS}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/pass-values.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPassValuesM${M}F${F}.png
                      ${M} ${F} 0
                      )

    ADD_TEST(itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F} ${REVIEW_TESTS}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/plateau.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestPlateauM${M}F${F}.png
                      ${M} ${F} 0
                      )

    ADD_TEST(itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F} ${REVIEW_TESTS}
            --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F}.png
                      itkMorphologicalWatershedImageFilterTest
                      ${INPUTDATA}/thick-lines.png
                      ${TEMP}/itkMorphologicalWatershedImageFilterTestThickLinesM${M}F${F}.png
                      ${M} ${F} 0
                      )

    ADD_TEST(itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F} ${REVIEW_TESTS}
            --compare ${BASELINE}/itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F}.png
                      ${TEMP}/itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F}.png
                      itkMorphologicalWatershedFromMarkersImageFilterTest
                      ${INPUTDATA}/cthead1.png
                      ${INPUTDATA}/cthead1-markers.png
                      ${TEMP}/itkMorphologicalWatershedFromMarkersImageFilterTestM${M}F${F}.png
                      ${M} ${F}
                      )
  ENDFOREACH(F)
ENDFOREACH(M)

FOREACH(level 00 10 20 30 40 50)
  ADD_TEST(itkMorphologicalWatershedImageFilterTestLevel${level} ${REVIEW_TESTS}
          --compare ${BASELINE}/itkMorphologicalWatershedImageFilterTestLevel${level}.png
                    ${TEMP}/itkMorphologicalWatershedImageFilterTestLevel${level}.png
                    itkMorphologicalWatershedImageFilterTest
                    ${INPUTDATA}/level.png
                    ${TEMP}/itkMorphologicalWatershedImageFilterTestLevel${level}.png
                    1 0 ${level}
                    )
ENDFOREACH(level)

ADD_TEST(itkBinaryMorphologicalClosingImageFilterTest$ ${REVIEW_TESTS}
        --compare ${BASELINE}/itkBinaryMorphologicalClosingImageFilterTest.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTest.png
                  itkBinaryMorphologicalClosingImageFilterTest
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTest.png
                  40 1 200
                  )

ADD_TEST(itkBinaryMorphologicalClosingImageFilterTestUnsafe$ ${REVIEW_TESTS}
        --compare ${BASELINE}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png
                  itkBinaryMorphologicalClosingImageFilterTest
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryMorphologicalClosingImageFilterTestUnsafe.png
                  40 0 200
                  )

ADD_TEST(itkBinaryMorphologicalOpeningImageFilterTest$ ${REVIEW_TESTS}
        --compare ${BASELINE}/itkBinaryMorphologicalOpeningImageFilterTest.png
                  ${TEMP}/itkBinaryMorphologicalOpeningImageFilterTest.png
                  itkBinaryMorphologicalOpeningImageFilterTest
                  ${INPUTDATA}/2th_cthead1.png
                  ${TEMP}/itkBinaryMorphologicalOpeningImageFilterTest.png
                  8 150 200
                  )
ADD_TEST(itkTimeAndMemoryProbeTest1 ${REVIEW_TESTS}
        itkTimeAndMemoryProbeTest
        )

ADD_TEST(itkBruker2DSEQImageIOTest1 ${REVIEW_TESTS}
        itkBruker2DSEQImageIOTest
        ${ITK_DATA_ROOT}/Input/DCB120604.t61/11/pdata/1/2dseq
        ${ITK_TEST_OUTPUT_DIR}/itkBruker2DSEQImageIOTest1.mha
        )

ADD_TEST(itkPhilipsRECImageIOTest1 ${REVIEW_TESTS}
        itkPhilipsRECImageIOTest
        ${ITK_DATA_ROOT}/Input/T1_MEASUREMENT_4_1.PAR
        ${ITK_TEST_OUTPUT_DIR}/itkPhilipsRECImageIOTest1.mha
        )

ADD_TEST(itkVoxBoCUBImageIOTest1 ${REVIEW_TESTS}
        itkVoxBoCUBImageIOTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.cub
        )

ADD_TEST(itkVoxBoCUBImageIOTest2 ${REVIEW_TESTS}
        itkVoxBoCUBImageIOTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume.cub
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume1.cub
        )

ADD_TEST(itkVoxBoCUBImageIOTest3 ${REVIEW_TESTS}
        itkVoxBoCUBImageIOTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume1.cub
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume1.cub.gz
        )

ADD_TEST(itkVoxBoCUBImageIOTest4 ${REVIEW_TESTS}
        itkVoxBoCUBImageIOTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume1.cub.gz
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume2.cub.gz
        )

ADD_TEST(itkVoxBoCUBImageIOTest5 ${REVIEW_TESTS}
        itkVoxBoCUBImageIOTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume2.cub.gz
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume2.cub
        )

ADD_TEST(itkVoxBoCUBImageIOTest6 ${REVIEW_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB2.mha
        itkVoxBoCUBImageIOTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume2.cub
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB2.mha
        )

ADD_TEST(itkVoxBoCUBImageIOTest7 ${REVIEW_TESTS}
  --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
            ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB3.mha
        itkVoxBoCUBImageIOTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume2.cub.gz
        ${ITK_TEST_OUTPUT_DIR}/HeadMRVolumeFromCUB3.mha
        )

ADD_TEST(itkSliceBySliceImageFilterTest ${REVIEW_TESTS}
        itkSliceBySliceImageFilterTest
        ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd
        ${ITK_TEST_OUTPUT_DIR}/itkSliceBySliceImageFilterTest.mha
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest01 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest 0  0  0.001  20.0
        200   0.01   0.08   5 10
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest02 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest 1  0  0.01  3.0
        400   0.2   0.9   10 70
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest03 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest 2  0  0.001  20.0
        200   0.01   0.08   5   10
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest04 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest 3  0  0.01  3.0
        200   0.2   0.8   5   70
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest05 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest 0  1  0.01  3.0
        400   0.2   0.08   5  70
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest06 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest 0  0  0.005  20.0
        200   0.01   0.08   5  10
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest07 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest2 
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice1.mha
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice2.mha
        ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest07.mha
        0  0  0.001  0.1
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest08 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest2 
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice1.mha
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice2.mha
        ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest08.mha
        1  0  0.001  0.1
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest09 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest2 
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice1.mha
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice2.mha
        ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest09.mha
        2  0  0.001  0.1
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest10 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest2 
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice1.mha
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice2.mha
        ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest10.mha
        3  0  0.001  0.1
        )

ADD_TEST(itkDiffeomorphicDemonsRegistrationFilterTest11 ${REVIEW_TESTS}
        itkDiffeomorphicDemonsRegistrationFilterTest2 
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice1.mha
        ${ITK_SOURCE_DIR}/Examples/Data/RatLungSlice2.mha
        ${ITK_TEST_OUTPUT_DIR}/itkDiffeomorphicDemonsRegistrationFilterTest11.mha
        0  1  0.001  0.1
        )

ADD_TEST(itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunctionTest ${REVIEW_TESTS}
        itkVectorLinearInterpolateNearestNeighborExtrapolateImageFunctionTest
        )

ADD_TEST(itkVectorCentralDifferenceImageFunctionTest ${REVIEW_TESTS}
        itkVectorCentralDifferenceImageFunctionTest
        )

ADD_TEST(itkExponentialDeformationFieldImageFilterTest ${REVIEW_TESTS}
        itkExponentialDeformationFieldImageFilterTest
        )

ADD_TEST(itkFastSymmetricForcesDemonsRegistrationFilterTest ${REVIEW_TESTS}
        itkFastSymmetricForcesDemonsRegistrationFilterTest
        )

ADD_TEST(itkGridForwardWarpImageFilterTest ${REVIEW_TESTS}
        itkGridForwardWarpImageFilterTest
        )

ADD_TEST(itkWarpHarmonicEnergyCalculatorTest ${REVIEW_TESTS}
        itkWarpHarmonicEnergyCalculatorTest
        )

ADD_TEST(itkBSplineScatteredDataPointSetToImageFilterTest01 ${REVIEW_TESTS}
        --compare
          ${BASELINE}/itkBSplineScatteredDataPointSetToImageFilterTest01.mha
          ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest01.mha
        itkBSplineScatteredDataPointSetToImageFilterTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/cake_easy.png
        ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredDataPointSetToImageFilterTest01.mha
        )

ADD_TEST(itkBSplineScatteredDataPointSetToImageFilterTest02 ${REVIEW_TESTS}
        itkBSplineScatteredDataPointSetToImageFilterTest2
        ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredApproximationDataPointsTestOutput2.txt
        )

ADD_TEST(itkBSplineScatteredDataPointSetToImageFilterTest03 ${REVIEW_TESTS}
        itkBSplineScatteredDataPointSetToImageFilterTest3
        ${ITK_SOURCE_DIR}/Testing/Data/Input/BSplineScatteredApproximationDataPointsInput.txt
        ${ITK_TEST_OUTPUT_DIR}/itkBSplineScatteredApproximationDataPointsTestOutput3.txt
        )

ADD_TEST(itkCoxDeBoorBSplineKernelFunctionTest01 ${REVIEW_TESTS}
        itkCoxDeBoorBSplineKernelFunctionTest
        )

ADD_TEST(itkCoxDeBoorBSplineKernelFunctionTest02 ${REVIEW_TESTS}
        itkCoxDeBoorBSplineKernelFunctionTest2
        )

ADD_TEST(itkKappaSigmaThresholdImageCalculatorTest01 ${REVIEW_TESTS}
        itkKappaSigmaThresholdImageCalculatorTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/CellsFluorescence1.png
        10 3.5 19
        )

ADD_TEST(itkKappaSigmaThresholdImageCalculatorTest02 ${REVIEW_TESTS}
        itkKappaSigmaThresholdImageCalculatorTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/CellsFluorescence2.png
        10 3.0 19
        )

ADD_TEST(itkKappaSigmaThresholdImageFilterTest01 ${REVIEW_TESTS}
        itkKappaSigmaThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/CellsFluorescence1.png
        ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest01.mha
        10 3.5
        )

ADD_TEST(itkKappaSigmaThresholdImageFilterTest02 ${REVIEW_TESTS}
        itkKappaSigmaThresholdImageFilterTest
        ${ITK_SOURCE_DIR}/Testing/Data/Input/CellsFluorescence2.png
        ${ITK_TEST_OUTPUT_DIR}/itkKappaSigmaThresholdImageFilterTest02.mha
        10 3.0
        )

ADD_TEST(itkMultiplyByConstantImageFilterTest ${REVIEW_TESTS}
        itkMultiplyByConstantImageFilterTest
        )

ADD_TEST(itkDivideByConstantImageFilterTest ${REVIEW_TESTS}
        itkDivideByConstantImageFilterTest
        )

ADD_TEST(itkAddConstantToImageFilterTest ${REVIEW_TESTS}
        itkAddConstantToImageFilterTest
        )

ADD_TEST(itkSubtractConstantFromImageFilterTest ${REVIEW_TESTS}
        itkSubtractConstantFromImageFilterTest
        )

ADD_TEST(itkImageReadRealAndImaginaryWriteComplexTest ${REVIEW_TESTS}
        itkImageReadRealAndImaginaryWriteComplexTest
        ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
        ${ITK_SOURCE_DIR}/Examples/Data/BrainT1Slice.png
        ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplex.mha
        )

ADD_TEST(itkImageReadComplexWriteMagnitudeAndPhaseTest ${REVIEW_TESTS}
        itkImageReadComplexWriteMagnitudeAndPhaseTest
        ${ITK_SOURCE_DIR}/Examples/Data/itkBrainSliceComplex.mha
        ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplexMagnitude.mha
        ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplexPhase.mha
        )

ADD_TEST(itkImageReadMagnitudeAndPhaseWriteComplexTest ${REVIEW_TESTS}
        itkImageReadMagnitudeAndPhaseWriteComplexTest
        ${ITK_SOURCE_DIR}/Examples/Data/itkBrainSliceComplexMagnitude.mha
        ${ITK_SOURCE_DIR}/Examples/Data/itkBrainSliceComplexPhase.mha
        ${ITK_TEST_OUTPUT_DIR}/itkBrainSliceComplex.mha
        )

ADD_TEST(itkComposeRGBAImageFilterTest ${REVIEW_TESTS}
    itkComposeRGBAImageFilterTest
    ${ITK_TEST_OUTPUT_DIR}/itkComposeRGBAImageFilterTestOutput.png
    ${ITK_SOURCE_DIR}/Testing/Data/Input/Number1inText.png
    ${ITK_SOURCE_DIR}/Testing/Data/Input/Number2inText.png
    ${ITK_SOURCE_DIR}/Testing/Data/Input/Number3inText.png
    ${ITK_SOURCE_DIR}/Testing/Data/Input/IntensityRamp64.png
)

ADD_TEST(itkTransformToDeformationFieldSourceTest01 ${REVIEW_TESTS}
    itkTransformToDeformationFieldSourceTest
    Affine
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestField01.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestImage01.mha
)

ADD_TEST(itkTransformToDeformationFieldSourceTest02 ${REVIEW_TESTS}
    itkTransformToDeformationFieldSourceTest
    BSpline
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestField02.mha
    ${INPUTDATA}/parametersBSpline.txt
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToDeformationFieldSourceTestImage02.mha
)

ADD_TEST(itkTransformToDeformationFieldSourceTest03 ${REVIEW_TESTS}
   --compare ${TEMP}/transformedImage.nii
             ${TEMP}/warpedImage.nii
  --compareNumberOfPixelsTolerance 20
   itkTransformToDeformationFieldSourceTest1
     ${TEMP}/transformedImage.nii
     ${TEMP}/warpedImage.nii
)

ADD_TEST(itkBoxMeanImageFilterTest3 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkBoxMeanImageFilter3.png
            ${TEMP}/itkBoxMeanImageFilter3.png
  itkBoxMeanImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkBoxMeanImageFilter3.png
  3
)

ADD_TEST(itkBoxMeanImageFilterTest10 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkBoxMeanImageFilter10.png
            ${TEMP}/itkBoxMeanImageFilter10.png
  itkBoxMeanImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkBoxMeanImageFilter10.png
  10
)


ADD_TEST(itkBoxSigmaImageFilterTest3 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkBoxSigmaImageFilter3.png
            ${TEMP}/itkBoxSigmaImageFilter3.png
  itkBoxSigmaImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkBoxSigmaImageFilter3.png
  3
)

ADD_TEST(itkBoxSigmaImageFilterTest10 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkBoxSigmaImageFilter10.png
            ${TEMP}/itkBoxSigmaImageFilter10.png
  itkBoxSigmaImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkBoxSigmaImageFilter10.png
  10
)


ADD_TEST(itkRankImageFilterTest3 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkRankImageFilter3.png
            ${TEMP}/itkRankImageFilter3.png
  itkRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkRankImageFilter3.png
  3
)

ADD_TEST(itkRankImageFilterTest10 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkRankImageFilter10.png
            ${TEMP}/itkRankImageFilter10.png
  itkRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkRankImageFilter10.png
  10
)


ADD_TEST(itkMapRankImageFilterTest3 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkRankImageFilter3.png
            ${TEMP}/itkMapRankImageFilter3.png
  itkMapRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapRankImageFilter3.png
  3
)

ADD_TEST(itkMapRankImageFilterTest10 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkRankImageFilter10.png
            ${TEMP}/itkMapRankImageFilter10.png
  itkMapRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkMapRankImageFilter10.png
  10
)


ADD_TEST(itkMaskedRankImageFilterTest3 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkMaskedRankImageFilter3.png
            ${TEMP}/itkMaskedRankImageFilter3.png
  itkMaskedRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${INPUTDATA}/cthead1-mask.png
  ${TEMP}/itkMaskedRankImageFilter3.png
  3
)

ADD_TEST(itkMaskedRankImageFilterTest10 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkMaskedRankImageFilter10.png
            ${TEMP}/itkMaskedRankImageFilter10.png
  itkMaskedRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${INPUTDATA}/cthead1-mask.png
  ${TEMP}/itkMaskedRankImageFilter10.png
  10
)


ADD_TEST(itkMapMaskedRankImageFilterTest3 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkMaskedRankImageFilter3.png
            ${TEMP}/itkMapMaskedRankImageFilter3.png
  itkMapMaskedRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${INPUTDATA}/cthead1-mask.png
  ${TEMP}/itkMapMaskedRankImageFilter3.png
  3
)

ADD_TEST(itkMapMaskedRankImageFilterTest10 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkMaskedRankImageFilter10.png
            ${TEMP}/itkMapMaskedRankImageFilter10.png
  itkMapMaskedRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${INPUTDATA}/cthead1-mask.png
  ${TEMP}/itkMapMaskedRankImageFilter10.png
  10
)


ADD_TEST(itkFastApproximateRankImageFilterTest3 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkFastApproximateRankImageFilter3.png
            ${TEMP}/itkFastApproximateRankImageFilter3.png
  itkFastApproximateRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkFastApproximateRankImageFilter3.png
  3
)

ADD_TEST(itkFastApproximateRankImageFilterTest10 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkFastApproximateRankImageFilter10.png
            ${TEMP}/itkFastApproximateRankImageFilter10.png
  itkFastApproximateRankImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkFastApproximateRankImageFilter10.png
  10
)

IF(ITK_USE_CONSOLIDATED_MORPHOLOGY)
  SET(ReviewTest_SRCS
    ${ReviewTest_SRCS}
    itkOptGrayscaleMorphologicalClosingImageFilterTest.cxx
    itkOptGrayscaleMorphologicalOpeningImageFilterTest.cxx
    itkOptGrayscaleDilateImageFilterTest.cxx
    itkOptGrayscaleErodeImageFilterTest.cxx
    itkMapOptGrayscaleMorphologicalClosingImageFilterTest.cxx
    itkMapOptGrayscaleMorphologicalOpeningImageFilterTest.cxx
    itkMapOptGrayscaleDilateImageFilterTest.cxx
    itkMapOptGrayscaleErodeImageFilterTest.cxx
  )
  ADD_TEST(itkOptGrayscaleMorphologicalClosingImageFilterTest ${REVIEW_TESTS}
    --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestHisto.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestVHGW.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestAnchor.png
    itkOptGrayscaleMorphologicalClosingImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestHisto.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestVHGW.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestAnchor.png
    0
  )
  ADD_TEST(itkOptGrayscaleMorphologicalClosingImageFilterTestSafeBorder ${REVIEW_TESTS}
    --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestHistoSafeBorder.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestVHGWSafeBorder.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestAnchorSafeBorder.png
    itkOptGrayscaleMorphologicalClosingImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestHistoSafeBorder.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestVHGWSafeBorder.png
    ${TEMP}/itkOptGrayscaleMorphologicalClosingImageFilterTestAnchorSafeBorder.png
    1
  )
  ADD_TEST(itkOptGrayscaleMorphologicalOpeningImageFilterTest ${REVIEW_TESTS}
    --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestHisto.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestVHGW.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestAnchor.png
    itkOptGrayscaleMorphologicalOpeningImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestHisto.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestVHGW.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestAnchor.png
    0
  )
  ADD_TEST(itkOptGrayscaleMorphologicalOpeningImageFilterTestSafeBorder ${REVIEW_TESTS}
    --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestHistoSafeBorder.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestVHGWSafeBorder.png
    --compare ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestAnchorSafeBorder.png
    itkOptGrayscaleMorphologicalOpeningImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestHistoSafeBorder.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestVHGWSafeBorder.png
    ${TEMP}/itkOptGrayscaleMorphologicalOpeningImageFilterTestAnchorSafeBorder.png
    1
  )
  ADD_TEST(itkOptGrayscaleDilateImageFilterTest ${REVIEW_TESTS}
    --compare ${TEMP}/itkOptGrayscaleDilateImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleDilateImageFilterTestHisto.png
    --compare ${TEMP}/itkOptGrayscaleDilateImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleDilateImageFilterTestVHGW.png
    --compare ${TEMP}/itkOptGrayscaleDilateImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleDilateImageFilterTestAnchor.png
    itkOptGrayscaleDilateImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkOptGrayscaleDilateImageFilterTestBasic.png
    ${TEMP}/itkOptGrayscaleDilateImageFilterTestHisto.png
    ${TEMP}/itkOptGrayscaleDilateImageFilterTestVHGW.png
    ${TEMP}/itkOptGrayscaleDilateImageFilterTestAnchor.png
  )
  ADD_TEST(itkOptGrayscaleErodeImageFilterTest ${REVIEW_TESTS}
    --compare ${TEMP}/itkOptGrayscaleErodeImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleErodeImageFilterTestHisto.png
    --compare ${TEMP}/itkOptGrayscaleErodeImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleErodeImageFilterTestVHGW.png
    --compare ${TEMP}/itkOptGrayscaleErodeImageFilterTestBasic.png
              ${TEMP}/itkOptGrayscaleErodeImageFilterTestAnchor.png
    itkOptGrayscaleErodeImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkOptGrayscaleErodeImageFilterTestBasic.png
    ${TEMP}/itkOptGrayscaleErodeImageFilterTestHisto.png
    ${TEMP}/itkOptGrayscaleErodeImageFilterTestVHGW.png
    ${TEMP}/itkOptGrayscaleErodeImageFilterTestAnchor.png
  )

  ADD_TEST(itkMapOptGrayscaleMorphologicalClosingImageFilterTest ${REVIEW_TESTS}
    --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
              ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestHisto.png
    --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
              ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestVHGW.png
    --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
              ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestAnchor.png
    itkMapOptGrayscaleMorphologicalClosingImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasic.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestHisto.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestVHGW.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestAnchor.png
    0
  )
  ADD_TEST(itkMapOptGrayscaleMorphologicalClosingImageFilterTestSafeBorder ${REVIEW_TESTS}
    --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestHistoSafeBorder.png
    --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestVHGWSafeBorder.png
    --compare ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestAnchorSafeBorder.png
    itkMapOptGrayscaleMorphologicalClosingImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestBasicSafeBorder.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestHistoSafeBorder.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestVHGWSafeBorder.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalClosingImageFilterTestAnchorSafeBorder.png
    1
  )
  ADD_TEST(itkMapOptGrayscaleMorphologicalOpeningImageFilterTest ${REVIEW_TESTS}
    --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
              ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestHisto.png
    --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
              ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestVHGW.png
    --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
              ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestAnchor.png
    itkMapOptGrayscaleMorphologicalOpeningImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasic.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestHisto.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestVHGW.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestAnchor.png
    0
  )
  ADD_TEST(itkMapOptGrayscaleMorphologicalOpeningImageFilterTestSafeBorder ${REVIEW_TESTS}
    --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestHistoSafeBorder.png
    --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestVHGWSafeBorder.png
    --compare ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
              ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestAnchorSafeBorder.png
    itkMapOptGrayscaleMorphologicalOpeningImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestBasicSafeBorder.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestHistoSafeBorder.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestVHGWSafeBorder.png
    ${TEMP}/itkMapOptGrayscaleMorphologicalOpeningImageFilterTestAnchorSafeBorder.png
    1
  )
  ADD_TEST(itkMapOptGrayscaleDilateImageFilterTest ${REVIEW_TESTS}
    --compare ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestBasic.png
              ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestHisto.png
    --compare ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestBasic.png
              ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestVHGW.png
    --compare ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestBasic.png
              ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestAnchor.png
    itkMapOptGrayscaleDilateImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestBasic.png
    ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestHisto.png
    ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestVHGW.png
    ${TEMP}/itkMapOptGrayscaleDilateImageFilterTestAnchor.png
  )
  ADD_TEST(itkMapOptGrayscaleErodeImageFilterTest ${REVIEW_TESTS}
    --compare ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestBasic.png
              ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestHisto.png
    --compare ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestBasic.png
              ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestVHGW.png
    --compare ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestBasic.png
              ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestAnchor.png
    itkMapOptGrayscaleErodeImageFilterTest
    ${INPUTDATA}/cthead1.png
    ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestBasic.png
    ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestHisto.png
    ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestVHGW.png
    ${TEMP}/itkMapOptGrayscaleErodeImageFilterTestAnchor.png
  )
ENDIF(ITK_USE_CONSOLIDATED_MORPHOLOGY)

ADD_TEST(itkDiscreteGaussianDerivativeImageFunctionTest01 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFunctionTest01.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFunctionTest01.png
  itkDiscreteGaussianDerivativeImageFunctionTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFunctionTest01.png
  1
  2.0
  0.01
  32
)

ADD_TEST(itkDiscreteGradientMagnitudeGaussianImageFunctionTest01 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkDiscreteGradientMagnitudeGaussianImageFunctionTest01.png
            ${TEMP}/itkDiscreteGradientMagnitudeGaussianImageFunctionTest01.png
  itkDiscreteGradientMagnitudeGaussianImageFunctionTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkDiscreteGradientMagnitudeGaussianImageFunctionTest01.png
  2.0
  0.01
  32
)

ADD_TEST(itkDiscreteHessianGaussianImageFunctionTest01 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkDiscreteHessianGaussianImageFunctionTest01_0.mha
            ${TEMP}/itkDiscreteHessianGaussianImageFunctionTest01_0.mha
  --compare ${BASELINE}/itkDiscreteHessianGaussianImageFunctionTest01_1.mha
            ${TEMP}/itkDiscreteHessianGaussianImageFunctionTest01_1.mha
  --compare ${BASELINE}/itkDiscreteHessianGaussianImageFunctionTest01_2.mha
            ${TEMP}/itkDiscreteHessianGaussianImageFunctionTest01_2.mha
  itkDiscreteHessianGaussianImageFunctionTest
  ${INPUTDATA}/HeadMRVolume.mhd
  ${TEMP}/itkDiscreteHessianGaussianImageFunctionTest01_%i.mha
  2.0
  0.01
  20
)

ADD_TEST(itkDiscreteGaussianDerivativeImageFilterTest01 ${REVIEW_TESTS3}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest01.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest01.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest01.png
  0    # orderX
  1    # orderY
  3.0  # sigma
)

ADD_TEST(itkDiscreteGaussianDerivativeImageFilterTest10 ${REVIEW_TESTS3}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest10.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest10.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest10.png
  1    # orderX
  0    # orderY
  3.0  # sigma
)

ADD_TEST(itkDiscreteGaussianDerivativeImageFilterTest11 ${REVIEW_TESTS3}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest11.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest11.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest11.png
  1    # orderX
  1    # orderY
  3.0  # sigma
)

ADD_TEST(itkDiscreteGaussianDerivativeImageFilterTest20 ${REVIEW_TESTS3}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest20.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest20.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest20.png
  2    # orderX
  0    # orderY
  3.0  # sigma
)

ADD_TEST(itkDiscreteGaussianDerivativeImageFilterTest02 ${REVIEW_TESTS3}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest02.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest02.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest02.png
  0    # orderX
  2    # orderY
  3.0  # sigma
)

ADD_TEST(itkDiscreteGaussianDerivativeImageFilterTest21 ${REVIEW_TESTS3}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest21.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest21.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest21.png
  2    # orderX
  1    # orderY
  3.0  # sigma
)

ADD_TEST(itkDiscreteGaussianDerivativeImageFilterTest12 ${REVIEW_TESTS3}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest12.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest12.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest12.png
  1    # orderX
  2    # orderY
  3.0  # sigma
)

ADD_TEST(itkDiscreteGaussianDerivativeImageFilterTest30 ${REVIEW_TESTS3}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest30.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest30.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest30.png
  3    # orderX
  0    # orderY
  3.0  # sigma
)

ADD_TEST(itkDiscreteGaussianDerivativeImageFilterTest03 ${REVIEW_TESTS3}
  --compare ${BASELINE}/itkDiscreteGaussianDerivativeImageFilterTest03.png
            ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest03.png
  itkDiscreteGaussianDerivativeImageFilterTest
  ${INPUTDATA}/whiteSpot.png
  ${TEMP}/itkDiscreteGaussianDerivativeImageFilterTest03.png
  0    # orderX
  3    # orderY
  3.0  # sigma
)

ADD_TEST(itkBinaryContourImageFilterTest0 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkBinaryContourImageFilterTest0.png
            ${TEMP}/itkBinaryContourImageFilterTest0.png
   itkBinaryContourImageFilterTest
   ${INPUTDATA}/2th_cthead1.png
   ${TEMP}/itkBinaryContourImageFilterTest0.png
   0 200 0
)

ADD_TEST(itkBinaryContourImageFilterTest1 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkBinaryContourImageFilterTest1.png
            ${TEMP}/itkBinaryContourImageFilterTest1.png
   itkBinaryContourImageFilterTest
   ${INPUTDATA}/2th_cthead1.png
   ${TEMP}/itkBinaryContourImageFilterTest1.png
   1 200 0
)

ADD_TEST(itkLabelContourImageFilterTest0 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkLabelContourImageFilterTest0.png
            ${TEMP}/itkLabelContourImageFilterTest0.png
   itkLabelContourImageFilterTest
   ${INPUTDATA}/2th_cthead1.png
   ${TEMP}/itkLabelContourImageFilterTest0.png
   0 0
)

ADD_TEST(itkLabelContourImageFilterTest1 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkLabelContourImageFilterTest1.png
            ${TEMP}/itkLabelContourImageFilterTest1.png
   itkLabelContourImageFilterTest
   ${INPUTDATA}/2th_cthead1.png
   ${TEMP}/itkLabelContourImageFilterTest1.png
   1 0
)

ADD_TEST(itkFFTShiftImageFilterTestOdd0 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkFFTShiftImageFilterTest0.png
            ${TEMP}/itkFFTShiftImageFilterTest0.png
   itkFFTShiftImageFilterTest
   ${INPUTDATA}/BrainSliceBinary.png
   ${TEMP}/itkFFTShiftImageFilterTest0.png
   0
)

ADD_TEST(itkFFTShiftImageFilterTestOdd1 ${REVIEW_TESTS}
  --compare ${INPUTDATA}/BrainSliceBinary.png
            ${TEMP}/itkFFTShiftImageFilterTest1.png
   itkFFTShiftImageFilterTest
   ${BASELINE}/itkFFTShiftImageFilterTest0.png
   ${TEMP}/itkFFTShiftImageFilterTest1.png
   1
)

ADD_TEST(itkFFTShiftImageFilterTestEven0 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkFFTShiftImageFilterTestEven0.png
            ${TEMP}/itkFFTShiftImageFilterTestEven0.png
   itkFFTShiftImageFilterTest
   ${INPUTDATA}/cthead1.png
   ${TEMP}/itkFFTShiftImageFilterTestEven0.png
   0
)

ADD_TEST(itkFFTShiftImageFilterTestEven1 ${REVIEW_TESTS}
  --compare ${INPUTDATA}/cthead1.png
            ${TEMP}/itkFFTShiftImageFilterTestEven1.png
   itkFFTShiftImageFilterTest
   ${BASELINE}/itkFFTShiftImageFilterTestEven0.png
   ${TEMP}/itkFFTShiftImageFilterTestEven1.png
   1
)

ADD_TEST(itkConvolutionImageFilterTest5x5Mean ${REVIEW_TESTS}
  --compare ${BASELINE}/itkConvolutionImageFilterTest5x5Mean.png
            ${TEMP}/itkConvolutionImageFilterTest5x5Mean.png
   itkConvolutionImageFilterTestInt
   ${INPUTDATA}/cthead1.png
   ${INPUTDATA}/5x5-constant.png
   ${TEMP}/itkConvolutionImageFilterTest5x5Mean.png
   1
)

ADD_TEST(itkConvolutionImageFilterTestSobelX ${REVIEW_TESTS}
  --compare ${BASELINE}/itkConvolutionImageFilterTestSobelX.nii.gz
            ${TEMP}/itkConvolutionImageFilterTestSobelX.nii.gz
   itkConvolutionImageFilterTest
   ${INPUTDATA}/cthead1.png
   ${INPUTDATA}/sobel_x.nii.gz
   ${TEMP}/itkConvolutionImageFilterTestSobelX.nii.gz
)

ADD_TEST(itkConvolutionImageFilterTestSobelY ${REVIEW_TESTS}
  --compare ${BASELINE}/itkConvolutionImageFilterTestSobelY.nii.gz
            ${TEMP}/itkConvolutionImageFilterTestSobelY.nii.gz
   itkConvolutionImageFilterTest
   ${INPUTDATA}/cthead1.png
   ${INPUTDATA}/sobel_y.nii.gz
   ${TEMP}/itkConvolutionImageFilterTestSobelY.nii.gz
)

ADD_TEST(itkRobustAutomaticThresholdImageFilterTest ${REVIEW_TESTS}
  --compare ${BASELINE}/itkRobustAutomaticThresholdImageFilterTest.png
            ${TEMP}/itkRobustAutomaticThresholdImageFilterTest.png
  itkRobustAutomaticThresholdImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkRobustAutomaticThresholdImageFilterTest.png
  2
)

ADD_TEST( itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1 ${REVIEW_TESTS}
   itkShapedFloodFilledImageFunctionConditionalConstIteratorTest1
   ${INPUTDATA}/8ConnectedImage.bmp )
ADD_TEST( itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2 
          ${REVIEW_TESTS} 
          itkShapedFloodFilledImageFunctionConditionalConstIteratorTest2 )
ADD_TEST( itkShapedFloodFilledImageFunctionConditionalConstIteratorTest3
          ${REVIEW_TESTS} 
          itkShapedFloodFilledImageFunctionConditionalConstIteratorTest3 )

ADD_TEST(itkAreaOpeningImageFilterTest0 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkAreaOpeningImageFilterTest0.png
            ${TEMP}/itkAreaOpeningImageFilterTest0.png
  itkAreaOpeningImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkAreaOpeningImageFilterTest0.png
  1000 0 1
)

ADD_TEST(itkAreaClosingImageFilterTest0 ${REVIEW_TESTS}
  --compare ${BASELINE}/itkAreaClosingImageFilterTest0.png
            ${TEMP}/itkAreaClosingImageFilterTest0.png
  itkAreaClosingImageFilterTest
  ${INPUTDATA}/cthead1.png
  ${TEMP}/itkAreaClosingImageFilterTest0.png
  1000 0 0
)

ADD_TEST(itkHessianToObjectnessMeasureImageFilterTest ${REVIEW_TESTS}
  itkHessianToObjectnessMeasureImageFilterTest
  ${INPUTDATA}/DSA.png
  ${TEMP}/itkHessianToObjectnessMeasureImageFilterTestOutput.mha
  1 0)

ADD_TEST(itkHessianToObjectnessMeasureImageFilterTest2 ${REVIEW_TESTS}
  itkHessianToObjectnessMeasureImageFilterTest
  ${INPUTDATA}/DSA.png
  ${TEMP}/itkHessianToObjectnessMeasureImageFilterTestOutput2.mha
  0 0)


ADD_TEST(itkMultiScaleHessianBasedMeasureImageFilterTest ${REVIEW_TESTS}
  itkMultiScaleHessianBasedMeasureImageFilterTest
  ${INPUTDATA}/DSA.png
  ${TEMP}/itkMultiScaleHessianBasedMeasureImageFilterTestEnhancedOutput.mha
  ${TEMP}/itkMultiScaleHessianBasedMeasureImageFilterTestScalesOutput.mha
   5  # Minimum Sigma
  10  # Maximum Sigma
  10  # Number of Sigma steps
   1  # Object Dimension (search for tubes)
   0  # Search for Dark objects over bright background
  ${TEMP}/itkMultiScaleHessianBasedMeasureImageFilterTestEnhancedOutput2.mha # Output image with equispaced sigma steps
  )

ADD_TEST(itkAggregateLabelMapFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-labelAggregate.mha
            ${TEMP}/cthead1-labelAggregate.mha
  itkAggregateLabelMapFilterTest 
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-labelAggregate.mha
  )

ADD_TEST(itkBinaryImageToLabelMapFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-labeled.mha
            ${TEMP}/cthead1-labeled.mha
  itkBinaryImageToLabelMapFilterTest
  ${INPUTDATA}/cthead1-mask.png
  ${TEMP}/cthead1-labeled.mha
  1     # Fully connected
  255   # Input foreground
  0     # Output background
  0     # Expect failure
  )

ADD_TEST(itkBinaryImageToLabelMapFilterTest2
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/connected0lines-0.png
            ${TEMP}/connected0lines-0.png
  itkBinaryImageToLabelMapFilterTest
  ${INPUTDATA}/connected0lines.png
  ${TEMP}/connected0lines-0.png
  0     # Not fully connected
  255   # Input foreground
  0     # Output background
  0     # Expect failure
  )

ADD_TEST(itkBinaryImageToLabelMapFilterTest3
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/connected0lines-1.png
            ${TEMP}/connected0lines-1.png
  itkBinaryImageToLabelMapFilterTest
  ${INPUTDATA}/connected0lines.png
  ${TEMP}/connected0lines-1.png
  1     # Fully connected
  255   # Input foreground
  0     # Output background
  0     # Expect failure
  )

ADD_TEST(itkBinaryImageToLabelMapFilterTest4
  ${REVIEW_TESTS3}
  itkBinaryImageToLabelMapFilterTest
  ${INPUTDATA}/289.png
  ${TEMP}/tooManyObjects.png
  0     # Not fully connected
  255   # Input foreground
  0     # Output background
  1     # Expect failure
  )

ADD_TEST(itkBinaryImageToLabelMapFilterTest5
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/nonConnected3DLines-0.tif
            ${TEMP}/nonConnected3DLines-0.tif
  itkBinaryImageToLabelMapFilterTest
  ${INPUTDATA}/nonConnected3DLines.tif
  ${TEMP}/nonConnected3DLines-0.tif
  0     # Not fully connected
  255   # Input foreground
  0     # Output background
  0     # Expect failure
  )

ADD_TEST(itkLabelMapToBinaryImageFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-label-binary.mha
            ${TEMP}/cthead1-label-binary.mha
  itkLabelMapToBinaryImageFilterTest
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-binary.mha
  255   # Input foreground
  0     # Output background
  )

ADD_TEST(itkChangeLabelLabelMapFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-label-changed.mha
            ${TEMP}/cthead1-label-changed.mha
  itkChangeLabelLabelMapFilterTest
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-changed.mha
   94  150  # ( Old, New ) Label pair
   92  163  # ( Old, New ) Label pair
  176  176  # ( Old, New ) Label pair
  )

ADD_TEST(itkChangeLabelLabelMapFilterTest2
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-label-changed2.mha
            ${TEMP}/cthead1-label-changed2.mha
  itkChangeLabelLabelMapFilterTest
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-changed2.mha
   94  100  # ( Old, New ) Label pair
    0   50  # ( Old, New ) Label pair background
   92    0
  )

ADD_TEST(itkChangeLabelLabelMapFilterTest3
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-label-changed3.png
            ${TEMP}/cthead1-label-changed3.png
  itkChangeLabelLabelMapFilterTest
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-changed3.png
    0   94  # ( Old, New ) Label pair background
  )

ADD_TEST(itkRelabelLabelMapFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-label-relabeled.mha
            ${TEMP}/cthead1-label-relabeled.mha
  itkRelabelLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-relabeled.mha
  )

ADD_TEST(itkShiftScaleLabelMapFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-label-shiftscaled.mha
            ${TEMP}/cthead1-label-shiftscaled.mha
  itkShiftScaleLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-shiftscaled.mha
   10   # shift value
   0.5  # scale value
   true # change background (boolean)
  )

ADD_TEST(itkAutoCropLabelMapFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-label-autocrop.mha
            ${TEMP}/cthead1-label-autocrop.mha
  itkAutoCropLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-autocrop.mha
  176  # Value to use as background of input image
  4    # border size
  4    # border size
  )

ADD_TEST(itkChangeRegionLabelMapFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-label-changeregion.mha
            ${TEMP}/cthead1-label-changeregion.mha
  itkChangeRegionLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-changeregion.mha
  50  100  # Start Index 
  120 150  # Size
  )

ADD_TEST(itkMergeLabelMapFilterTest1a
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/simple-label-merge-keep.mha
            ${TEMP}/simple-label-merge-keep.mha
  itkMergeLabelMapFilterTest1
  ${INPUTDATA}/simple-label-c.png
  ${INPUTDATA}/simple-label-d.png  
  ${TEMP}/simple-label-merge-keep.mha
  50   # Background value 1
  50   # Background value 2
  0    # Keep Method
  0    # Expect success
  )

ADD_TEST(itkMergeLabelMapFilterTest1b
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/simple-label-merge-aggregate.mha
            ${TEMP}/simple-label-merge-aggregate.mha
  itkMergeLabelMapFilterTest1
  ${INPUTDATA}/simple-label-c.png
  ${INPUTDATA}/simple-label-d.png  
  ${TEMP}/simple-label-merge-aggregate.mha
  50   # Background value 1
  50   # Background value 2
  1    # Aggregate Method
  0    # Expect success
  )

ADD_TEST(itkMergeLabelMapFilterTest1c
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/simple-label-merge-pack.mha
            ${TEMP}/simple-label-merge-pack.mha
  itkMergeLabelMapFilterTest1
  ${INPUTDATA}/simple-label-c.png
  ${INPUTDATA}/simple-label-d.png  
  ${TEMP}/simple-label-merge-pack.mha
  50   # Background value 1
  50   # Background value 2
  2    # Pack Method
  0    # Expect success
  )


ADD_TEST(itkMergeLabelMapFilterTest1d
  ${REVIEW_TESTS3}
  itkMergeLabelMapFilterTest1
  ${INPUTDATA}/simple-label-c.png
  ${INPUTDATA}/simple-label-d.png  
  ${TEMP}/thisFileShouldNotExist.mha
  50   # Background value 1
  50   # Background value 2
  3    # Strict Method
  1    # Expect FAILURE
  )

ADD_TEST(itkMergeLabelMapFilterTest1e
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/simple-label-merge-strict.mha
            ${TEMP}/simple-label-merge-strict.mha
  itkMergeLabelMapFilterTest1
  ${INPUTDATA}/simple-label-e.png
  ${INPUTDATA}/simple-label-d.png  
  ${TEMP}/simple-label-merge-strict.mha
  50   # Background value 1
  50   # Background value 2
  3    # Strict Method
  0    # Expect success
  )

ADD_TEST(itkMergeLabelMapFilterTest1f
  ${REVIEW_TESTS3}
  itkMergeLabelMapFilterTest1
  ${INPUTDATA}/simple-label-e.png
  ${INPUTDATA}/simple-label-d.png  
  ${TEMP}/simple-label-merge-wrong.mha
  50   # Background value 1
  50   # Background value 2
  10   # wrong Method
  1    # Expect failure
  )


ADD_TEST(itkLabelImageToShapeLabelMapFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/simple-label-to-shapelabelmap.mha
            ${TEMP}/simple-label-to-shapelabelmap.mha
  itkLabelImageToShapeLabelMapFilterTest1
  ${INPUTDATA}/simple-label-b.png
  ${TEMP}/simple-label-to-shapelabelmap.mha
  90    #output background value
  1     #compute Feret Diameter (bool)
  1     #computer Perimeter (bool)
  )  

ADD_TEST(itkCropLabelMapFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-label-crop.mha
            ${TEMP}/cthead1-label-crop.mha
  itkCropLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-crop.mha
  40    # Horizontal left and right crop border
  50    # Vertical upper and lower crop border
  )

ADD_TEST(itkPadLabelMapFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-label-pad.mha
            ${TEMP}/cthead1-label-pad.mha
  itkPadLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-label-pad.mha
  40    # Horizontal left and right pad border 
  50    # Vertical upper and lower pad border 
  )

ADD_TEST(itkRegionFromReferenceLabelMapFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-label-regionreference.mha
            ${TEMP}/cthead1-label-regionreference.mha
  itkRegionFromReferenceLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${INPUTDATA}/circle.png  #TODO replace with reference image
  ${TEMP}/cthead1-label-regionreference.mha
  )

ADD_TEST(itkBinaryImageToShapeLabelMapFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/Spots-binaryimage-to-shapelabel.mha
            ${TEMP}/Spots-binaryimage-to-shapelabel.mha
  itkBinaryImageToShapeLabelMapFilterTest1
  ${INPUTDATA}/Spots.png
  ${TEMP}/Spots-binaryimage-to-shapelabel.mha
  1     # Fully connected (bool)
  255   # Input image foreground
  0     # Output image background
  1     # Compute Feret Diameter (bool)
  1     # Compute Perimeter (bool)
  )

ADD_TEST(itkShapeKeepNObjectsLabelMapFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-keep-n-objects.mha
            ${TEMP}/cthead1-shape-keep-n-objects.mha
  itkShapeKeepNObjectsLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-shape-keep-n-objects.mha
  0         # Reverse Ordering (bool) - Objects with values smaller than Lambda are removed  
  0         # Attribute
  2         # Number of objects to keep
  )

ADD_TEST(itkShapeOpeningLabelMapFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-shape-opening.mha
            ${TEMP}/cthead1-shape-opening.mha
  itkShapeOpeningLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-shape-opening.mha
  140   # Lambda
  1     # Reverse Ordering (bool) - Objects with values smaller than Lambda are removed 
  0     # Attribute
  )

ADD_TEST(itkBinaryShapeOpeningImageFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-binary-shape-opening.mha
            ${TEMP}/cthead1-binary-shape-opening.mha
  itkBinaryShapeOpeningImageFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-binary-shape-opening.mha
  255   # Input image foreground
  0     # Output image background
  140   # Lambda
  1     # Reverse Ordering (bool) - Objects with values smaller than Lambda are removed
  1     # Fully connected (bool) 
  100   # Attribute (bool)
  )

ADD_TEST(itkShapeRelabelLabelMapFilterTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/cthead1-shape-relabel-labelmap.mha
            ${TEMP}/cthead1-shape-relabel-labelmap.mha
  itkShapeRelabelLabelMapFilterTest1
  ${INPUTDATA}/cthead1Label.png
  ${TEMP}/cthead1-shape-relabel-labelmap.mha
  1     # Reverse Ordering (bool) - Objects with values smaller than Lambda are removed  
  0     # Attribute
  )

ADD_TEST(itkShapeLabelObjectAccessorsTest1
  ${REVIEW_TESTS3}
  itkShapeLabelObjectAccessorsTest1
  ${INPUTDATA}/cthead1Label.png
  )

ADD_TEST(itkGridImageSourceTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/itkGridImageSourceTest1.nhdr
            ${TEMP}/itkGridImageSourceTest1.nhdr
  itkGridImageSourceTest ${TEMP}/itkGridImageSourceTest1.nhdr 0)

ADD_TEST(itkGridImageSourceTest2
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/itkGridImageSourceTest2.nhdr
            ${TEMP}/itkGridImageSourceTest2.nhdr
  itkGridImageSourceTest ${TEMP}/itkGridImageSourceTest2.nhdr 1)

ADD_TEST(itkGridImageSourceTest3
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/itkGridImageSourceTest3.nhdr
            ${TEMP}/itkGridImageSourceTest3.nhdr
  itkGridImageSourceTest ${TEMP}/itkGridImageSourceTest3.nhdr 2)

ADD_TEST(itkGridImageSourceTest4
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/itkGridImageSourceTest4.nhdr
  ${TEMP}/itkGridImageSourceTest4.nhdr
  itkGridImageSourceTest ${TEMP}/itkGridImageSourceTest4.nhdr 3)

ADD_TEST(itkGaborImageSourceTest0
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/GaborImageSourceTest0.mha
  ${TEMP}/GaborImageSourceTest0.mha
  itkGaborImageSourceTest ${TEMP}/GaborImageSourceTest0.mha 0)

ADD_TEST(itkGaborImageSourceTest1
  ${REVIEW_TESTS3}
  --compare ${BASELINE}/GaborImageSourceTest1.mha
  ${TEMP}/GaborImageSourceTest1.mha
  itkGaborImageSourceTest ${TEMP}/GaborImageSourceTest1.mha 1)

IF(NOT BORLAND)
ADD_TEST(itkBinaryImageToStatisticsLabelMapFilterTest1
  ${REVIEW_TESTS4}
  --compare ${BASELINE}/Spots-binaryimage-to-statisticslabel.mha
            ${TEMP}/Spots-binaryimage-to-statisticslabel.mha
  itkBinaryImageToStatisticsLabelMapFilterTest1
  ${INPUTDATA}/Spots.png
  ${INPUTDATA}/Spots.png
  ${TEMP}/Spots-binaryimage-to-statisticslabel.mha
  1     # Fully connected (bool)
  0     # Input image foreground
  0     # Output image background
  1     # Compute Feret Diameter (bool)
  1     # Compute Perimeter (bool)
  1     # Compute Histogram (bool)
  128   # Number Of Bins
  )

ADD_TEST(itkLabelImageToStatisticsLabelMapFilterTest1
  ${REVIEW_TESTS4}
  --compare ${INPUTDATA}/Spots.png
            ${TEMP}/Spots-labelimage-to-statisticslabel.png
  itkLabelImageToStatisticsLabelMapFilterTest1
  ${INPUTDATA}/Spots.png
  ${INPUTDATA}/Spots.png
  ${TEMP}/Spots-labelimage-to-statisticslabel.png
  0     # Image background
  1     # Compute Feret Diameter (bool)
  1     # Compute Perimeter (bool)
  1     # Compute Histogram (bool)
  128   # Number Of Bins
  )
ENDIF(NOT BORLAND)

ADD_TEST(itkLabelGeometryImageFilterTest2
  ${REVIEW_TESTS5}
  --compare ${BASELINE}/itkLabelGeometryImageFilterTest2Image.png
            ${TEMP}/itkLabelGeometryImageFilterTest2Image.png
  itkLabelGeometryImageFilterTest
  ${INPUTDATA}/BinaryImageWithVariousShapes01.png
  ${TEMP}/itkLabelGeometryImageFilterTest2Image.png 2 
  ${INPUTDATA}/GrayscaleImageWithVariousShapes01.png
  )

ADD_TEST(itkScalarChanAndVeseLevelSetFunctionTest1 ${REVIEW_TESTS6}
  itkScalarChanAndVeseLevelSetFunctionTest1)
ADD_TEST(itkScalarChanAndVeseLevelSetFunctionTest2 ${REVIEW_TESTS6}
  itkScalarChanAndVeseLevelSetFunctionTest2)
ADD_TEST(itkScalarChanAndVeseDenseLevelSetImageFilterTest1 ${REVIEW_TESTS6}
  itkScalarChanAndVeseDenseLevelSetImageFilterTest1)
ADD_TEST(itkScalarChanAndVeseSparseLevelSetImageFilterTest1 ${REVIEW_TESTS6}
  itkScalarChanAndVeseSparseLevelSetImageFilterTest1)
ADD_TEST(itkScalarChanAndVeseSparseLevelSetImageFilterTest2 ${REVIEW_TESTS6}
  --compare ${BASELINE_ROOT}/Review/itkScalarChanAndVeseSparseLevelSetImageFilterTest2.mha
            ${TEMP}/itkScalarChanAndVeseSparseLevelSetImageFilterTest2.mha
  itkScalarChanAndVeseSparseLevelSetImageFilterTest2
  ${BASELINE_ROOT}/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
  ${TEMP}/itkScalarChanAndVeseSparseLevelSetImageFilterTest2.mha
)
ADD_TEST(itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0 ${REVIEW_TESTS6}
  --compare ${BASELINE_ROOT}/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha
            ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha
  itkScalarChanAndVeseDenseLevelSetImageFilterTest2
  ${BASELINE_ROOT}/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
  ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha
  1 1 0 0 0 
)
ADD_TEST(itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1 ${REVIEW_TESTS6}
  --compare ${BASELINE_ROOT}/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1.mha
            ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1.mha
  itkScalarChanAndVeseDenseLevelSetImageFilterTest2
  ${BASELINE_ROOT}/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
  ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_1.mha
  1 1 1 1 200 
)
ADD_TEST(itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2 ${REVIEW_TESTS6}
  --compare ${BASELINE_ROOT}/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2.mha
            ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2.mha
  itkScalarChanAndVeseDenseLevelSetImageFilterTest2
  ${BASELINE_ROOT}/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
  ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_2.mha
  0 0 1 0 0 
)
ADD_TEST(itkScalarChanAndVeseDenseLevelSetImageFilterTest2_3 ${REVIEW_TESTS6}
  itkScalarChanAndVeseDenseLevelSetImageFilterTest2
  ${BASELINE_ROOT}/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
  ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_3.mha
  0 0 0 0 0 
)

ADD_TEST(itkScalarChanAndVeseDenseLevelSetImageFilterTest3 ${REVIEW_TESTS6}
  itkScalarChanAndVeseDenseLevelSetImageFilterTest3)

ADD_TEST(itkScalarChanAndVeseDenseLevelSetImageFilterTest4 ${REVIEW_TESTS6}
  --compare ${BASELINE_ROOT}/Review/itkScalarChanAndVeseDenseLevelSetImageFilterTest2_0.mha
            ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest4.mha
  itkScalarChanAndVeseDenseLevelSetImageFilterTest4
  ${BASELINE_ROOT}/Segmentation/ShapeDetectionLevelSetLeftVentricleTest.png
  ${ITK_SOURCE_DIR}/Examples/Data/BrainProtonDensitySlice.png
  ${TEMP}/itkScalarChanAndVeseDenseLevelSetImageFilterTest4.mha
  1 1 0 0 0 
)

ADD_TEST(itkMRCImageIOTest ${REVIEW_TESTS5} itkMRCImageIOTest ${TEMP})
ADD_TEST(itkVTKImageIO2Test ${REVIEW_TESTS5} itkVTKImageIO2Test ${TEMP})



#
# Functor colormaps
#
FOREACH( COLORMAP red green blue hot cool spring summer autumn winter copper hot hsv jet overunder )
  ADD_TEST( RGBColormapTest_${COLORMAP} 
     ${REVIEW_TESTS2} 
     --compare ${TEMP}/RGBColormapTest_${COLORMAP}.png ${BASELINE}/RGBColormapTest_${COLORMAP}.png
     itkScalarToRGBColormapImageFilterTest
     ${INPUTDATA}/Colormap_Grey.png ${TEMP}/RGBColormapTest_${COLORMAP}.png ${COLORMAP} )
ENDFOREACH(COLORMAP)


ADD_EXECUTABLE(itkReviewPrintTest itkReviewPrintTest.cxx)
TARGET_LINK_LIBRARIES(itkReviewPrintTest
  ITKAlgorithms
  ITKFEM
  ITKQuadEdgeMesh
  )

ADD_EXECUTABLE(itkReviewHeaderTest itkReviewHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkReviewHeaderTest
  ITKAlgorithms
  ITKFEM
  ITKQuadEdgeMesh
  )

ADD_TEST(itkReviewHeaderTest ${REVIEW_HEADER_TESTS})

ADD_EXECUTABLE(itkReviewTests itkReviewTests.cxx ${ReviewTest_SRCS})

SET(ReviewTest_LIBS
  ITKAlgorithms
  ITKFEM
  ITKStatistics
  ITKQuadEdgeMesh
  ITKIOReview
  ITKIO
  )

ADD_EXECUTABLE(itkReviewTests2 itkReviewTests2.cxx ${ReviewTest2_SRCS})

SET(ReviewTest2_LIBS
  ITKAlgorithms
  ITKFEM
  ITKStatistics
  ITKQuadEdgeMesh
  ITKIOReview
  ITKIO
  )
TARGET_LINK_LIBRARIES(itkReviewTests2 ${ReviewTest2_LIBS})

ADD_EXECUTABLE(itkReviewTests3 itkReviewTests3.cxx ${ReviewTest3_SRCS})

SET(ReviewTest3_LIBS
  ITKIOReview
  ITKIO
  ITKStatistics
  )
TARGET_LINK_LIBRARIES(itkReviewTests3 ${ReviewTest3_LIBS})

IF(NOT BORLAND)
ADD_EXECUTABLE(itkReviewTests4 itkReviewTests4.cxx ${ReviewTest4_SRCS})

SET(ReviewTest4_LIBS
  ITKIO
  ITKStatistics
  )
TARGET_LINK_LIBRARIES(itkReviewTests4 ${ReviewTest4_LIBS})
ENDIF(NOT BORLAND)

ADD_EXECUTABLE(itkReviewTests5 itkReviewTests5.cxx ${ReviewTest5_SRCS})

SET(ReviewTest5_LIBS
  ITKStatistics
  ITKIOReview
  )
TARGET_LINK_LIBRARIES(itkReviewTests5 ${ReviewTest5_LIBS})

ADD_EXECUTABLE(itkReviewTests6 itkReviewTests6.cxx ${ReviewTest6_SRCS})

SET(ReviewTest6_LIBS
  ITKIO
  ITKStatistics
  )
TARGET_LINK_LIBRARIES(itkReviewTests6 ${ReviewTest6_LIBS})

IF(ITK_USE_TRANSFORM_IO_FACTORIES)
SET(ReviewTest_LIBS ${ReviewTest_LIBS}
  ITKTransformIOReview
  )
ENDIF(ITK_USE_TRANSFORM_IO_FACTORIES)


IF(ITK_USE_MINC2 AND MINC2_FOUND)
  SET(ReviewTest_LIBS ${ReviewTest_LIBS} ITKIOMINC2)
ENDIF(ITK_USE_MINC2 AND MINC2_FOUND)


TARGET_LINK_LIBRARIES(itkReviewTests ${ReviewTest_LIBS})

IF (ITK_USE_REVIEW_STATISTICS)
  SUBDIRS( Statistics )
ENDIF (ITK_USE_REVIEW_STATISTICS)

ENDIF(NOT ITK_DISABLE_CXX_TESTING)
