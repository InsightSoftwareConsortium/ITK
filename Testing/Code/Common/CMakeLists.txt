if(ITK_CSWIG_TCL)
  add_test(itkBasicClasses ${ITKWISH_EXECUTABLE}
           ${ITKTesting_SOURCE_DIR}/Code/Common/itkBasicClasses.tcl)
endif(ITK_CSWIG_TCL)

if( NOT ITK_DISABLE_CXX_TESTING )
if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-Common ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/Common)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)


# Define some convenient locations
set(BASELINE ${ITK_DATA_ROOT}/Baseline/Common)
set(TEMP ${ITKTesting_BINARY_DIR}/Temporary)
set(EXAMPLES_DATA_ROOT "${ITK_SOURCE_DIR}/Examples/Data")

if(ITK_USE_FLOATINGPOINTEXCEPTIONS)
  set_source_files_properties(itkFloatingPointExceptionsTest.cxx
    COMPILE_FLAGS -DITK_USE_FPE)
endif(ITK_USE_FLOATINGPOINTEXCEPTIONS)


############################################################
# CXX Testing
#
set(KIT Common)
set(KIT_LIBS ITKIO ITKCommon)
set(KitTests
  itkAdaptorComparisonTest
  itkAddImageAdaptorTest
  itkAffineTransformTest
  itkAnnulusOperatorTest
  itkArray2DTest
  itkArrayTest
  itkAutoPointerTest
  itkAzimuthElevationToCartesianTransformTest
  itkBSplineDeformableTransformTest
  itkBSplineDeformableTransformTest2
  itkBSplineDeformableTransformTest3
  itkBSplineInterpolationWeightFunctionTest
  itkBSplineKernelFunctionTest
  itkBarrierTest
  itkBinaryThresholdImageFunctionTest
  itkBinaryThresholdSpatialFunctionTest
  itkBoundaryConditionTest
  itkBoundingBoxTest
  itkByteSwapTest
  itkCellInterfaceTest
  itkCenteredAffineTransformTest
  itkCenteredEuler3DTransformTest
  itkCenteredRigid2DTransformTest
  itkCenteredTransformInitializerTest
  itkCenteredVersorTransformInitializerTest
  itkCentralDifferenceImageFunctionTest
  itkChainCodePath2DTest
  itkChainCodePathTest
  itkColorTableTest
  itkCommonPrintTest
  itkConditionVariableTest
  itkConstNeighborhoodIteratorTest
  itkConstShapedNeighborhoodIteratorTest
  itkConstShapedNeighborhoodIteratorTest2
  itkCovarianceImageFunctionTest
  itkCovariantVectorGeometryTest
  itkCrossHelperTest
  itkDataObjectTest
  itkDataTypeTest
  itkDecoratorTest
  itkDerivativeOperatorTest
  itkDifferenceImageFilterTest
  itkDiffusionTensor3DTest
  itkDirectoryTest
  itkDynamicMeshTest
  itkDynamicQuadEdgeMeshTest
  itkEllipsoidInteriorExteriorSpatialFunctionTest
  itkEuler2DTransformTest
  itkEuler3DTransformTest
  itkEventObjectTest
  itkExceptionObjectTest
  itkFileOutputWindowTest
  itkFiniteCylinderSpatialFunctionTest
  itkFixedArrayTest
  itkFixedArrayTest2
  itkFixedCenterOfRotationAffineTransformTest
  itkFloatingPointExceptionsTest
  itkFloodFilledSpatialFunctionTest
  itkFourierSeriesPathTest
  itkGaussianBlurImageFunctionTest
  itkGaussianDerivativeImageFunctionTest
  itkGaussianSpatialFunctionTest
  itkGeometricalQuadEdgeTest1
  itkHashTableTest
  itkIdentityTransformTest
  itkImageAdaptorInterpolateImageFunctionTest
  itkImageAdaptorTest
  itkImageComputeOffsetAndIndexTest
  itkImageDuplicatorTest
  itkImageFillBufferTest
  itkImageIteratorTest
  itkImageIteratorWithIndexTest
  itkImageIteratorsForwardBackwardTest
  itkImageLinearIteratorTest
  itkImageRandomIteratorTest
  itkImageRandomIteratorTest2
  itkImageRandomNonRepeatingIteratorWithIndexTest
  itkImageRandomNonRepeatingIteratorWithIndexTest2
  itkImageRegionExclusionIteratorWithIndexTest
  itkImageRegionIteratorTest
  itkImageRegionTest
  itkImageReverseIteratorTest
  itkImageSliceIteratorTest
  itkImageTest
  itkImageToImageFilterTest
  itkImageTransformTest
  itkImportContainerTest
  itkIntTypesTest
  itkIteratorTests
  itkLandmarkBasedTransformInitializerTest
  itkLevelSetFunctionTest
  itkLightObjectTest
  itkLineIteratorTest
  itkLinearInterpolateImageFunctionTest
  itkLoggerManagerTest
  itkLoggerOutputTest
  itkLoggerTest
  itkMahalanobisDistanceThresholdImageFunctionTest
  itkMapContainerTest
  itkMathCastWithRangeCheckTest
  itkMathRoundProfileTest1
  itkMathRoundTest
  itkMathRoundTest2
  itkMatrixTest
  itkMaximumDecisionRuleTest
  itkMaximumRatioDecisionRuleTest
  itkMeanImageFunctionTest
  itkMedialNodeCorrespondencesTest
  itkMedianImageFunctionTest
  itkMemoryLeakTest
  itkMeshFstreamTest
  itkMeshSourceGraftOutputTest
  itkMeshTest
  itkMetaDataDictionaryTest
  itkMinimumDecisionRuleTest
  itkModifiedTimeTest
  itkMultiThreaderTest
  itkMultipleLogOutputTest
  itkNearestNeighborExtrapolateImageFunctionTest
  itkNeighborhoodAlgorithmTest
  itkNeighborhoodIteratorTest
  itkNeighborhoodOperatorImageFunctionTest
  itkNeighborhoodOperatorTest
  itkNeighborhoodTest
  itkNonUniformBSplineTest
  itkNumericTraitsTest
  itkObjectFactoryTest
  itkObjectFactoryTest2
  itkObjectStoreTest
  itkOctreeTest
  itkOrientedImage2DTest
  itkOrientedImage3DTest
  itkOrientedImageAdaptorTest
  itkOrientedImageProfileTest1
  itkOrientedImageProfileTest2
  itkOrientedImageProfileTest3
  itkOrthogonallyCorrected2DParametricPathTest
  itkPCAShapeSignedDistanceFunctionTest
  itkPathFunctionsTest
  itkPathIteratorTest
  itkPeriodicBoundaryConditionTest
  itkPhasedArray3DSpecialCoordinatesImageTest
  itkPixelAccessTest
  itkPointGeometryTest
  itkPointSetTest
  itkPolyLineParametricPathTest
  itkPolygonCellTest
  itkPriorityQueueTest
  itkQuadEdgeMeshAddFaceTest1
  itkQuadEdgeMeshAddFaceTest2
  itkQuadEdgeMeshBasicLayerTest
  itkQuadEdgeMeshCellInterfaceTest
  itkQuadEdgeMeshCountingCellsTest
  itkQuadEdgeMeshDeleteEdgeTest
  itkQuadEdgeMeshDeletePointAndReorderIDsTest
  itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest
  itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest
  itkQuadEdgeMeshEulerOperatorFlipTest
  itkQuadEdgeMeshEulerOperatorJoinFacetTest
  itkQuadEdgeMeshEulerOperatorJoinVertexTest
  itkQuadEdgeMeshEulerOperatorSplitEdgeTest
  itkQuadEdgeMeshEulerOperatorSplitFaceTest
  itkQuadEdgeMeshEulerOperatorSplitVertexTest
  itkQuadEdgeMeshFrontIteratorTest
  itkQuadEdgeMeshIteratorTest
  itkQuadEdgeMeshNoPointConstTest
  itkQuadEdgeMeshPointTest1
  itkQuadEdgeMeshPolygonCellTest
  itkQuadEdgeMeshTest1
  itkQuadEdgeMeshTest2
  itkQuadEdgeMeshTest3
  itkQuadEdgeTest1
  itkQuaternionRigidTransformTest
  itkRGBInterpolateImageFunctionTest
  itkRGBPixelTest
  itkRGBToVectorImageAdaptorTest
  itkRealTimeClockTest
  itkRealTimeIntervalTest
  itkRealTimeStampTest
  itkRigid2DTransformTest
  itkRigid3DPerspectiveTransformTest
  itkRigid3DTransformTest
  itkSTLContainerAdaptorTest
  itkSTLThreadTest
  itkScalarToRGBPixelFunctorTest
  itkScaleLogarithmicTransformTest
  itkScaleSkewVersor3DTransformTest
  itkScaleTransformTest
  itkScaleVersor3DTransformTest
  itkScatterMatrixImageFunctionTest
  itkShapedNeighborhoodIteratorTest
  itkSimilarity2DTransformTest
  itkSimilarity3DTransformTest
  itkSimpleFilterWatcherTest
  itkSimplexMeshTest
  itkSizeTest
  itkSliceIteratorTest
  itkSmartPointerTest
  itkSparseImageTest
  itkSpatialFunctionTest
  itkSphereSignedDistanceFunctionTest
  itkSplineKernelTransformTest
  itkStdStreamLogOutputTest
  itkSymmetricEigenAnalysisTest
  itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest
  itkSymmetricSecondRankTensorTest
  itkThreadDefsTest
  itkThreadLoggerTest
  itkTimeProbesTest
  itkTimeStampTest
  itkTransformFactoryBaseTest
  itkTransformTest
  itkTransformsSetParametersTest
  itkTranslationTransformTest
  itkTreeContainerTest
  itkTreeContainerTest2
  itkTriangleCellTest
  itkTriangleHelperTest
  itkVNLRoundProfileTest1
  itkVariableLengthVectorTest
  itkVariableSizeMatrixTest
  itkVarianceImageFunctionTest
  itkVectorGeometryTest
  itkVectorImageTest
  itkVectorInterpolateImageFunctionTest
  itkVectorMeanImageFunctionTest
  itkVectorTest
  itkVectorToRGBImageAdaptorTest
  itkVersorRigid3DTransformTest
  itkVersorTest
  itkVersorTransformTest
  itkWindowedSincInterpolateImageFunctionTest
  itkZeroFluxBoundaryConditionTest
  itkv3Rigid3DTransformTest
)

# Given KIT, KIT_LIBS and KitTests, build TestDriver
CreateTestDriver(${KIT} "${KIT_LIBS}" "${KitTests}" TestDriver)

#############################################################
# Tests that use the Test Driver
#
add_test(itkAnnulusOperatorTest ${TestDriver} itkAnnulusOperatorTest)
add_test(itkChainCodePath2DTest ${TestDriver} itkChainCodePath2DTest)
add_test(itkChainCodePathTest ${TestDriver} itkChainCodePathTest)
add_test(itkColorTableTest ${TestDriver} itkColorTableTest)
add_test(itkCommonPrintTest ${TestDriver} itkCommonPrintTest)
add_test(itkDecoratorTest ${TestDriver} itkDecoratorTest)
add_test(itkDirectoryTest ${TestDriver} itkDirectoryTest ${TEMP})
add_test(itkDerivativeOperatorTest ${TestDriver} itkDerivativeOperatorTest)
add_test(itkModifiedTimeTest ${TestDriver} itkModifiedTimeTest)
add_test(itkMultipleLogOutputTest ${TestDriver} itkMultipleLogOutputTest ${TEMP}/test_multi.txt)
add_test(itkAdaptorComparisonTest ${TestDriver} itkAdaptorComparisonTest)
add_test(itkAffineTransformTest ${TestDriver} itkAffineTransformTest)
add_test(itkFixedCenterOfRotationAffineTransformTest ${TestDriver} itkFixedCenterOfRotationAffineTransformTest)
add_test(itkFixedArrayTest ${TestDriver} itkFixedArrayTest)
add_test(itkFixedArrayTest2 ${TestDriver} itkFixedArrayTest2)
add_test(itkArrayTest ${TestDriver} itkArrayTest)
add_test(itkArray2DTest ${TestDriver} itkArray2DTest)
add_test(itkAutoPointerTest ${TestDriver} itkAutoPointerTest)
add_test(itkAzimuthElevationToCartesianTransformTest ${TestDriver} itkAzimuthElevationToCartesianTransformTest)
add_test(itkTimeStampTest ${TestDriver} itkTimeStampTest)
add_test(itkBinaryThresholdImageFunctionTest ${TestDriver} itkBinaryThresholdImageFunctionTest)
add_test(itkBinaryThresholdSpatialFunctionTest ${TestDriver} itkBinaryThresholdSpatialFunctionTest)
add_test(itkBoundingBoxTest ${TestDriver} itkBoundingBoxTest)
add_test(itkBSplineDeformableTransformTest ${TestDriver} itkBSplineDeformableTransformTest)
add_test(itkBSplineInterpolationWeightFunctionTest ${TestDriver} itkBSplineInterpolationWeightFunctionTest)
add_test(itkBSplineKernelFunctionTest ${TestDriver} itkBSplineKernelFunctionTest)
add_test(itkBoundaryConditionTest ${TestDriver} itkBoundaryConditionTest)
add_test(itkByteSwapTest ${TestDriver} itkByteSwapTest)
add_test(itkCenteredRigid2DTransformTest ${TestDriver} itkCenteredRigid2DTransformTest ${TEMP}/testCenteredTransform.txt)
add_test(itkCenteredAffineTransformTest ${TestDriver} itkCenteredAffineTransformTest)
add_test(itkCenteredTransformInitializerTest ${TestDriver} itkCenteredTransformInitializerTest)
add_test(itkCenteredVersorTransformInitializerTest ${TestDriver} itkCenteredVersorTransformInitializerTest)
add_test(itkCentralDifferenceImageFunctionTest ${TestDriver} itkCentralDifferenceImageFunctionTest)
add_test(itkCenteredEuler3DTransformTest ${TestDriver} itkCenteredEuler3DTransformTest)
add_test(itkConstNeighborhoodIteratorTest ${TestDriver} itkConstNeighborhoodIteratorTest)
add_test(itkConstShapedNeighborhoodIteratorTest ${TestDriver} itkConstShapedNeighborhoodIteratorTest)
add_test(itkConstShapedNeighborhoodIteratorTest2 ${TestDriver} itkConstShapedNeighborhoodIteratorTest2)
add_test(itkCovariantVectorGeometryTest ${TestDriver} itkCovariantVectorGeometryTest)
add_test(itkCovarianceImageFunctionTest ${TestDriver} itkCovarianceImageFunctionTest)
add_test(itkDataTypeTest ${TestDriver} itkDataTypeTest)
add_test(itkDataObjectTest ${TestDriver} itkDataObjectTest)
add_test(itkDynamicMeshTest ${TestDriver} itkDynamicMeshTest)
add_test(itkDiffusionTensor3DTest ${TestDriver} itkDiffusionTensor3DTest)
add_test(itkEuler2DTransformTest ${TestDriver} itkEuler2DTransformTest ${TEMP}/testEulerTransform.txt)
add_test(itkEuler3DTransformTest ${TestDriver} itkEuler3DTransformTest)
add_test(itkEventObjectTest ${TestDriver} itkEventObjectTest)
add_test(itkExceptionObjectTest ${TestDriver} itkExceptionObjectTest)
add_test(itkFileOutputWindowTest ${TestDriver} itkFileOutputWindowTest)
add_test(itkFiniteCylinderSpatialFunctionTest ${TestDriver} itkFiniteCylinderSpatialFunctionTest)
add_test(itkFloodFilledSpatialFunctionTest ${TestDriver} itkFloodFilledSpatialFunctionTest)
add_test(itkFourierSeriesPathTest ${TestDriver} itkFourierSeriesPathTest)
add_test(itkGaussianBlurImageFunctionTest ${TestDriver} itkGaussianBlurImageFunctionTest)
add_test(itkGaussianDerivativeImageFunctionTest ${TestDriver} itkGaussianDerivativeImageFunctionTest)
add_test(itkGaussianSpatialFunctionTest ${TestDriver} itkGaussianSpatialFunctionTest)
add_test(itkHashTableTest ${TestDriver} itkHashTableTest)
add_test(itkIdentityTransformTest ${TestDriver} itkIdentityTransformTest)
add_test(itkImageAdaptorTest ${TestDriver} itkImageAdaptorTest)
add_test(itkAddImageAdaptorTest ${TestDriver} itkAddImageAdaptorTest)
add_test(itkOrientedImageAdaptorTest ${TestDriver} itkOrientedImageAdaptorTest)
add_test(itkImageAdaptorInterpolateImageFunctionTest ${TestDriver} itkImageAdaptorInterpolateImageFunctionTest)

add_test(itkDynamicQuadEdgeMeshTest ${TestDriver}
  itkDynamicQuadEdgeMeshTest)
add_test(itkQuadEdgeMeshNoPointConstTest ${TestDriver} itkQuadEdgeMeshNoPointConstTest)
add_test(itkQuadEdgeTest1 ${TestDriver} itkQuadEdgeTest1)
add_test(itkGeometricalQuadEdgeTest1 ${TestDriver}
  itkGeometricalQuadEdgeTest1)
add_test(itkQuadEdgeMeshPointTest1 ${TestDriver}
  itkQuadEdgeMeshPointTest1)
add_test(itkPriorityQueueTest ${TestDriver}
  itkPriorityQueueTest)

FOREACH( index 0 1 2 3 4 5 )
  add_test(itkQuadEdgeMeshAddFaceTest1_${index} ${TestDriver}
    itkQuadEdgeMeshAddFaceTest1 ${index} )
ENDFOREACH( index )
add_test(itkQuadEdgeMeshAddFaceTest2 ${TestDriver}
  itkQuadEdgeMeshAddFaceTest2)
add_test(itkQuadEdgeMeshDeleteEdgeTest ${TestDriver}
  itkQuadEdgeMeshDeleteEdgeTest)
add_test(itkQuadEdgeMeshFrontIteratorTest ${TestDriver}
  itkQuadEdgeMeshFrontIteratorTest)
add_test(itkQuadEdgeMeshTest1 ${TestDriver} itkQuadEdgeMeshTest1)
add_test(itkQuadEdgeMeshTest2 ${TestDriver} itkQuadEdgeMeshTest2)
add_test(itkQuadEdgeMeshTest3 ${TestDriver} itkQuadEdgeMeshTest3)
add_test(itkQuadEdgeMeshEulerOperatorFlipTest ${TestDriver}
  itkQuadEdgeMeshEulerOperatorFlipTest)
add_test(itkQuadEdgeMeshEulerOperatorJoinFacetTest ${TestDriver}
  itkQuadEdgeMeshEulerOperatorJoinFacetTest)
FOREACH( arguments 0 1 2 3 4 )
  add_test( itkQuadEdgeMeshEulerOperatorJoinVertexTest${arguments}
    ${TestDriver}
    itkQuadEdgeMeshEulerOperatorJoinVertexTest ${arguments} )
ENDFOREACH( arguments )
add_test(itkQuadEdgeMeshEulerOperatorSplitEdgeTest ${TestDriver}
  itkQuadEdgeMeshEulerOperatorSplitEdgeTest)
add_test(itkQuadEdgeMeshEulerOperatorSplitVertexTest ${TestDriver}
  itkQuadEdgeMeshEulerOperatorSplitVertexTest)
add_test( itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest ${TestDriver}
  itkQuadEdgeMeshEulerOperatorDeleteCenterVertexTest)
add_test( itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest ${TestDriver}
  itkQuadEdgeMeshEulerOperatorCreateCenterVertexTest)
add_test(itkQuadEdgeMeshCellInterfaceTest ${TestDriver}
  itkQuadEdgeMeshCellInterfaceTest)
add_test(itkQuadEdgeMeshIteratorTest ${TestDriver}
  itkQuadEdgeMeshIteratorTest)
add_test(itkQuadEdgeMeshPolygonCellTest ${TestDriver}
  itkQuadEdgeMeshPolygonCellTest)
add_test(itkQuadEdgeMeshCountingCellsTest ${TestDriver}
  itkQuadEdgeMeshCountingCellsTest)
add_test(itkQuadEdgeMeshDeletePointAndReorderIDsTest ${TestDriver}
  itkQuadEdgeMeshDeletePointAndReorderIDsTest)
add_test(itkTriangleHelperTest ${TestDriver}
  itkTriangleHelperTest)
add_test(itkCrossHelperTest ${TestDriver}
  itkCrossHelperTest)



add_test(itkBSplineDeformableTransformTest2 ${TestDriver}
  --compare ${BASELINE}/itkBSplineDeformableTransformTest2PixelCentered.png
            ${TEMP}/itkBSplineDeformableTransformTest2PixelCentered.png
  itkBSplineDeformableTransformTest2
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements5.txt
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${TEMP}/itkBSplineDeformableTransformTest2PixelCentered.png
        ${TEMP}/itkBSplineDeformableTransformTest2DeformationFieldPixelCentered.mhd 1
  )

add_test(itkBSplineDeformableTransformTest3 ${TestDriver}
  --compare ${BASELINE}/itkBSplineDeformableTransformTest3PixelCentered.png
            ${TEMP}/itkBSplineDeformableTransformTest3PixelCentered.png
  itkBSplineDeformableTransformTest2
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements3.txt
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${TEMP}/itkBSplineDeformableTransformTest3PixelCentered.png
        ${TEMP}/itkBSplineDeformableTransformTest3DeformationFieldPixelCentered.mhd 2
  )

add_test(itkBSplineDeformableTransformTest4 ${TestDriver}
  --compare ${BASELINE}/itkBSplineDeformableTransformTest4PixelCentered.png
            ${TEMP}/itkBSplineDeformableTransformTest4PixelCentered.png
  itkBSplineDeformableTransformTest2
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${TEMP}/itkBSplineDeformableTransformTest4PixelCentered.png
        ${TEMP}/itkBSplineDeformableTransformTest4DeformationFieldPixelCentered.mhd 3
  )

add_test(itkBSplineDeformableTransformTest5 ${TestDriver}
  --compare ${BASELINE}/itkBSplineDeformableTransformTest5PixelCentered.png
            ${TEMP}/itkBSplineDeformableTransformTest5PixelCentered.png
  itkBSplineDeformableTransformTest3
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${TEMP}/itkBSplineDeformableTransformTest5PixelCentered.png
        ${TEMP}/itkBSplineDeformableTransformTest5DeformationFieldPixelCentered.mhd 0
  )

add_test(itkBSplineDeformableTransformTest6 ${TestDriver}
  --compare ${BASELINE}/itkBSplineDeformableTransformTest5PixelCentered.png
            ${TEMP}/itkBSplineDeformableTransformTest6PixelCentered.png
  itkBSplineDeformableTransformTest3
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${TEMP}/itkBSplineDeformableTransformTest6PixelCentered.png
        ${TEMP}/itkBSplineDeformableTransformTest6DeformationFieldPixelCentered.mhd 1
  )

add_test(itkBSplineDeformableTransformTest7 ${TestDriver}
  --compare ${BASELINE}/itkBSplineDeformableTransformTest5PixelCentered.png
            ${TEMP}/itkBSplineDeformableTransformTest7PixelCentered.png
  itkBSplineDeformableTransformTest3
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${TEMP}/itkBSplineDeformableTransformTest7PixelCentered.png
        ${TEMP}/itkBSplineDeformableTransformTest7DeformationFieldPixelCentered.mhd 2
  )

add_test(itkCellInterfaceTest ${TestDriver} itkCellInterfaceTest)
add_test(itkImageTransformTest ${TestDriver} itkImageTransformTest)
add_test(itkImageToImageFilterTest ${TestDriver} itkImageToImageFilterTest)
add_test(itkRGBInterpolateImageFunctionTest ${TestDriver} itkRGBInterpolateImageFunctionTest)
add_test(itkImageDuplicatorTest ${TestDriver} itkImageDuplicatorTest)
add_test(itkImageIteratorTest ${TestDriver} itkImageIteratorTest)
add_test(itkImageIteratorsForwardBackwardTest ${TestDriver} itkImageIteratorsForwardBackwardTest)
add_test(itkImageIteratorWithIndexTest ${TestDriver} itkImageIteratorWithIndexTest)
add_test(itkImageRegionIteratorTest ${TestDriver} itkImageRegionIteratorTest)
add_test(itkImageLinearIteratorTest ${TestDriver} itkImageLinearIteratorTest)
add_test(itkImageRandomIteratorTest ${TestDriver} itkImageRandomIteratorTest)
add_test(itkImageRandomIteratorTest2 ${TestDriver} itkImageRandomIteratorTest2
      ${TEMP}/itkImageRandomIteratorTest2Output.mha)
add_test(itkImageRandomNonRepeatingIteratorWithIndexTest ${TestDriver} itkImageRandomNonRepeatingIteratorWithIndexTest)
add_test(itkImageRandomNonRepeatingIteratorWithIndexTest2
  ${TestDriver} itkImageRandomNonRepeatingIteratorWithIndexTest2)
add_test(itkImageRegionTest ${TestDriver} itkImageRegionTest)
add_test(itkImageRegionExclusionIteratorWithIndexTest ${TestDriver} itkImageRegionExclusionIteratorWithIndexTest)
add_test(itkImageReverseIteratorTest ${TestDriver} itkImageReverseIteratorTest)
add_test(itkImageSliceIteratorTest ${TestDriver} itkImageSliceIteratorTest)
add_test(itkImageTest ${TestDriver} itkImageTest)
add_test(itkIteratorTests ${TestDriver} itkIteratorTests)
add_test(itkLandmarkBasedTransformInitializerTest ${TestDriver} itkLandmarkBasedTransformInitializerTest)
add_test(itkLightObjectTest ${TestDriver} itkLightObjectTest)
add_test(itkLinearInterpolateImageFunctionTest ${TestDriver} itkLinearInterpolateImageFunctionTest)
add_test(itkLineIteratorTest ${TestDriver} itkLineIteratorTest ${BASELINE}/itkLineIteratorTest.txt)
add_test(itkLevelSetFunctionTest ${TestDriver} itkLevelSetFunctionTest)
add_test(itkLoggerTest ${TestDriver} itkLoggerTest ${TEMP}/test_logger.txt)
#Needs to be added to SRCS when this works across platforms.  itkLoggerThreadWrapperTest.cxx
#add_test(itkLoggerThreadWrapperTest ${TestDriver} itkLoggerThreadWrapperTest ${TEMP}/test_logger.txt)
add_test(itkLoggerOutputTest ${TestDriver} itkLoggerOutputTest ${TEMP}/test_loggerOutput.txt)
add_test(itkLoggerManagerTest ${TestDriver} itkLoggerManagerTest ${TEMP}/test_LoggerManager.txt)
add_test(itkMatrixTest ${TestDriver} itkMatrixTest)
add_test(itkMapContainerTest ${TestDriver} itkMapContainerTest)
add_test(itkMahalanobisDistanceThresholdImageFunctionTest ${TestDriver} itkMahalanobisDistanceThresholdImageFunctionTest)

add_test(itkMaximumDecisionRuleTest ${TestDriver} itkMaximumDecisionRuleTest)
add_test(itkMaximumRatioDecisionRuleTest ${TestDriver} itkMaximumRatioDecisionRuleTest)
add_test(itkMeanImageFunctionTest ${TestDriver} itkMeanImageFunctionTest)

if(ITK_DATA_ROOT)
  add_test(itkDifferenceImageFilterTest ${TestDriver}
       --compare ${BASELINE}/itkDifferenceImageFilterTest.png
                 ${ITK_TEST_OUTPUT_DIR}/itkDifferenceImageFilterTest.png
     itkDifferenceImageFilterTest
                 ${ITK_DATA_ROOT}/Input/cake_easy.png
                 ${ITK_DATA_ROOT}/Input/cake_hard.png
                 ${ITK_TEST_OUTPUT_DIR}/itkDifferenceImageFilterTest.png 10 1)

    add_test(itkOrientedImage2DTest01 ${TestDriver}
       itkOrientedImage2DTest
                   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd

                       0.000    0.000
                     191.392  110.500
                    -128.500  222.569

                       4.000   -8.000
                       7.4641  -4.9282
                    )

    add_test(itkOrientedImage3DTest01 ${TestDriver}
       itkOrientedImage3DTest
                   ${ITK_DATA_ROOT}/Input/HeadMRVolumeWithDirection.mhd
                      0.000      0.000     0.000
                    166.277    -96.000     0.000
                    124.000    214.774     0.000
                      0.000      0.000   168.000

                      0.125     -4.125     9.000
                     -1.95425   -3.63485   9.000
                   )
endif(ITK_DATA_ROOT)

add_test(itkOrientedImageProfileTest1 ${TestDriver} itkOrientedImageProfileTest1)
add_test(itkOrientedImageProfileTest2 ${TestDriver} itkOrientedImageProfileTest2)
add_test(itkOrientedImageProfileTest3 ${TestDriver} itkOrientedImageProfileTest3)

add_test(itkMathRoundTest ${TestDriver} itkMathRoundTest)
add_test(itkMathRoundTest2 ${TestDriver} itkMathRoundTest2)

add_test(itkVNLRoundProfileTest1 ${TestDriver} itkVNLRoundProfileTest1)
add_test(itkMathRoundProfileTest1 ${TestDriver} itkMathRoundProfileTest1)

add_test(itkMathCastWithRangeCheckTest ${TestDriver} itkMathCastWithRangeCheckTest )

add_test(itkMedianImageFunctionTest ${TestDriver} itkMedianImageFunctionTest)
add_test(itkMedialNodeCorrespondencesTest ${TestDriver} itkMedialNodeCorrespondencesTest)

#
# This test should be enabled if you suspect that the memory leak detector
# is not working
if(0)
  add_test(itkMemoryLeakTest ${TestDriver} itkMemoryLeakTest)
endif(0)

add_test(itkMeshTest ${TestDriver} itkMeshTest)
add_test(itkMeshFstreamTest ${TestDriver} itkMeshFstreamTest ${TEMP}/testMeshFstream.txt)
add_test(itkMeshSourceGraftOutputTest ${TestDriver} itkMeshSourceGraftOutputTest)
add_test(itkMetaDataDictionaryTest ${TestDriver} itkMetaDataDictionaryTest)
add_test(itkMinimumDecisionRuleTest ${TestDriver} itkMinimumDecisionRuleTest)
add_test(itkMultiThreaderTest ${TestDriver} itkMultiThreaderTest)
add_test(itkNearestNeighborExtrapolateImageFunctionTest ${TestDriver} itkNearestNeighborExtrapolateImageFunctionTest)
add_test(itkNeighborhoodAlgorithmTest ${TestDriver} itkNeighborhoodAlgorithmTest)
add_test(itkNeighborhoodTest ${TestDriver} itkNeighborhoodTest)
add_test(itkNeighborhoodIteratorTest ${TestDriver} itkNeighborhoodIteratorTest)
add_test(itkNeighborhoodOperatorImageFunctionTest ${TestDriver} itkNeighborhoodOperatorImageFunctionTest)
add_test(itkNeighborhoodOperatorTest ${TestDriver} itkNeighborhoodOperatorTest)
add_test(itkNumericTraitsTest ${TestDriver} itkNumericTraitsTest)
add_test(itkIntTypesTest ${TestDriver} itkIntTypesTest)
add_test(itkOctreeTest ${TestDriver} itkOctreeTest)
add_test(itkObjectFactoryTest ${TestDriver} itkObjectFactoryTest)
add_test(itkOrthogonallyCorrected2DParametricPathTest ${TestDriver} itkOrthogonallyCorrected2DParametricPathTest)
add_test(itkPathFunctionsTest ${TestDriver} itkPathFunctionsTest)
add_test(itkNonUniformBSplineTest ${TestDriver} itkNonUniformBSplineTest)
add_test(itkPathIteratorTest ${TestDriver} itkPathIteratorTest)
add_test(itkPCAShapeSignedDistanceFunctionTest ${TestDriver} itkPCAShapeSignedDistanceFunctionTest)
add_test(itkPeriodicBoundaryConditionTest ${TestDriver} itkPeriodicBoundaryConditionTest)
add_test(itkPhasedArray3DSpecialCoordinatesImageTest ${TestDriver} itkPhasedArray3DSpecialCoordinatesImageTest)
add_test(itkPixelAccessTest ${TestDriver} itkPixelAccessTest)
add_test(itkPointGeometryTest ${TestDriver} itkPointGeometryTest)
add_test(itkPointSetTest ${TestDriver} itkPointSetTest)
add_test(itkPolyLineParametricPathTest ${TestDriver} itkPolyLineParametricPathTest)
add_test(itkPolygonCellTest ${TestDriver} itkPolygonCellTest)
add_test(itkQuaternionRigidTransformTest ${TestDriver} itkQuaternionRigidTransformTest)
add_test(itkRealTimeClockTest ${TestDriver} itkRealTimeClockTest)
add_test(itkRealTimeStampTest ${TestDriver} itkRealTimeStampTest)
add_test(itkRealTimeIntervalTest ${TestDriver} itkRealTimeIntervalTest)
add_test(itkRGBPixelTest ${TestDriver} itkRGBPixelTest)
add_test(itkRGBToVectorImageAdaptorTest ${TestDriver} itkRGBToVectorImageAdaptorTest)
add_test(itkRigid2DTransformTest ${TestDriver} itkRigid2DTransformTest)
add_test(itkRigid3DTransformTest ${TestDriver} itkRigid3DTransformTest)
add_test(itkv3Rigid3DTransformTest ${TestDriver} itkv3Rigid3DTransformTest)
add_test(itkRigid3DPerspectiveTransformTest ${TestDriver} itkRigid3DPerspectiveTransformTest)
add_test(itkScalarToRGBPixelFunctorTest ${TestDriver} itkScalarToRGBPixelFunctorTest)
add_test(itkScatterMatrixImageFunctionTest ${TestDriver} itkScatterMatrixImageFunctionTest)
add_test(itkScaleSkewVersor3DTransformTest ${TestDriver} itkScaleSkewVersor3DTransformTest)
add_test(itkScaleVersor3DTransformTest ${TestDriver} itkScaleVersor3DTransformTest)
add_test(itkScaleTransformTest ${TestDriver} itkScaleTransformTest)
add_test(itkScaleLogarithmicTransformTest ${TestDriver} itkScaleLogarithmicTransformTest)
add_test(itkShapedNeighborhoodIteratorTest ${TestDriver} itkShapedNeighborhoodIteratorTest)
add_test(itkSimilarity2DTransformTest ${TestDriver} itkSimilarity2DTransformTest)
add_test(itkSimilarity3DTransformTest ${TestDriver} itkSimilarity3DTransformTest)
add_test(itkSimpleFilterWatcherTest ${TestDriver} itkSimpleFilterWatcherTest)
add_test(itkSimplexMeshTest ${TestDriver} itkSimplexMeshTest)
add_test(itkSizeTest ${TestDriver} itkSizeTest)
add_test(itkSliceIteratorTest ${TestDriver} itkSliceIteratorTest)
add_test(itkSmartPointerTest ${TestDriver} itkSmartPointerTest)
add_test(itkSparseImageTest ${TestDriver} itkSparseImageTest)
add_test(itkSpatialFunctionTest ${TestDriver} itkSpatialFunctionTest)
add_test(itkSphereSignedDistanceFunctionTest ${TestDriver} itkSphereSignedDistanceFunctionTest)
add_test(itkSplineKernelTransformTest ${TestDriver} itkSplineKernelTransformTest)
add_test(itkSTLContainerAdaptorTest ${TestDriver} itkSTLContainerAdaptorTest)
add_test(itkStdStreamLogOutputTest ${TestDriver} itkStdStreamLogOutputTest ${TEMP}/testStreamLogOutput.txt)
add_test(itkThreadDefsTest ${TestDriver} itkThreadDefsTest)
add_test(itkThreadLoggerTest ${TestDriver} itkThreadLoggerTest ${TEMP}/test_threadLogger.txt)
add_test(itkTimeProbesTest ${TestDriver} itkTimeProbesTest)
add_test(itkTransformTest ${TestDriver} itkTransformTest)
add_test(itkTransformFactoryBaseTest ${TestDriver} itkTransformFactoryBaseTest)
add_test(itkTransformsSetParametersTest ${TestDriver} itkTransformsSetParametersTest)
add_test(itkTranslationTransformTest ${TestDriver} itkTranslationTransformTest)
add_test(itkTreeContainerTest ${TestDriver} itkTreeContainerTest)
add_test(itkTreeContainerTest2 ${TestDriver} itkTreeContainerTest2)
add_test(itkTriangleCellTest ${TestDriver} itkTriangleCellTest)
add_test(itkVarianceImageFunctionTest ${TestDriver} itkVarianceImageFunctionTest)
add_test(itkVectorGeometryTest ${TestDriver} itkVectorGeometryTest)
add_test(itkVectorImageTest ${TestDriver} itkVectorImageTest ${TEMP}/VectorImage.nrrd ${TEMP}/VectorImage.mhd)
add_test(itkVectorMeanImageFunctionTest ${TestDriver} itkVectorMeanImageFunctionTest)
add_test(itkVersorTransformTest ${TestDriver} itkVersorTransformTest)
add_test(itkVersorTest ${TestDriver} itkVersorTest)
add_test(itkVersorRigid3DTransformTest ${TestDriver} itkVersorRigid3DTransformTest)
add_test(itkVectorTest ${TestDriver} itkVectorTest)
add_test(itkVectorInterpolateImageFunctionTest ${TestDriver} itkVectorInterpolateImageFunctionTest)
add_test(itkVectorToRGBImageAdaptorTest ${TestDriver} itkVectorToRGBImageAdaptorTest)
add_test(itkWindowedSincInterpolateImageFunctionTest ${TestDriver} itkWindowedSincInterpolateImageFunctionTest)
add_test(itkEllipsoidInteriorExteriorSpatialFunctionTest ${TestDriver} itkEllipsoidInteriorExteriorSpatialFunctionTest)
add_test(itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest ${TestDriver} itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest)
add_test(itkSymmetricSecondRankTensorTest ${TestDriver} itkSymmetricSecondRankTensorTest)
add_test(itkSymmetricEigenAnalysisTest ${TestDriver} itkSymmetricEigenAnalysisTest)
add_test(itkSTLThreadTest ${TestDriver} itkSTLThreadTest)
add_test(itkVariableLengthVectorTest ${TestDriver} itkVariableLengthVectorTest)
add_test(itkVariableSizeMatrixTest ${TestDriver} itkVariableSizeMatrixTest)
add_test(itkImportContainerTest ${TestDriver} itkImportContainerTest)
#add_test(itkQuaternionOrientationAdapterTest ${TestDriver} itkQuaternionOrientationAdapterTest)
add_test(itkZeroFluxBoundaryConditionTest ${TestDriver} itkZeroFluxBoundaryConditionTest)

#
# Tests that are run based on a particular cmake setting
#

if(ITK_BUILD_SHARED_LIBS)
  add_test(itkObjectFactoryTest2 ${TestDriver} itkObjectFactoryTest2 ${ITK_TEST_OUTPUT_DIR})
  set_tests_properties(itkObjectFactoryTest2 PROPERTIES RUN_SERIAL 1)
endif(ITK_BUILD_SHARED_LIBS)

if(NOT MINGW)
  add_test(itkBarrierTest ${TestDriver} itkBarrierTest)
  add_test(itkConditionVariableTest ${TestDriver} itkConditionVariableTest)
endif(NOT MINGW)

if( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 4 )
  add_test(itkImageFillBufferTest4.1 ${TestDriver} itkImageFillBufferTest 4.1)
endif( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 4 )

#
# this test depends on being able to raise C++ exceptions from
# the signal handler. This is apparently not something that
# works properly
if(ITK_USE_FLOATINGPOINTEXCEPTIONS)

  add_test(itkFloatingPointExceptionsTest1 ${TestDriver}
    itkFloatingPointExceptionsTest DivByZero)

  add_test(itkFloatingPointExceptionsTest2 ${TestDriver}
    itkFloatingPointExceptionsTest ZeroDivByZero)

  add_test(itkFloatingPointExceptionsTest3 ${TestDriver}
    itkFloatingPointExceptionsTest FPOverFlow)

  add_test(itkFloatingPointExceptionsTest4 ${TestDriver}
    itkFloatingPointExceptionsTest FPUnderFlow)

  add_test(itkFloatingPointExceptionsTest5 ${TestDriver}
    itkFloatingPointExceptionsTest IntDivByZero)

  set_tests_properties(
    itkFloatingPointExceptionsTest1
    itkFloatingPointExceptionsTest2
    itkFloatingPointExceptionsTest3
    itkFloatingPointExceptionsTest4
    itkFloatingPointExceptionsTest5
    PROPERTIES WILL_FAIL TRUE
    )

endif(ITK_USE_FLOATINGPOINTEXCEPTIONS)

if(ITK_BINARY_DIR)
  set(ITK_SYSTEM_INFORMATION_DIR ${ITK_BINARY_DIR})
else(ITK_BINARY_DIR)
  set(ITK_SYSTEM_INFORMATION_DIR ${ITKTesting_BINARY_DIR})
endif(ITK_BINARY_DIR)

write_file(
  "${ITK_SYSTEM_INFORMATION_DIR}/Testing/HTML/TestingResults/Sites/${SITE}/${BUILDNAME}/BuildNameNotes.xml"

  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
  "<Site BuildName=\"CMakeCache\" Name=\"crd\">\n"
  "<BuildNameNotes>\n"
  "<Note>\n"
  "<DateTime>Wed Oct 24 1:00:00 EST</DateTime>\n"
  "<Text>\n"
  "The full itkSystemInformationTest did not run.\n"
  "Here is some basic information:\n"
  "ITK_SOURCE_DIR = ${ITK_SOURCE_DIR}\n"
  "ITK_BINARY_DIR = ${ITK_BINARY_DIR}\n"
  "ITKTesting_SOURCE_DIR = ${ITKTesting_SOURCE_DIR}\n"
  "ITKTesting_BINARY_DIR = ${ITKTesting_BINARY_DIR}\n"
  "CMAKE_C_COMPILER = ${CMAKE_C_COMPILER}\n"
  "CMAKE_C_FLAGS = ${CMAKE_C_FLAGS}\n"
  "CMAKE_CXX_COMPILER = ${CMAKE_CXX_COMPILER}\n"
  "CMAKE_CXX_FLAGS = ${CMAKE_CXX_FLAGS}\n"
  "CMAKE_SYSTEM = ${CMAKE_SYSTEM}\n"
  "CMAKE_MAKE_PROGRAM = ${CMAKE_MAKE_PROGRAM}\n"
  "</Text>\n"
  "</Note>\n"
  "</BuildNameNotes>\n"
  "</Site>\n"
)

###
# will be moved to a common test driver after the file is determined to be portable and the rounding has been fully migrated
add_executable( itkMathTest itkMathTest.cxx )
add_test( itkMathTest  ${CXX_TEST_PATH}/itkMathTest )
###

#-----------------------------------------------------------------------------
# Setup the system information test.  Write out some basic failsafe
# information in case the test doesn't run.

configure_file(${ITKTesting_SOURCE_DIR}/Code/Common/itkSystemInformationTest.h.in
               ${ITKTesting_BINARY_DIR}/Code/Common/itkSystemInformationTest.h
               @ONLY IMMEDIATE)
include_directories("${ITKTesting_BINARY_DIR}")

if(ITK_OBJCXX_COMPILER_WORKS)
  set(COMMON_OBJCXX_TEST ${CXX_TEST_PATH}/itkCommonHeaderObjCxxTest)
  add_test(itkCommonHeaderObjCxxTest ${COMMON_OBJCXX_TEST})
  add_executable(itkCommonHeaderObjCxxTest itkCommonHeaderObjCxxTest.mm)
  target_link_libraries(itkCommonHeaderObjCxxTest ITKCommon)
endif(ITK_OBJCXX_COMPILER_WORKS)

############################################################
# Tests that have their own executable
#
set(COMMON_HEADER_TEST ${CXX_TEST_PATH}/itkCommonHeaderTest)
set(SYSTEM_INFORMATION_TEST ${CXX_TEST_PATH}/itkSystemInformationTest)
set(SYSTEM_INFORMATION ${CXX_TEST_PATH}/itkSystemInformation)

add_test(itkCommonHeaderTest ${COMMON_HEADER_TEST})
add_test(itkSystemInformationTest ${SYSTEM_INFORMATION_TEST})
add_test(SystemInformation ${SYSTEM_INFORMATION})

add_executable(itkSystemInformationTest itkSystemInformationTest.cxx)
add_executable(itkCommonHeaderTest itkCommonHeaderTest.cxx)
target_link_libraries(itkCommonHeaderTest ITKCommon)
add_executable(itkSystemInformation itkSystemInformation.cxx)
target_link_libraries(itkSystemInformation itksys)

if(ITK_BUILD_SHARED_LIBS)
  ## Create a library to test ITK loadable factories
  set(FactoryTest_Source itkFactoryTestLib.cxx)
  set(LIBRARY_OUTPUT_PATH "${ITKTesting_BINARY_DIR}/Temporary")
  add_library(FactoryTestLib SHARED ${FactoryTest_Source})
  target_link_libraries(FactoryTestLib ITKCommon)
  add_dependencies(CommonCxxTests FactoryTestLib)
  endif(ITK_BUILD_SHARED_LIBS)
endif( NOT ITK_DISABLE_CXX_TESTING )
