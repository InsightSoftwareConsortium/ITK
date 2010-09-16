if(ITK_CSWIG_TCL)
  add_test(itkBasicClasses ${ITKWISH_EXECUTABLE}
           ${ITKTesting_SOURCE_DIR}/Code/Common/itkBasicClasses.tcl)
endif(ITK_CSWIG_TCL)
#-----------------------------------------------------------------------------
# Setup the system information test.  Write out some basic failsafe
# information in case the test doesn't run.

if( NOT ITK_DISABLE_CXX_TESTING )
if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-Common ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/Common)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)


set(COMMON_TESTS ${CXX_TEST_PATH}/itkCommonTests)
set(COMMON_TESTS2 ${CXX_TEST_PATH}/itkCommonTests2)
set(COMMON_HEADER_TEST ${CXX_TEST_PATH}/itkCommonHeaderTest)
set(SYSTEM_INFORMATION_TEST ${CXX_TEST_PATH}/itkSystemInformationTest)
set(SYSTEM_INFORMATION ${CXX_TEST_PATH}/itkSystemInformation)
set(BASELINE ${ITK_DATA_ROOT}/Baseline/Common)

set(TEMP ${ITKTesting_BINARY_DIR}/Temporary)
set(EXAMPLES_DATA_ROOT "${ITK_SOURCE_DIR}/Examples/Data")

add_test(itkAnnulusOperatorTest ${COMMON_TESTS} itkAnnulusOperatorTest)
add_test(itkChainCodePath2DTest ${COMMON_TESTS} itkChainCodePath2DTest)
add_test(itkChainCodePathTest ${COMMON_TESTS} itkChainCodePathTest)
add_test(itkColorTableTest ${COMMON_TESTS} itkColorTableTest)
add_test(itkCommonPrintTest ${COMMON_TESTS} itkCommonPrintTest)
add_test(itkDecoratorTest ${COMMON_TESTS} itkDecoratorTest)
add_test(itkDirectoryTest ${COMMON_TESTS} itkDirectoryTest ${TEMP})
add_test(itkModifiedTimeTest ${COMMON_TESTS} itkModifiedTimeTest)
add_test(itkMultipleLogOutputTest ${COMMON_TESTS} itkMultipleLogOutputTest ${TEMP}/test_multi.txt)
add_test(itkAdaptorComparisonTest ${COMMON_TESTS} itkAdaptorComparisonTest)
add_test(itkAffineTransformTest ${COMMON_TESTS} itkAffineTransformTest)
add_test(itkFixedCenterOfRotationAffineTransformTest ${COMMON_TESTS} itkFixedCenterOfRotationAffineTransformTest)
add_test(itkFixedArrayTest ${COMMON_TESTS} itkFixedArrayTest)
add_test(itkFixedArrayTest2 ${COMMON_TESTS} itkFixedArrayTest2)
add_test(itkArrayTest ${COMMON_TESTS} itkArrayTest)
add_test(itkArray2DTest ${COMMON_TESTS} itkArray2DTest)
add_test(itkAutoPointerTest ${COMMON_TESTS} itkAutoPointerTest)
add_test(itkAzimuthElevationToCartesianTransformTest ${COMMON_TESTS} itkAzimuthElevationToCartesianTransformTest)
if(NOT MINGW)
add_test(itkBarrierTest ${COMMON_TESTS} itkBarrierTest)
add_test(itkConditionVariableTest ${COMMON_TESTS} itkConditionVariableTest)
add_test(itkSemaphoreTest ${COMMON_TESTS2} itkSemaphoreTest)
endif(NOT MINGW)
add_test(itkTimeStampTest ${COMMON_TESTS2} itkTimeStampTest)
add_test(itkBinaryThresholdImageFunctionTest ${COMMON_TESTS} itkBinaryThresholdImageFunctionTest)
add_test(itkBinaryThresholdSpatialFunctionTest ${COMMON_TESTS} itkBinaryThresholdSpatialFunctionTest)
add_test(itkBoundingBoxTest ${COMMON_TESTS} itkBoundingBoxTest)
add_test(itkBSplineDeformableTransformTest ${COMMON_TESTS} itkBSplineDeformableTransformTest)
add_test(itkBSplineInterpolationWeightFunctionTest ${COMMON_TESTS} itkBSplineInterpolationWeightFunctionTest)
add_test(itkBSplineKernelFunctionTest ${COMMON_TESTS} itkBSplineKernelFunctionTest)
add_test(itkBoundaryConditionTest ${COMMON_TESTS} itkBoundaryConditionTest)
add_test(itkByteSwapTest ${COMMON_TESTS} itkByteSwapTest)
add_test(itkCenteredRigid2DTransformTest ${COMMON_TESTS} itkCenteredRigid2DTransformTest ${TEMP}/testCenteredTransform.txt)
add_test(itkCenteredAffineTransformTest ${COMMON_TESTS} itkCenteredAffineTransformTest)
add_test(itkCenteredTransformInitializerTest ${COMMON_TESTS} itkCenteredTransformInitializerTest)
add_test(itkCenteredVersorTransformInitializerTest ${COMMON_TESTS} itkCenteredVersorTransformInitializerTest)
add_test(itkCentralDifferenceImageFunctionTest ${COMMON_TESTS} itkCentralDifferenceImageFunctionTest)
add_test(itkCenteredEuler3DTransformTest ${COMMON_TESTS} itkCenteredEuler3DTransformTest)
add_test(itkConstNeighborhoodIteratorTest ${COMMON_TESTS} itkConstNeighborhoodIteratorTest)
add_test(itkConstShapedNeighborhoodIteratorTest ${COMMON_TESTS} itkConstShapedNeighborhoodIteratorTest)
add_test(itkCovariantVectorGeometryTest ${COMMON_TESTS} itkCovariantVectorGeometryTest)
add_test(itkCovarianceImageFunctionTest ${COMMON_TESTS} itkCovarianceImageFunctionTest)
add_test(itkDataTypeTest ${COMMON_TESTS} itkDataTypeTest)
add_test(itkDynamicMeshTest ${COMMON_TESTS} itkDynamicMeshTest)
add_test(itkDiffusionTensor3DTest ${COMMON_TESTS} itkDiffusionTensor3DTest)
add_test(itkEuler2DTransformTest ${COMMON_TESTS} itkEuler2DTransformTest ${TEMP}/testEulerTransform.txt)
add_test(itkEuler3DTransformTest ${COMMON_TESTS} itkEuler3DTransformTest)
add_test(itkEventObjectTest ${COMMON_TESTS} itkEventObjectTest)
add_test(itkExceptionObjectTest ${COMMON_TESTS} itkExceptionObjectTest)
add_test(itkFileOutputWindowTest ${COMMON_TESTS} itkFileOutputWindowTest)
add_test(itkFiniteCylinderSpatialFunctionTest ${COMMON_TESTS} itkFiniteCylinderSpatialFunctionTest)
add_test(itkFloodFilledSpatialFunctionTest ${COMMON_TESTS} itkFloodFilledSpatialFunctionTest)
add_test(itkFourierSeriesPathTest ${COMMON_TESTS} itkFourierSeriesPathTest)
add_test(itkGaussianBlurImageFunctionTest ${COMMON_TESTS} itkGaussianBlurImageFunctionTest)
add_test(itkGaussianDerivativeImageFunctionTest ${COMMON_TESTS} itkGaussianDerivativeImageFunctionTest)
add_test(itkGaussianSpatialFunctionTest ${COMMON_TESTS} itkGaussianSpatialFunctionTest)
add_test(itkCommonHeaderTest ${COMMON_HEADER_TEST})
add_test(itkHashTableTest ${COMMON_TESTS} itkHashTableTest)
add_test(itkIdentityTransformTest ${COMMON_TESTS} itkIdentityTransformTest)
add_test(itkImageAdaptorTest ${COMMON_TESTS} itkImageAdaptorTest)
add_test(itkOrientedImageAdaptorTest ${COMMON_TESTS} itkOrientedImageAdaptorTest)
add_test(itkImageAdaptorInterpolateImageFunctionTest ${COMMON_TESTS} itkImageAdaptorInterpolateImageFunctionTest)

add_test(itkBSplineDeformableTransformTest2 ${COMMON_TESTS2}
  --compare ${BASELINE}/itkBSplineDeformableTransformTest2PixelCentered.png
            ${TEMP}/itkBSplineDeformableTransformTest2PixelCentered.png
  itkBSplineDeformableTransformTest2
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements5.txt
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${TEMP}/itkBSplineDeformableTransformTest2PixelCentered.png
        ${TEMP}/itkBSplineDeformableTransformTest2DeformationFieldPixelCentered.mhd 1
  )

add_test(itkBSplineDeformableTransformTest3 ${COMMON_TESTS2}
  --compare ${BASELINE}/itkBSplineDeformableTransformTest3PixelCentered.png
            ${TEMP}/itkBSplineDeformableTransformTest3PixelCentered.png
  itkBSplineDeformableTransformTest2
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements3.txt
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${TEMP}/itkBSplineDeformableTransformTest3PixelCentered.png
        ${TEMP}/itkBSplineDeformableTransformTest3DeformationFieldPixelCentered.mhd 2
  )

add_test(itkBSplineDeformableTransformTest4 ${COMMON_TESTS2}
  --compare ${BASELINE}/itkBSplineDeformableTransformTest4PixelCentered.png
            ${TEMP}/itkBSplineDeformableTransformTest4PixelCentered.png
  itkBSplineDeformableTransformTest2
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${TEMP}/itkBSplineDeformableTransformTest4PixelCentered.png
        ${TEMP}/itkBSplineDeformableTransformTest4DeformationFieldPixelCentered.mhd 3
  )

add_test(itkBSplineDeformableTransformTest5 ${COMMON_TESTS2}
  --compare ${BASELINE}/itkBSplineDeformableTransformTest5PixelCentered.png
            ${TEMP}/itkBSplineDeformableTransformTest5PixelCentered.png
  itkBSplineDeformableTransformTest3
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${TEMP}/itkBSplineDeformableTransformTest5PixelCentered.png
        ${TEMP}/itkBSplineDeformableTransformTest5DeformationFieldPixelCentered.mhd 0
  )

add_test(itkBSplineDeformableTransformTest6 ${COMMON_TESTS2}
  --compare ${BASELINE}/itkBSplineDeformableTransformTest5PixelCentered.png
            ${TEMP}/itkBSplineDeformableTransformTest6PixelCentered.png
  itkBSplineDeformableTransformTest3
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${TEMP}/itkBSplineDeformableTransformTest6PixelCentered.png
        ${TEMP}/itkBSplineDeformableTransformTest6DeformationFieldPixelCentered.mhd 1
  )

add_test(itkBSplineDeformableTransformTest7 ${COMMON_TESTS2}
  --compare ${BASELINE}/itkBSplineDeformableTransformTest5PixelCentered.png
            ${TEMP}/itkBSplineDeformableTransformTest7PixelCentered.png
  itkBSplineDeformableTransformTest3
        ${ITK_SOURCE_DIR}/Examples/Data/BSplineDisplacements1.txt
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${ITK_SOURCE_DIR}/Examples/Data/DiagonalLines.png
        ${TEMP}/itkBSplineDeformableTransformTest7PixelCentered.png
        ${TEMP}/itkBSplineDeformableTransformTest7DeformationFieldPixelCentered.mhd 2
  )

add_test(itkCellInterfaceTest ${COMMON_TESTS2} itkCellInterfaceTest)
add_test(itkImageTransformTest ${COMMON_TESTS2} itkImageTransformTest)
add_test(itkImageToImageFilterTest ${COMMON_TESTS2} itkImageToImageFilterTest)
add_test(itkRGBInterpolateImageFunctionTest ${COMMON_TESTS2} itkRGBInterpolateImageFunctionTest)
add_test(itkImageDuplicatorTest ${COMMON_TESTS} itkImageDuplicatorTest)
add_test(itkImageIteratorTest ${COMMON_TESTS} itkImageIteratorTest)
add_test(itkImageIteratorsForwardBackwardTest ${COMMON_TESTS} itkImageIteratorsForwardBackwardTest)
add_test(itkImageIteratorWithIndexTest ${COMMON_TESTS} itkImageIteratorWithIndexTest)
add_test(itkImageRegionIteratorTest ${COMMON_TESTS} itkImageRegionIteratorTest)
add_test(itkImageLinearIteratorTest ${COMMON_TESTS} itkImageLinearIteratorTest)
add_test(itkImageRandomIteratorTest ${COMMON_TESTS} itkImageRandomIteratorTest)
add_test(itkImageRandomIteratorTest2 ${COMMON_TESTS} itkImageRandomIteratorTest2
      ${TEMP}/itkImageRandomIteratorTest2Output.mha)
add_test(itkImageRandomNonRepeatingIteratorWithIndexTest ${COMMON_TESTS} itkImageRandomNonRepeatingIteratorWithIndexTest)
add_test(itkImageRegionTest ${COMMON_TESTS} itkImageRegionTest)
add_test(itkImageRegionExclusionIteratorWithIndexTest ${COMMON_TESTS} itkImageRegionExclusionIteratorWithIndexTest)
add_test(itkImageReverseIteratorTest ${COMMON_TESTS} itkImageReverseIteratorTest)
add_test(itkImageSliceIteratorTest ${COMMON_TESTS} itkImageSliceIteratorTest)
add_test(itkImageTest ${COMMON_TESTS} itkImageTest)
add_test(itkIteratorTests ${COMMON_TESTS} itkIteratorTests)
add_test(itkLandmarkBasedTransformInitializerTest ${COMMON_TESTS} itkLandmarkBasedTransformInitializerTest)
add_test(itkLightObjectTest ${COMMON_TESTS} itkLightObjectTest)
add_test(itkLinearInterpolateImageFunctionTest ${COMMON_TESTS2} itkLinearInterpolateImageFunctionTest)
add_test(itkLineIteratorTest ${COMMON_TESTS}
  --compare ${BASELINE}/itkLineIteratorTest.png
            ${TEMP}/itkLineIteratorTest.png
  itkLineIteratorTest
            ${TEMP}/itkLineIteratorTest.png
)
add_test(itkLevelSetFunctionTest ${COMMON_TESTS} itkLevelSetFunctionTest)
add_test(itkLoggerTest ${COMMON_TESTS} itkLoggerTest ${TEMP}/test_logger.txt)
#Needs to be added to SRCS when this works across platforms.  itkLoggerThreadWrapperTest.cxx
#add_test(itkLoggerThreadWrapperTest ${COMMON_TESTS} itkLoggerThreadWrapperTest ${TEMP}/test_logger.txt)
add_test(itkLoggerOutputTest ${COMMON_TESTS} itkLoggerOutputTest ${TEMP}/test_loggerOutput.txt)
add_test(itkLoggerManagerTest ${COMMON_TESTS} itkLoggerManagerTest ${TEMP}/test_LoggerManager.txt)
add_test(itkMatrixTest ${COMMON_TESTS} itkMatrixTest)
add_test(itkMapContainerTest ${COMMON_TESTS} itkMapContainerTest)
add_test(itkMahalanobisDistanceThresholdImageFunctionTest ${COMMON_TESTS} itkMahalanobisDistanceThresholdImageFunctionTest)

add_test(itkMaximumDecisionRuleTest ${COMMON_TESTS2} itkMaximumDecisionRuleTest)
add_test(itkMaximumRatioDecisionRuleTest ${COMMON_TESTS2} itkMaximumRatioDecisionRuleTest)
add_test(itkMeanImageFunctionTest ${COMMON_TESTS2} itkMeanImageFunctionTest)

if(ITK_DATA_ROOT)
  add_test(itkDifferenceImageFilterTest ${COMMON_TESTS2}
       --compare ${BASELINE}/itkDifferenceImageFilterTest.png
                 ${ITK_TEST_OUTPUT_DIR}/itkDifferenceImageFilterTest.png
     itkDifferenceImageFilterTest
                 ${ITK_DATA_ROOT}/Input/cake_easy.png
                 ${ITK_DATA_ROOT}/Input/cake_hard.png
                 ${ITK_TEST_OUTPUT_DIR}/itkDifferenceImageFilterTest.png 10 1)

    add_test(itkOrientedImage2DTest01 ${COMMON_TESTS2}
       itkOrientedImage2DTest
                   ${EXAMPLES_DATA_ROOT}/BrainProtonDensitySliceBorder20DirectionPlus30.mhd

                       0.000    0.000
                     191.392  110.500
                    -128.500  222.569

                       4.000   -8.000
                       7.4641  -4.9282
                    )

    add_test(itkOrientedImage3DTest01 ${COMMON_TESTS2}
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

add_test(itkOrientedImageProfileTest1 ${COMMON_TESTS2} itkOrientedImageProfileTest1)
add_test(itkOrientedImageProfileTest2 ${COMMON_TESTS2} itkOrientedImageProfileTest2)
add_test(itkOrientedImageProfileTest3 ${COMMON_TESTS2} itkOrientedImageProfileTest3)

add_test(itkMathRoundTest ${COMMON_TESTS2} itkMathRoundTest)
add_test(itkMathRoundTest2 ${COMMON_TESTS2} itkMathRoundTest2)

add_test(itkVNLRoundProfileTest1 ${COMMON_TESTS2} itkVNLRoundProfileTest1)
add_test(itkMathRoundProfileTest1 ${COMMON_TESTS2} itkMathRoundProfileTest1)

add_test(itkMathCastWithRangeCheckTest ${COMMON_TESTS2} itkMathCastWithRangeCheckTest )

add_test(itkMedianImageFunctionTest ${COMMON_TESTS2} itkMedianImageFunctionTest)
add_test(itkMedialNodeCorrespondencesTest ${COMMON_TESTS2} itkMedialNodeCorrespondencesTest)

#
# This test should be enabled if you suspect that the memory leak detector
# is not working
if(0)
  add_test(itkMemoryLeakTest ${COMMON_TESTS2} itkMemoryLeakTest)
endif(0)

add_test(itkMeshTest ${COMMON_TESTS2} itkMeshTest)
add_test(itkMeshFstreamTest ${COMMON_TESTS2} itkMeshFstreamTest ${TEMP}/testMeshFstream.txt)
add_test(itkMeshSourceGraftOutputTest ${COMMON_TESTS2} itkMeshSourceGraftOutputTest)
add_test(itkMetaDataDictionaryTest ${COMMON_TESTS2} itkMetaDataDictionaryTest)
add_test(itkMinimumDecisionRuleTest ${COMMON_TESTS2} itkMinimumDecisionRuleTest)
add_test(itkMultiThreaderTest ${COMMON_TESTS2} itkMultiThreaderTest)
add_test(itkNearestNeighborExtrapolateImageFunctionTest ${COMMON_TESTS2} itkNearestNeighborExtrapolateImageFunctionTest)
add_test(itkNeighborhoodTest ${COMMON_TESTS2} itkNeighborhoodTest)
add_test(itkNeighborhoodIteratorTest ${COMMON_TESTS2} itkNeighborhoodIteratorTest)
add_test(itkNeighborhoodOperatorImageFunctionTest ${COMMON_TESTS2} itkNeighborhoodOperatorImageFunctionTest)
add_test(itkNeighborhoodOperatorTest ${COMMON_TESTS2} itkNeighborhoodOperatorTest)
add_test(itkNumericTraitsTest ${COMMON_TESTS2} itkNumericTraitsTest)
add_test(itkIntTypesTest ${COMMON_TESTS2} itkIntTypesTest)
add_test(itkOctreeTest ${COMMON_TESTS2} itkOctreeTest)
add_test(itkObjectFactoryTest ${COMMON_TESTS2} itkObjectFactoryTest)
if(ITK_BUILD_SHARED_LIBS)
add_test(itkObjectFactoryTest2 ${COMMON_TESTS2} itkObjectFactoryTest2 ${ITK_TEST_OUTPUT_DIR})
set_tests_properties(itkObjectFactoryTest2 PROPERTIES RUN_SERIAL 1)

endif(ITK_BUILD_SHARED_LIBS)
add_test(itkOrthogonallyCorrected2DParametricPathTest ${COMMON_TESTS2} itkOrthogonallyCorrected2DParametricPathTest)
add_test(itkPathFunctionsTest ${COMMON_TESTS2} itkPathFunctionsTest)
add_test(itkNonUniformBSplineTest ${COMMON_TESTS2} itkNonUniformBSplineTest)
add_test(itkPathIteratorTest ${COMMON_TESTS2} itkPathIteratorTest)
add_test(itkPCAShapeSignedDistanceFunctionTest ${COMMON_TESTS2} itkPCAShapeSignedDistanceFunctionTest)
add_test(itkPeriodicBoundaryConditionTest ${COMMON_TESTS2} itkPeriodicBoundaryConditionTest)
add_test(itkPhasedArray3DSpecialCoordinatesImageTest ${COMMON_TESTS2} itkPhasedArray3DSpecialCoordinatesImageTest)
add_test(itkPixelAccessTest ${COMMON_TESTS2} itkPixelAccessTest)
add_test(itkPointGeometryTest ${COMMON_TESTS2} itkPointGeometryTest)
add_test(itkPointSetTest ${COMMON_TESTS2} itkPointSetTest)
add_test(itkPolyLineParametricPathTest ${COMMON_TESTS2} itkPolyLineParametricPathTest)
add_test(itkPolygonCellTest ${COMMON_TESTS2} itkPolygonCellTest)
add_test(itkQuaternionRigidTransformTest ${COMMON_TESTS2} itkQuaternionRigidTransformTest)
add_test(itkRealTimeClockTest ${COMMON_TESTS2} itkRealTimeClockTest)
add_test(itkRGBPixelTest ${COMMON_TESTS2} itkRGBPixelTest)
add_test(itkRGBInterpolateImageFunctionTest ${COMMON_TESTS2} itkRGBInterpolateImageFunctionTest)
add_test(itkRGBToVectorImageAdaptorTest ${COMMON_TESTS2} itkRGBToVectorImageAdaptorTest)
add_test(itkRigid2DTransformTest ${COMMON_TESTS2} itkRigid2DTransformTest)
add_test(itkRigid3DTransformTest ${COMMON_TESTS2} itkRigid3DTransformTest)
add_test(itkRigid3DPerspectiveTransformTest ${COMMON_TESTS2} itkRigid3DPerspectiveTransformTest)
add_test(itkScalarToRGBPixelFunctorTest ${COMMON_TESTS2} itkScalarToRGBPixelFunctorTest)
add_test(itkScatterMatrixImageFunctionTest ${COMMON_TESTS2} itkScatterMatrixImageFunctionTest)
add_test(itkScaleSkewVersor3DTransformTest ${COMMON_TESTS2} itkScaleSkewVersor3DTransformTest)
add_test(itkScaleVersor3DTransformTest ${COMMON_TESTS2} itkScaleVersor3DTransformTest)
add_test(itkScaleTransformTest ${COMMON_TESTS2} itkScaleTransformTest)
add_test(itkScaleLogarithmicTransformTest ${COMMON_TESTS2} itkScaleLogarithmicTransformTest)
add_test(itkShapedNeighborhoodIteratorTest ${COMMON_TESTS2} itkShapedNeighborhoodIteratorTest)
add_test(itkSimilarity2DTransformTest ${COMMON_TESTS2} itkSimilarity2DTransformTest)
add_test(itkSimilarity3DTransformTest ${COMMON_TESTS2} itkSimilarity3DTransformTest)
add_test(itkSimpleFilterWatcherTest ${COMMON_TESTS2} itkSimpleFilterWatcherTest)
add_test(itkSimplexMeshTest ${COMMON_TESTS2} itkSimplexMeshTest)
add_test(itkSizeTest ${COMMON_TESTS2} itkSizeTest)
add_test(itkSliceIteratorTest ${COMMON_TESTS2} itkSliceIteratorTest)
add_test(itkSmartPointerTest ${COMMON_TESTS2} itkSmartPointerTest)
add_test(itkSparseImageTest ${COMMON_TESTS2} itkSparseImageTest)
add_test(itkSpatialFunctionTest ${COMMON_TESTS2} itkSpatialFunctionTest)
add_test(itkSphereSignedDistanceFunctionTest ${COMMON_TESTS2} itkSphereSignedDistanceFunctionTest)
add_test(itkSplineKernelTransformTest ${COMMON_TESTS2} itkSplineKernelTransformTest)
add_test(itkSTLContainerAdaptorTest ${COMMON_TESTS2} itkSTLContainerAdaptorTest)
add_test(itkStdStreamLogOutputTest ${COMMON_TESTS2} itkStdStreamLogOutputTest ${TEMP}/testStreamLogOutput.txt)
add_test(itkThreadDefsTest ${COMMON_TESTS2} itkThreadDefsTest)
add_test(itkThreadLoggerTest ${COMMON_TESTS2} itkThreadLoggerTest ${TEMP}/test_threadLogger.txt)
add_test(itkTimeProbesTest ${COMMON_TESTS2} itkTimeProbesTest)
add_test(itkTransformTest ${COMMON_TESTS2} itkTransformTest)
add_test(itkTransformFactoryBaseTest ${COMMON_TESTS2} itkTransformFactoryBaseTest)
add_test(itkTransformsSetParametersTest ${COMMON_TESTS2} itkTransformsSetParametersTest)
add_test(itkTranslationTransformTest ${COMMON_TESTS2} itkTranslationTransformTest)
add_test(itkTreeContainerTest ${COMMON_TESTS2} itkTreeContainerTest)
add_test(itkTreeContainerTest2 ${COMMON_TESTS2} itkTreeContainerTest2)
add_test(itkTriangleCellTest ${COMMON_TESTS2} itkTriangleCellTest)
add_test(itkVarianceImageFunctionTest ${COMMON_TESTS2} itkVarianceImageFunctionTest)
add_test(itkVectorGeometryTest ${COMMON_TESTS2} itkVectorGeometryTest)
add_test(itkVectorImageTest ${COMMON_TESTS2} itkVectorImageTest ${TEMP}/VectorImage.nrrd ${TEMP}/VectorImage.mhd)
add_test(itkVectorMeanImageFunctionTest ${COMMON_TESTS2} itkVectorMeanImageFunctionTest)
add_test(itkVersorTransformTest ${COMMON_TESTS2} itkVersorTransformTest)
add_test(itkVersorTest ${COMMON_TESTS2} itkVersorTest)
add_test(itkVersorRigid3DTransformTest ${COMMON_TESTS2} itkVersorRigid3DTransformTest)
add_test(itkVectorTest ${COMMON_TESTS2} itkVectorTest)
add_test(itkVectorInterpolateImageFunctionTest ${COMMON_TESTS2} itkVectorInterpolateImageFunctionTest)
add_test(itkVectorToRGBImageAdaptorTest ${COMMON_TESTS2} itkVectorToRGBImageAdaptorTest)
add_test(itkWindowedSincInterpolateImageFunctionTest ${COMMON_TESTS2} itkWindowedSincInterpolateImageFunctionTest)
add_test(itkEllipsoidInteriorExteriorSpatialFunctionTest ${COMMON_TESTS2} itkEllipsoidInteriorExteriorSpatialFunctionTest)
add_test(itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest ${COMMON_TESTS2} itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest)
add_test(itkSymmetricSecondRankTensorTest ${COMMON_TESTS2} itkSymmetricSecondRankTensorTest)
add_test(itkSymmetricEigenAnalysisTest ${COMMON_TESTS2} itkSymmetricEigenAnalysisTest)
add_test(itkSTLThreadTest ${COMMON_TESTS2} itkSTLThreadTest)
add_test(itkVariableLengthVectorTest ${COMMON_TESTS2} itkVariableLengthVectorTest)
add_test(itkVariableSizeMatrixTest ${COMMON_TESTS2} itkVariableSizeMatrixTest)
add_test(itkImportContainerTest ${COMMON_TESTS2} itkImportContainerTest)
#add_test(itkQuaternionOrientationAdapterTest ${COMMON_TESTS2} itkQuaternionOrientationAdapterTest)
add_test(itkZeroFluxBoundaryConditionTest ${COMMON_TESTS2} itkZeroFluxBoundaryConditionTest)
add_test(itkSystemInformationTest ${SYSTEM_INFORMATION_TEST})
add_test(SystemInformation ${SYSTEM_INFORMATION})
if( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 4 )
  add_test(itkImageFillBufferTest4.1 ${COMMON_TESTS2} itkImageFillBufferTest 4.1)
endif( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 4 )

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
#-----------------------------------------------------------------------------
set(BasicTests_SRCS
itkImageComputeOffsetAndIndexTest.cxx
itkCommonPrintTest.cxx
itkConditionVariableTest.cxx
itkModifiedTimeTest.cxx
itkMultipleLogOutputTest.cxx
itkAdaptorComparisonTest.cxx
itkAffineTransformTest.cxx
itkAnnulusOperatorTest.cxx
itkArrayTest.cxx
itkArray2DTest.cxx
itkAutoPointerTest.cxx
itkAzimuthElevationToCartesianTransformTest.cxx
itkBarrierTest.cxx
itkBinaryThresholdImageFunctionTest.cxx
itkBinaryThresholdSpatialFunctionTest.cxx
itkBoundingBoxTest.cxx
itkBSplineDeformableTransformTest.cxx
itkBSplineInterpolationWeightFunctionTest.cxx
itkBSplineKernelFunctionTest.cxx
itkBoundaryConditionTest.cxx
itkByteSwapTest.cxx
itkCenteredRigid2DTransformTest.cxx
itkCenteredAffineTransformTest.cxx
itkCentralDifferenceImageFunctionTest.cxx
itkCenteredEuler3DTransformTest.cxx
itkCenteredTransformInitializerTest.cxx
itkCenteredVersorTransformInitializerTest.cxx
itkChainCodePath2DTest.cxx
itkChainCodePathTest.cxx
itkColorTableTest.cxx
itkConstNeighborhoodIteratorTest.cxx
itkConstShapedNeighborhoodIteratorTest.cxx
itkCovariantVectorGeometryTest.cxx
itkCovarianceImageFunctionTest.cxx
itkDataTypeTest.cxx
itkDecoratorTest.cxx
itkDirectoryTest.cxx
itkDynamicMeshTest.cxx
itkDiffusionTensor3DTest.cxx
itkEuler2DTransformTest.cxx
itkEuler3DTransformTest.cxx
itkEventObjectTest.cxx
itkExceptionObjectTest.cxx
itkFileOutputWindowTest.cxx
itkFiniteCylinderSpatialFunctionTest.cxx
itkFixedArrayTest.cxx
itkFixedArrayTest2.cxx
itkFixedCenterOfRotationAffineTransformTest.cxx
itkFloodFilledSpatialFunctionTest.cxx
itkFourierSeriesPathTest.cxx
itkGaussianBlurImageFunctionTest.cxx
itkGaussianDerivativeImageFunctionTest.cxx
itkGaussianSpatialFunctionTest.cxx
itkHashTableTest.cxx
itkIdentityTransformTest.cxx
itkImageAdaptorTest.cxx
itkOrientedImageAdaptorTest.cxx
itkImageAdaptorInterpolateImageFunctionTest.cxx
itkImageDuplicatorTest.cxx
itkImageIteratorTest.cxx
itkImageIteratorsForwardBackwardTest.cxx
itkImageIteratorWithIndexTest.cxx
itkImageRegionIteratorTest.cxx
itkImageLinearIteratorTest.cxx
itkImageRandomIteratorTest.cxx
itkImageRandomIteratorTest2.cxx
itkImageRandomNonRepeatingIteratorWithIndexTest.cxx
itkImageRegionTest.cxx
itkImageRegionExclusionIteratorWithIndexTest.cxx
itkImageReverseIteratorTest.cxx
itkImageSliceIteratorTest.cxx
itkImageTest.cxx
itkIteratorTests.cxx
itkLandmarkBasedTransformInitializerTest.cxx
itkLevelSetFunctionTest.cxx
itkLightObjectTest.cxx
itkLineIteratorTest.cxx
itkLoggerTest.cxx
itkLoggerOutputTest.cxx
itkLoggerManagerTest.cxx
itkMatrixTest.cxx
itkMapContainerTest.cxx
itkMahalanobisDistanceThresholdImageFunctionTest.cxx
)


#-----------------------------------------------------------------------------
set(BasicTests2_SRCS
itkBSplineDeformableTransformTest2.cxx
itkBSplineDeformableTransformTest3.cxx
itkCellInterfaceTest.cxx
itkImageTransformTest.cxx
itkImageToImageFilterTest.cxx
itkLinearInterpolateImageFunctionTest.cxx
itkMaximumDecisionRuleTest.cxx
itkMaximumRatioDecisionRuleTest.cxx
itkDifferenceImageFilterTest.cxx
itkMeanImageFunctionTest.cxx
itkMedialNodeCorrespondencesTest.cxx
itkMedianImageFunctionTest.cxx
itkMemoryLeakTest.cxx
itkMeshTest.cxx
itkMeshFstreamTest.cxx
itkMeshSourceGraftOutputTest.cxx
itkMetaDataDictionaryTest.cxx
itkMinimumDecisionRuleTest.cxx
itkMultiThreaderTest.cxx
itkNearestNeighborExtrapolateImageFunctionTest.cxx
itkNeighborhoodTest.cxx
itkNeighborhoodIteratorTest.cxx
itkNeighborhoodOperatorImageFunctionTest.cxx
itkNeighborhoodOperatorTest.cxx
itkNumericTraitsTest.cxx
itkIntTypesTest.cxx
itkObjectFactoryTest.cxx
itkObjectFactoryTest2.cxx
itkObjectStoreTest.cxx
itkOctreeTest.cxx
itkOrthogonallyCorrected2DParametricPathTest.cxx
itkOrientedImage2DTest.cxx
itkOrientedImage3DTest.cxx
itkOrientedImageProfileTest1.cxx
itkOrientedImageProfileTest2.cxx
itkOrientedImageProfileTest3.cxx
itkVNLRoundProfileTest1.cxx
itkMathRoundTest.cxx
itkMathRoundTest2.cxx
itkMathRoundProfileTest1.cxx
itkMathCastWithRangeCheck.cxx
itkNonUniformBSplineTest.cxx
itkPathFunctionsTest.cxx
itkPathIteratorTest.cxx
itkPCAShapeSignedDistanceFunctionTest.cxx
itkPeriodicBoundaryConditionTest.cxx
itkPhasedArray3DSpecialCoordinatesImageTest.cxx
itkPixelAccessTest.cxx
itkPointGeometryTest.cxx
itkPointSetTest.cxx
itkPolyLineParametricPathTest.cxx
itkPolygonCellTest.cxx
itkQuaternionRigidTransformTest.cxx
itkRealTimeClockTest.cxx
itkRGBPixelTest.cxx
itkRGBInterpolateImageFunctionTest.cxx
itkRGBToVectorImageAdaptorTest.cxx
itkRigid2DTransformTest.cxx
itkRigid3DTransformTest.cxx
itkRigid3DPerspectiveTransformTest.cxx
itkScalarToRGBPixelFunctorTest.cxx
itkScatterMatrixImageFunctionTest.cxx
itkScaleSkewVersor3DTransformTest.cxx
itkScaleVersor3DTransformTest.cxx
itkScaleTransformTest.cxx
itkScaleLogarithmicTransformTest.cxx
itkSemaphoreTest.cxx
itkShapedNeighborhoodIteratorTest.cxx
itkSimilarity2DTransformTest.cxx
itkSimilarity3DTransformTest.cxx
itkSimpleFilterWatcherTest.cxx
itkSimplexMeshTest.cxx
itkSizeTest.cxx
itkSliceIteratorTest.cxx
itkSmartPointerTest.cxx
itkSparseImageTest.cxx
itkSpatialFunctionTest.cxx
itkSphereSignedDistanceFunctionTest.cxx
itkSplineKernelTransformTest.cxx
itkSTLContainerAdaptorTest.cxx
itkStdStreamLogOutputTest.cxx
itkThreadDefsTest.cxx
itkThreadLoggerTest.cxx
itkTimeProbesTest.cxx
itkTimeStampTest.cxx
itkTransformTest.cxx
itkTransformFactoryBaseTest.cxx
itkTransformsSetParametersTest.cxx
itkTranslationTransformTest.cxx
itkTreeContainerTest.cxx
itkTreeContainerTest2.cxx
itkTriangleCellTest.cxx
itkVarianceImageFunctionTest.cxx
itkVectorGeometryTest.cxx
itkVectorImageTest.cxx
itkVectorMeanImageFunctionTest.cxx
itkVersorTransformTest.cxx
itkVersorTest.cxx
itkVersorRigid3DTransformTest.cxx
itkVectorTest.cxx
itkVectorInterpolateImageFunctionTest.cxx
itkVectorToRGBImageAdaptorTest.cxx
itkWindowedSincInterpolateImageFunctionTest.cxx
itkEllipsoidInteriorExteriorSpatialFunctionTest.cxx
itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest.cxx
itkSymmetricSecondRankTensorTest.cxx
itkSymmetricEigenAnalysisTest.cxx
itkSTLThreadTest.cxx
itkVariableLengthVectorTest.cxx
itkVariableSizeMatrixTest.cxx
itkImportContainerTest.cxx
itkZeroFluxBoundaryConditionTest.cxx
itkImageFillBufferTest.cxx
)
# itkQuaternionOrientationAdapterTest.cxx

###
# will be moved to a common test driver after the file is determined to be portable and the rounding has been fully migrated
add_executable( itkMathTest itkMathTest.cxx )
add_test( itkMathTest  ${CXX_TEST_PATH}/itkMathTest )
###

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

add_executable(itkSystemInformationTest itkSystemInformationTest.cxx)
add_executable(itkSystemInformation itkSystemInformation.cxx)
add_executable(itkCommonHeaderTest itkCommonHeaderTest.cxx)
add_executable(itkCommonTests itkCommonTests.cxx ${BasicTests_SRCS})
add_executable(itkCommonTests2 itkCommonTests2.cxx ${BasicTests2_SRCS})
target_link_libraries(itkCommonHeaderTest ITKCommon)
target_link_libraries(itkCommonTests ITKIO)
target_link_libraries(itkCommonTests2 ITKIO)
set_target_properties(itkCommonTests2 PROPERTIES ENABLE_EXPORTS 1)
target_link_libraries(itkSystemInformation itksys)

if(ITK_BUILD_SHARED_LIBS)
## Create a library to test ITK loadable factories
set(FactoryTest_Source itkFactoryTestLib.cxx)
set(LIBRARY_OUTPUT_PATH "${ITKTesting_BINARY_DIR}/Temporary")
add_library(FactoryTestLib SHARED ${FactoryTest_Source})
target_link_libraries(FactoryTestLib ITKCommon)
add_dependencies(itkCommonTests2 FactoryTestLib)
endif(ITK_BUILD_SHARED_LIBS)
endif( NOT ITK_DISABLE_CXX_TESTING )

