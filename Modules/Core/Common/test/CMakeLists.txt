itk_module_test()
set(ITK-Common1Tests
itkAdaptorComparisonTest.cxx
itkCellInterfaceTest.cxx
itkCovariantVectorGeometryTest.cxx
itkDataTypeTest.cxx
itkDecoratorTest.cxx
itkExtractImage3Dto2DTest.cxx
itkExtractImageTest.cxx
itkFilterDispatchTest.cxx
itkFloodFilledSpatialFunctionTest.cxx
itkFloodFillIteratorTest.cxx
itkMapContainerTest.cxx
itkIteratorTests.cxx
itkImageReverseIteratorTest.cxx
itkImageComputeOffsetAndIndexTest.cxx
itkImageDuplicatorTest.cxx
itkImageIteratorsForwardBackwardTest.cxx
itkImageLinearIteratorTest.cxx
itkImageAdaptorPipeLineTest.cxx
itkImportContainerTest.cxx
itkImportImageTest.cxx
itkImageRandomIteratorTest.cxx
itkImageRandomIteratorTest2.cxx
itkImageRandomNonRepeatingIteratorWithIndexTest.cxx
itkImageRandomNonRepeatingIteratorWithIndexTest2.cxx
itkImageSliceIteratorTest.cxx
itkTreeContainerTest2.cxx
itkTriangleCellTest.cxx
itkRGBPixelTest.cxx
itkLightObjectTest.cxx
itkBoundingBoxTest.cxx
itkBoundaryConditionTest.cxx
itkByteSwapTest.cxx
itkScalarToRGBPixelFunctorTest.cxx
itkSparseImageTest.cxx
itkSimpleFilterWatcherTest.cxx
itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest.cxx
itkSymmetricSecondRankTensorImageReadTest.cxx
itkSymmetricSecondRankTensorImageWriteReadTest.cxx
itkArray2DTest.cxx
itkFloatingPointExceptionsTest.cxx
itkFixedArrayTest2.cxx
itkNeighborhoodAlgorithmTest.cxx
itkPhasedArray3DSpecialCoordinatesImageTest.cxx
itkPriorityQueueTest.cxx
itkFileOutputWindowTest.cxx
itkSymmetricEigenAnalysisTest.cxx
itkSTLThreadTest.cxx
itkStreamingImageFilterTest.cxx
itkStreamingImageFilterTest2.cxx
itkStreamingImageFilterTest3.cxx
itkLoggerTest.cxx
itkDerivativeOperatorTest.cxx
itkColorTableTest.cxx
itkNumericTraitsTest.cxx
itkImageRegionTest.cxx
itkExceptionObjectTest.cxx
itkNeighborhoodOperatorTest.cxx
itkNewTest.cxx
itkNumericsTest.cxx
itkAnnulusOperatorTest.cxx
itkLineIteratorTest.cxx
itkGaussianSpatialFunctionTest.cxx
itkRealTimeClockTest.cxx
itkRealTimeIntervalTest
itkRealTimeStampTest
itkIntTypesTest.cxx
itkBSplineKernelFunctionTest.cxx
itkArrayTest.cxx
itkImageIteratorTest.cxx
itkImageRegionIteratorTest.cxx
itkCrossHelperTest.cxx
itkImageIteratorWithIndexTest.cxx
itkDirectoryTest.cxx
itkObjectStoreTest.cxx
itkObjectFactoryTest.cxx
itkEventObjectTest.cxx
itkMathCastWithRangeCheckTest.cxx
itkMathRoundProfileTest1.cxx
itkMathRoundTest.cxx
itkMathRoundTest2.cxx
itkModifiedTimeTest.cxx
itkMultipleLogOutputTest.cxx
itkVectorTest.cxx
itkImageTest.cxx
itkConditionVariableTest.cxx
itkCommonHeaderTest.cxx
itkPointSetTest.cxx
itkBresenhamLineTest.cxx
itkSparseFieldLayerTest.cxx
itkDataObjectTest.cxx
itkAtanRegularizedHeavisideStepFunctionTest1.cxx
itkHashTableTest.cxx
itkHeavisideStepFunctionTest1.cxx
itkSinRegularizedHeavisideStepFunctionTest1.cxx
itkPixelAccessTest.cxx
itkPointGeometryTest.cxx
)



set(ITK-Common2Tests
itkMetaDataDictionaryTest.cxx
itkStdStreamLogOutputTest.cxx
itkOctreeTest.cxx
itkThreadLoggerTest.cxx
itkThreadDefsTest.cxx
itkTimeProbesTest.cxx
itkTreeContainerTest.cxx
itkVariableLengthVectorTest.cxx
itkSpatialFunctionTest.cxx
itkPeriodicBoundaryConditionTest.cxx
itkSmartPointerTest.cxx
itkBarrierTest.cxx
itkVariableSizeMatrixTest.cxx
itkEllipsoidInteriorExteriorSpatialFunctionTest.cxx
itkTimeStampTest.cxx
itkConstNeighborhoodIteratorTest.cxx
itkShapedNeighborhoodIteratorTest.cxx
itkSizeTest.cxx
itkMatrixTest.cxx
itkAutoPointerTest.cxx
itkNeighborhoodIteratorTest.cxx
itkLoggerManagerTest.cxx
itkBSplineInterpolationWeightFunctionTest.cxx
itkSymmetricSecondRankTensorTest.cxx
itkConstShapedNeighborhoodIteratorTest.cxx
itkConstShapedNeighborhoodIteratorTest2.cxx
itkSTLContainerAdaptorTest.cxx
itkFiniteCylinderSpatialFunctionTest.cxx
itkLoggerOutputTest.cxx
itkNeighborhoodTest.cxx
itkVersorTest.cxx
itkObjectFactoryTest2.cxx
itkObjectFactoryTest3.cxx
itkMinimumMaximumImageCalculatorTest.cxx
itkSliceIteratorTest.cxx
itkMultiThreaderTest.cxx
itkImageRegionExclusionIteratorWithIndexTest.cxx
itkFixedArrayTest.cxx
itkImageTransformTest.cxx
itkImageFillBufferTest.cxx
itkMemoryLeakTest.cxx
itkVectorGeometryTest.cxx
itkVNLRoundProfileTest1.cxx
itkZeroFluxBoundaryConditionTest.cxx
itkMemoryProbesCollecterBaseTest.cxx
)

CreateTestDriver(ITK-Common1 "${ITK-Common_LIBRARIES}" "${ITK-Common1Tests}")
CreateTestDriver(ITK-Common2 "${ITK-Common_LIBRARIES}" "${ITK-Common2Tests}")

set(BASELINE "${ITK_DATA_ROOT}/Baseline/Common")
set(TEMP ${ITK_TEST_OUTPUT_DIR})


add_executable(itkMathTest itkMathTest.cxx )
itk_module_target_label(itkMathTest)
target_link_libraries(itkMathTest ${ITK-Common_LIBRARIES})
itk_add_test(NAME itkMathTest COMMAND itkMathTest)

add_executable(itkSystemInformation itkSystemInformation.cxx)
itk_module_target_label(itkSystemInformation)
target_link_libraries(itkSystemInformation ${ITK-Common_LIBRARIES})
itk_add_test(NAME SystemInformation COMMAND itkSystemInformation)


if(ITK_OBJCXX_COMPILER_WORKS)
  add_executable(itkCommonHeaderObjCxxTest itkCommonHeaderObjCxxTest.mm)
  itk_module_target_label(itkCommonHeaderObjCxxTest)
  target_link_libraries(itkCommonHeaderObjCxxTest ${ITK-Common_LIBRARIES})
  itk_add_test(NAME itkCommonHeaderObjCxxTest COMMAND itkCommonHeaderObjCxxTest)
endif(ITK_OBJCXX_COMPILER_WORKS)


if(ITK_BUILD_SHARED_LIBS)
  ## Create a library to test ITK loadable factories
  set(FactoryTest_Source itkFactoryTestLib.cxx)
  add_library(FactoryTestLib MODULE ${FactoryTest_Source})
  itk_module_target_label(FactoryTestLib)
  target_link_libraries(FactoryTestLib ${ITK-Common_LIBRARIES})
  set_property(TARGET FactoryTestLib PROPERTY LIBRARY_OUTPUT_DIRECTORY ${ITK_TEST_OUTPUT_DIR})
  add_dependencies(ITK-Common2TestDriver FactoryTestLib)
endif(ITK_BUILD_SHARED_LIBS)

itk_add_test(NAME itkCommonHeaderTest COMMAND ITK-Common1TestDriver itkCommonHeaderTest)
itk_add_test(NAME itkAdaptorComparisonTest COMMAND ITK-Common1TestDriver itkAdaptorComparisonTest)
itk_add_test(NAME itkImageAdaptorPipeLineTest COMMAND ITK-Common1TestDriver itkImageAdaptorPipeLineTest)
itk_add_test(NAME itkImportContainerTest COMMAND ITK-Common1TestDriver itkImportContainerTest)
itk_add_test(NAME itkImportImageTest COMMAND ITK-Common1TestDriver itkImportImageTest)
itk_add_test(NAME itkCellInterfaceTest COMMAND ITK-Common1TestDriver itkCellInterfaceTest)
itk_add_test(NAME itkCovariantVectorGeometryTest COMMAND ITK-Common1TestDriver itkCovariantVectorGeometryTest)
itk_add_test(NAME itkDataTypeTest COMMAND ITK-Common1TestDriver itkDataTypeTest)
itk_add_test(NAME itkDecoratorTest COMMAND ITK-Common1TestDriver  itkDecoratorTest)
itk_add_test(NAME itkExtractImage3Dto2DTest COMMAND ITK-Common1TestDriver  itkExtractImage3Dto2DTest)
itk_add_test(NAME itkExtractImageTest COMMAND ITK-Common1TestDriver itkExtractImageTest)
itk_add_test(NAME itkFilterDispatchTest COMMAND ITK-Common1TestDriver itkFilterDispatchTest)
itk_add_test(NAME itkFloodFilledSpatialFunctionTest COMMAND ITK-Common1TestDriver itkFloodFilledSpatialFunctionTest)
itk_add_test(NAME itkFloodFillIteratorTest COMMAND ITK-Common1TestDriver itkFloodFillIteratorTest)
itk_add_test(NAME itkAnnulusOperatorTest COMMAND ITK-Common1TestDriver itkAnnulusOperatorTest)
itk_add_test(NAME itkColorTableTest COMMAND ITK-Common1TestDriver itkColorTableTest)
itk_add_test(NAME itkDirectoryTest COMMAND ITK-Common1TestDriver itkDirectoryTest ${TEMP})
itk_add_test(NAME itkDerivativeOperatorTest COMMAND ITK-Common1TestDriver itkDerivativeOperatorTest)
itk_add_test(NAME itkMultipleLogOutputTest COMMAND ITK-Common1TestDriver itkMultipleLogOutputTest ${TEMP}/test_multi.txt)
itk_add_test(NAME itkFixedArrayTest COMMAND ITK-Common2TestDriver itkFixedArrayTest)
itk_add_test(NAME itkImageTransformTest COMMAND ITK-Common2TestDriver itkImageTransformTest)
itk_add_test(NAME itkMinimumMaximumImageCalculatorTest COMMAND ITK-Common2TestDriver itkMinimumMaximumImageCalculatorTest)
itk_add_test(NAME itkFixedArrayTest2 COMMAND ITK-Common1TestDriver itkFixedArrayTest2)
itk_add_test(NAME itkArrayTest COMMAND ITK-Common1TestDriver itkArrayTest)
itk_add_test(NAME itkArray2DTest COMMAND ITK-Common1TestDriver itkArray2DTest)
itk_add_test(NAME itkAutoPointerTest COMMAND ITK-Common2TestDriver itkAutoPointerTest)
if(NOT MINGW)
itk_add_test(NAME itkBarrierTest COMMAND ITK-Common2TestDriver itkBarrierTest)
itk_add_test(NAME itkConditionVariableTest COMMAND ITK-Common1TestDriver itkConditionVariableTest)
endif(NOT MINGW)
itk_add_test(NAME itkTimeStampTest COMMAND ITK-Common2TestDriver itkTimeStampTest)
itk_add_test(NAME itkBoundingBoxTest COMMAND ITK-Common1TestDriver itkBoundingBoxTest)
itk_add_test(NAME itkBoundaryConditionTest COMMAND ITK-Common1TestDriver itkBoundaryConditionTest)
itk_add_test(NAME itkByteSwapTest COMMAND ITK-Common1TestDriver itkByteSwapTest)
itk_add_test(NAME itkBSplineInterpolationWeightFunctionTest COMMAND ITK-Common2TestDriver itkBSplineInterpolationWeightFunctionTest)
itk_add_test(NAME itkBSplineKernelFunctionTest COMMAND ITK-Common1TestDriver itkBSplineKernelFunctionTest)
itk_add_test(NAME itkConstNeighborhoodIteratorTest COMMAND ITK-Common2TestDriver itkConstNeighborhoodIteratorTest)
itk_add_test(NAME itkShapedNeighborhoodIteratorTest COMMAND ITK-Common2TestDriver itkShapedNeighborhoodIteratorTest)
itk_add_test(NAME itkConstShapedNeighborhoodIteratorTest COMMAND ITK-Common2TestDriver itkConstShapedNeighborhoodIteratorTest)
itk_add_test(NAME itkConstShapedNeighborhoodIteratorTest2 COMMAND ITK-Common2TestDriver itkConstShapedNeighborhoodIteratorTest2)
itk_add_test(NAME itkEventObjectTest COMMAND ITK-Common1TestDriver itkEventObjectTest)
itk_add_test(NAME itkMathCastWithRangeCheckTest COMMAND ITK-Common1TestDriver itkMathCastWithRangeCheckTest)
itk_add_test(NAME itkMathRoundProfileTest1
      COMMAND ITK-Common1TestDriver itkMathRoundProfileTest1)
itk_add_test(NAME itkMathRoundTest
      COMMAND ITK-Common1TestDriver itkMathRoundTest)
itk_add_test(NAME itkMathRoundTest2
      COMMAND ITK-Common1TestDriver itkMathRoundTest2)
itk_add_test(NAME itkModifiedTimeTest
      COMMAND ITK-Common1TestDriver itkModifiedTimeTest)
itk_add_test(NAME itkExceptionObjectTest COMMAND ITK-Common1TestDriver itkExceptionObjectTest)
itk_add_test(NAME itkFileOutputWindowTest COMMAND ITK-Common1TestDriver itkFileOutputWindowTest)
itk_add_test(NAME itkFiniteCylinderSpatialFunctionTest COMMAND ITK-Common2TestDriver itkFiniteCylinderSpatialFunctionTest)
itk_add_test(NAME itkGaussianSpatialFunctionTest COMMAND ITK-Common1TestDriver itkGaussianSpatialFunctionTest)
itk_add_test(NAME itkCrossHelperTest COMMAND ITK-Common1TestDriver itkCrossHelperTest)
itk_add_test(NAME itkImageIteratorTest COMMAND ITK-Common1TestDriver itkImageIteratorTest)
itk_add_test(NAME itkImageIteratorWithIndexTest COMMAND ITK-Common1TestDriver itkImageIteratorWithIndexTest)
itk_add_test(NAME itkImageRegionIteratorTest COMMAND ITK-Common1TestDriver itkImageRegionIteratorTest)
itk_add_test(NAME itkImageRegionTest COMMAND ITK-Common1TestDriver itkImageRegionTest)
itk_add_test(NAME itkImageRegionExclusionIteratorWithIndexTest COMMAND ITK-Common2TestDriver itkImageRegionExclusionIteratorWithIndexTest)
itk_add_test(NAME itkImageReverseIteratorTest COMMAND ITK-Common1TestDriver itkImageReverseIteratorTest)
itk_add_test(NAME itkImageDuplicatorTest
      COMMAND ITK-Common1TestDriver itkImageDuplicatorTest)
itk_add_test(NAME itkImageIteratorsForwardBackwardTest
      COMMAND ITK-Common1TestDriver itkImageIteratorsForwardBackwardTest)
itk_add_test(NAME itkImageLinearIteratorTest
      COMMAND ITK-Common1TestDriver itkImageLinearIteratorTest)
itk_add_test(NAME itkImageRandomIteratorTest
      COMMAND ITK-Common1TestDriver itkImageRandomIteratorTest)
itk_add_test(NAME itkImageRandomIteratorTest2
      COMMAND ITK-Common1TestDriver itkImageRandomIteratorTest2
              ${ITK_TEST_OUTPUT_DIR}/itkImageRandomIteratorTest2Output.mha)
itk_add_test(NAME itkImageRandomNonRepeatingIteratorWithIndexTest
      COMMAND ITK-Common1TestDriver itkImageRandomNonRepeatingIteratorWithIndexTest)
itk_add_test(NAME itkImageRandomNonRepeatingIteratorWithIndexTest2
      COMMAND ITK-Common1TestDriver itkImageRandomNonRepeatingIteratorWithIndexTest2)
itk_add_test(NAME itkImageSliceIteratorTest
      COMMAND ITK-Common1TestDriver itkImageSliceIteratorTest)
itk_add_test(NAME itkImageTest COMMAND ITK-Common1TestDriver itkImageTest)
itk_add_test(NAME itkLightObjectTest COMMAND ITK-Common1TestDriver itkLightObjectTest)
itk_add_test(NAME itkLineIteratorTest COMMAND ITK-Common1TestDriver itkLineIteratorTest ${BASELINE}/itkLineIteratorTest.txt )
itk_add_test(NAME itkLoggerTest COMMAND ITK-Common1TestDriver itkLoggerTest ${TEMP}/test_logger.txt)
itk_add_test(NAME itkLoggerOutputTest COMMAND ITK-Common2TestDriver itkLoggerOutputTest ${TEMP}/test_loggerOutput.txt)
itk_add_test(NAME itkLoggerManagerTest COMMAND ITK-Common2TestDriver itkLoggerManagerTest ${TEMP}/test_LoggerManager.txt)
itk_add_test(NAME itkMatrixTest COMMAND ITK-Common2TestDriver itkMatrixTest)
itk_add_test(NAME itkMapContainerTest COMMAND ITK-Common1TestDriver itkMapContainerTest)
itk_add_test(NAME itkIteratorTests COMMAND ITK-Common1TestDriver itkIteratorTests)
itk_add_test(NAME itkObjectFactoryTest COMMAND ITK-Common1TestDriver itkObjectFactoryTest)
itk_add_test(NAME itkVectorTest COMMAND ITK-Common1TestDriver itkVectorTest)
itk_add_test(NAME itkPointSetTest COMMAND ITK-Common1TestDriver itkPointSetTest)
itk_add_test(NAME itkBresenhamLineTest COMMAND ITK-Common1TestDriver itkBresenhamLineTest)
itk_add_test(NAME itkSparseFieldLayerTest COMMAND ITK-Common1TestDriver itkSparseFieldLayerTest)
itk_add_test(NAME itkDataObjectTest COMMAND ITK-Common1TestDriver itkDataObjectTest)
itk_add_test(NAME itkAtanRegularizedHeavisideStepFunctionTest1
 COMMAND ITK-Common1TestDriver itkAtanRegularizedHeavisideStepFunctionTest1)
itk_add_test(NAME itkHashTableTest COMMAND ITK-Common1TestDriver itkHashTableTest)
itk_add_test(NAME itkHeavisideStepFunctionTest1
 COMMAND ITK-Common1TestDriver itkHeavisideStepFunctionTest1)
itk_add_test(NAME itkSinRegularizedHeavisideStepFunctionTest1
 COMMAND ITK-Common1TestDriver itkSinRegularizedHeavisideStepFunctionTest1)
itk_add_test(NAME itkPixelAccessTest COMMAND ITK-Common1TestDriver itkPixelAccessTest)
itk_add_test(NAME itkPointGeometryTest COMMAND ITK-Common1TestDriver itkPointGeometryTest)

#
# This test should be enabled if you suspect that the memory leak detector
# is not working
if(0)
  itk_add_test(NAME itkMemoryLeakTest COMMAND ITK-Common2TestDriver itkMemoryLeakTest)
endif(0)

itk_add_test(NAME itkMetaDataDictionaryTest COMMAND ITK-Common2TestDriver itkMetaDataDictionaryTest)
itk_add_test(NAME itkMultiThreaderTest COMMAND ITK-Common2TestDriver itkMultiThreaderTest)
itk_add_test(NAME itkNeighborhoodAlgorithmTest COMMAND ITK-Common1TestDriver itkNeighborhoodAlgorithmTest)
itk_add_test(NAME itkNeighborhoodTest COMMAND ITK-Common2TestDriver itkNeighborhoodTest)
itk_add_test(NAME itkNeighborhoodIteratorTest COMMAND ITK-Common2TestDriver itkNeighborhoodIteratorTest)
itk_add_test(NAME itkNeighborhoodOperatorTest COMMAND ITK-Common1TestDriver itkNeighborhoodOperatorTest)
itk_add_test(NAME itkNewTest COMMAND ITK-Common1TestDriver itkNewTest)
itk_add_test(NAME itkNumericsTest COMMAND ITK-Common1TestDriver itkNumericsTest)
itk_add_test(NAME itkNumericTraitsTest COMMAND ITK-Common1TestDriver itkNumericTraitsTest)
itk_add_test(NAME itkIntTypesTest COMMAND ITK-Common1TestDriver itkIntTypesTest)
itk_add_test(NAME itkOctreeTest COMMAND ITK-Common2TestDriver itkOctreeTest)


if(ITK_BUILD_SHARED_LIBS)
itk_add_test(NAME itkObjectFactoryTest2 COMMAND ITK-Common2TestDriver itkObjectFactoryTest2 ${ITK_TEST_OUTPUT_DIR})
set_tests_properties(itkObjectFactoryTest2 PROPERTIES RUN_SERIAL 1)
endif(ITK_BUILD_SHARED_LIBS)

itk_add_test(NAME itkObjectFactoryTest3 COMMAND ITK-Common2TestDriver itkObjectFactoryTest3)
itk_add_test(NAME itkPeriodicBoundaryConditionTest COMMAND ITK-Common2TestDriver itkPeriodicBoundaryConditionTest)
itk_add_test(NAME itkPhasedArray3DSpecialCoordinatesImageTest COMMAND ITK-Common1TestDriver itkPhasedArray3DSpecialCoordinatesImageTest)
itk_add_test(NAME itkPriorityQueueTest COMMAND ITK-Common1TestDriver itkPriorityQueueTest)
itk_add_test(NAME itkRealTimeClockTest COMMAND ITK-Common1TestDriver itkRealTimeClockTest)
itk_add_test(NAME itkRealTimeStampTest COMMAND ITK-Common1TestDriver itkRealTimeStampTest)
itk_add_test(NAME itkRealTimeIntervalTest COMMAND ITK-Common1TestDriver itkRealTimeIntervalTest)
itk_add_test(NAME itkTriangleCellTest COMMAND ITK-Common1TestDriver itkTriangleCellTest)
itk_add_test(NAME itkRGBPixelTest COMMAND ITK-Common1TestDriver itkRGBPixelTest)
itk_add_test(NAME itkScalarToRGBPixelFunctorTest COMMAND ITK-Common1TestDriver itkScalarToRGBPixelFunctorTest)
itk_add_test(NAME itkSparseImageTest COMMAND ITK-Common1TestDriver itkSparseImageTest)
itk_add_test(NAME itkSimpleFilterWatcherTest COMMAND ITK-Common1TestDriver itkSimpleFilterWatcherTest)
itk_add_test(NAME itkSizeTest COMMAND ITK-Common2TestDriver itkSizeTest)
itk_add_test(NAME itkSliceIteratorTest COMMAND ITK-Common2TestDriver itkSliceIteratorTest)
itk_add_test(NAME itkSmartPointerTest COMMAND ITK-Common2TestDriver itkSmartPointerTest)
itk_add_test(NAME itkSpatialFunctionTest COMMAND ITK-Common2TestDriver itkSpatialFunctionTest)
itk_add_test(NAME itkSTLContainerAdaptorTest COMMAND ITK-Common2TestDriver itkSTLContainerAdaptorTest)
itk_add_test(NAME itkStdStreamLogOutputTest COMMAND ITK-Common2TestDriver itkStdStreamLogOutputTest ${TEMP}/testStreamLogOutput.txt)
itk_add_test(NAME itkThreadLoggerTest COMMAND ITK-Common2TestDriver itkThreadLoggerTest ${TEMP}/test_threadLogger.txt)
itk_add_test(NAME itkThreadDefsTest COMMAND ITK-Common2TestDriver itkThreadDefsTest)
itk_add_test(NAME itkTimeProbesTest COMMAND ITK-Common2TestDriver itkTimeProbesTest)
itk_add_test(NAME itkTreeContainerTest COMMAND ITK-Common2TestDriver itkTreeContainerTest)
itk_add_test(NAME itkTreeContainerTest2 COMMAND ITK-Common1TestDriver itkTreeContainerTest2)
itk_add_test(NAME itkVersorTest COMMAND ITK-Common2TestDriver itkVersorTest)
itk_add_test(NAME itkEllipsoidInteriorExteriorSpatialFunctionTest COMMAND ITK-Common2TestDriver itkEllipsoidInteriorExteriorSpatialFunctionTest)
itk_add_test(NAME itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest COMMAND ITK-Common1TestDriver itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest)
itk_add_test(NAME itkSymmetricSecondRankTensorImageReadTest COMMAND ITK-Common1TestDriver itkSymmetricSecondRankTensorImageReadTest
              ${ITK_TEST_OUTPUT_DIR}/testSymmetricTensor.nrrd)
itk_add_test(NAME itkSymmetricSecondRankTensorImageWriteReadTest COMMAND ITK-Common1TestDriver itkSymmetricSecondRankTensorImageWriteReadTest
              ${ITK_TEST_OUTPUT_DIR}/testSymmetricTensorWriteRead.mha)
itk_add_test(NAME itkSymmetricSecondRankTensorTest COMMAND ITK-Common2TestDriver itkSymmetricSecondRankTensorTest)
itk_add_test(NAME itkSymmetricEigenAnalysisTest COMMAND ITK-Common1TestDriver itkSymmetricEigenAnalysisTest)
itk_add_test(NAME itkSTLThreadTest COMMAND ITK-Common1TestDriver itkSTLThreadTest)
itk_add_test(NAME itkStreamingImageFilterTest COMMAND ITK-Common1TestDriver itkStreamingImageFilterTest)
itk_add_test(NAME itkStreamingImageFilterTest2 COMMAND ITK-Common1TestDriver itkStreamingImageFilterTest2)
itk_add_test(NAME itkStreamingImageFilterTest3_1 COMMAND ITK-Common1TestDriver
    --compare ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png
              ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_1.png
    itkStreamingImageFilterTest3 ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_1.png 10)
itk_add_test(NAME itkStreamingImageFilterTest3_2
      COMMAND ITK-Common1TestDriver
    --compare ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png
              ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_2.png
    itkStreamingImageFilterTest3 ${ITK_DATA_ROOT}/Input/CellsFluorescence1.png ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_2.png 1000)
itk_add_test(NAME itkVariableLengthVectorTest COMMAND ITK-Common2TestDriver itkVariableLengthVectorTest)
itk_add_test(NAME itkVariableSizeMatrixTest COMMAND ITK-Common2TestDriver itkVariableSizeMatrixTest)
#itk_add_test(NAME itkQuaternionOrientationAdapterTest COMMAND ITK-Common2TestDriver itkQuaternionOrientationAdapterTest)
itk_add_test(NAME itkVectorGeometryTest COMMAND ITK-Common2TestDriver itkVectorGeometryTest)
itk_add_test(NAME itkZeroFluxBoundaryConditionTest COMMAND ITK-Common2TestDriver itkZeroFluxBoundaryConditionTest)
 itk_add_test(NAME itkVNLRoundProfileTest1 COMMAND ITK-Common2TestDriver itkVNLRoundProfileTest1)
 itk_add_test(NAME itkMemoryProbesCollecterBaseTest COMMAND ITK-Common2TestDriver
   itkMemoryProbesCollecterBaseTest)
if( "${ITK_COMPUTER_MEMORY_SIZE}" GREATER 4 )
  itk_add_test(NAME itkImageFillBufferTest4.1 COMMAND ITK-Common2TestDriver itkImageFillBufferTest 4.1)
endif()

#
# this test depends on being able to raise C++ exceptions from
# the signal handler. This is apparently not something that
# works properly
if("${CMAKE_SYSTEM_NAME}" MATCHES "Linux")
  itk_add_test(NAME itkFloatingPointExceptionsTest1 COMMAND ITK-Common1TestDriver
    itkFloatingPointExceptionsTest DivByZero)
  itk_add_test(NAME itkFloatingPointExceptionsTest2 COMMAND ITK-Common1TestDriver
    itkFloatingPointExceptionsTest ZeroDivByZero)
  itk_add_test(NAME itkFloatingPointExceptionsTest3 COMMAND ITK-Common1TestDriver
    itkFloatingPointExceptionsTest FPOverFlow)
  itk_add_test(NAME itkFloatingPointExceptionsTest4 COMMAND ITK-Common1TestDriver
    itkFloatingPointExceptionsTest FPUnderFlow)
  itk_add_test(NAME itkFloatingPointExceptionsTest5 COMMAND ITK-Common1TestDriver
    itkFloatingPointExceptionsTest IntDivByZero)

  set_tests_properties(
    itkFloatingPointExceptionsTest1
    itkFloatingPointExceptionsTest2
    itkFloatingPointExceptionsTest3
    itkFloatingPointExceptionsTest4
    itkFloatingPointExceptionsTest5
    PROPERTIES WILL_FAIL TRUE
    )

endif("${CMAKE_SYSTEM_NAME}" MATCHES "Linux")

add_executable(itkSystemInformationTest itkSystemInformationTest.cxx)
itk_module_target_label(itkSystemInformationTest)
target_link_libraries(itkSystemInformationTest  ${ITK-Common_LIBRARIES})
itk_add_test(NAME itkSystemInformationTest
         COMMAND itkSystemInformationTest ${CMAKE_BINARY_DIR})
