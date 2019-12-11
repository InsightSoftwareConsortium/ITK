itk_module_test()
set(ITKCommon1Tests
itkImageRegionExplicitTest.cxx
itkAbortProcessObjectTest.cxx
itkCommandObserverObjectTest.cxx
itkAdaptorComparisonTest.cxx
itkCovariantVectorGeometryTest.cxx
itkDataTypeTest.cxx
itkDecoratorTest.cxx
itkExtractImage3Dto2DTest.cxx
itkExtractImageTest.cxx
itkFilterDispatchTest.cxx
itkFloodFilledSpatialFunctionTest.cxx
itkFloodFillIteratorTest.cxx
itkGaussianDerivativeOperatorTest.cxx
itkMapContainerTest.cxx
itkIteratorTests.cxx
itkImageReverseIteratorTest.cxx
itkImageComputeOffsetAndIndexTest.cxx
itkImageDuplicatorTest.cxx
itkImageDuplicatorTest2.cxx
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
itkRGBPixelTest.cxx
itkLightObjectTest.cxx
itkBoundingBoxTest.cxx
itkBoundaryConditionTest.cxx
itkByteSwapTest.cxx
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
itkNumericsTest.cxx
itkAnnulusOperatorTest.cxx
itkLineIteratorTest.cxx
itkGaussianSpatialFunctionTest.cxx
itkRealTimeClockTest.cxx
itkRealTimeIntervalTest
itkRealTimeStampTest
itkTimeStampTest.cxx
itkIntTypesTest.cxx
itkBSplineKernelFunctionTest.cxx
itkArrayTest.cxx
itkImageIteratorTest.cxx
itkImageRegionIteratorTest.cxx
itkImageScanlineIteratorTest1.cxx
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
itkMersenneTwisterRandomVariateGeneratorTest.cxx
itkNumberToStringTest.cxx
itkTimeProbeTest.cxx
itkTimeProbeTest2.cxx
itkStdStreamStateSaveTest.cxx
itkVersionTest.cxx
VNLSparseLUSolverTraitsTest.cxx
)
set(ITKCommon2Tests
itkSTLThreadTest.cxx
itkThreadedIndexedContainerPartitionerTest.cxx
itkThreadedIteratorRangePartitionerTest.cxx
itkThreadedIteratorRangePartitionerTest2.cxx
itkThreadedIteratorRangePartitionerTest3.cxx
itkThreadedImageRegionPartitionerTest.cxx
itkThreadLoggerTest.cxx
itkLoggerThreadWrapperTest.cxx
itkThreadDefsTest.cxx
itkMultiThreaderBaseTest.cxx
itkMultiThreaderTypeFromEnvironmentTest
itkMultiThreadingEnvironmentTest.cxx
itkMultiThreaderParallelizeArrayTest.cxx
itkMultithreadingTest.cxx

itkMetaProgrammingLibraryTest.cxx
itkIsConvertible.cxx
itkIsBaseOf.cxx
itkPromoteType.cxx
itkMetaDataDictionaryTest.cxx
itkStdStreamLogOutputTest.cxx
itkOctreeTest.cxx
itkTimeProbesTest.cxx
itkVariableLengthVectorTest.cxx
itkSpatialFunctionTest.cxx
itkPeriodicBoundaryConditionTest.cxx
itkSmartPointerTest.cxx
itkVariableSizeMatrixTest.cxx
itkConicShellInteriorExteriorSpatialFunctionTest.cxx
itkEllipsoidInteriorExteriorSpatialFunctionTest.cxx
itkFrustumSpatialFunctionTest.cxx
itkTorusInteriorExteriorSpatialFunctionTest.cxx
itkConstNeighborhoodIteratorTest.cxx
itkShapedNeighborhoodIteratorTest.cxx
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
itkImageRegionExclusionIteratorWithIndexTest.cxx
itkFixedArrayTest.cxx
itkImageTransformTest.cxx
itkImageFillBufferTest.cxx
itkMemoryLeakTest.cxx
itkVectorGeometryTest.cxx
itkVNLRoundProfileTest1.cxx
itkZeroFluxBoundaryConditionTest.cxx
itkMemoryProbesCollecterBaseTest.cxx
itkImageAlgorithmCopyTest.cxx
itkImageAlgorithmCopyTest2.cxx
itkConstantBoundaryConditionTest.cxx
itkDataObjectAndProcessObjectTest.cxx
itkOptimizerParametersTest.cxx
itkImageVectorOptimizerParametersHelperTest.cxx
itkCompensatedSummationTest.cxx
itkCompensatedSummationTest2.cxx
itkEnableIfTest.cxx
itkImageRegionConstIteratorWithOnlyIndexTest.cxx
itkImageRandomConstIteratorWithOnlyIndexTest.cxx
itkConstNeighborhoodIteratorWithOnlyIndexTest.cxx
itkImageToImageToleranceTest.cxx
itkImageRegionSplitterSlowDimensionTest.cxx
itkImageRegionSplitterDirectionTest.cxx
itkImageRegionSplitterMultidimensionalTest.cxx
itkMetaDataObjectTest.cxx
# itkVectorMultiplyTest.cxx
)
if(ITK_BUILD_SHARED_LIBS AND ITK_DYNAMIC_LOADING)
  list(APPEND ITKCommon2Tests itkDownCastTest.cxx)
endif()

# Note: Tests are arbitrarly split into two drivers to avoid memory issues.
CreateTestDriver(ITKCommon1 "${ITKCommon-Test_LIBRARIES}" "${ITKCommon1Tests}" itkFloatingPointExceptionsExtern.cxx)
CreateTestDriver(ITKCommon2 "${ITKCommon-Test_LIBRARIES}" "${ITKCommon2Tests}" itkFloatingPointExceptionsExtern.cxx)

set(BASELINE "${ITK_DATA_ROOT}/Baseline/Common")
set(TEMP ${ITK_TEST_OUTPUT_DIR})


add_executable(itkMathTest itkMathTest.cxx )
itk_module_target_label(itkMathTest)
target_link_libraries(itkMathTest LINK_PUBLIC ${ITKCommon_LIBRARIES})
itk_add_test(NAME itkMathTest COMMAND itkMathTest)

add_executable(itkSystemInformation itkSystemInformation.cxx)
itk_module_target_label(itkSystemInformation)
target_link_libraries(itkSystemInformation LINK_PUBLIC ${ITKCommon_LIBRARIES})
itk_add_test(NAME SystemInformation COMMAND itkSystemInformation)

itk_add_test(NAME itkVersionTest COMMAND ITKCommon1TestDriver itkVersionTest)

if(ITK_BUILD_SHARED_LIBS)
  ## Create a library to test ITK loadable factories
  set(FactoryTest_Source itkFactoryTestLib.cxx)
  add_library(FactoryTestLib MODULE ${FactoryTest_Source})
  itk_module_target_label(FactoryTestLib)
  target_link_libraries(FactoryTestLib LINK_PUBLIC ${ITKCommon_LIBRARIES})
  set_property(TARGET FactoryTestLib PROPERTY LIBRARY_OUTPUT_DIRECTORY ${ITK_TEST_OUTPUT_DIR})
  add_dependencies(ITKCommon2TestDriver FactoryTestLib)
endif()

itk_add_test(NAME itkAbortProcessObjectTest COMMAND ITKCommon1TestDriver itkAbortProcessObjectTest)
itk_add_test(NAME itkCommandObserverObjectTest COMMAND ITKCommon1TestDriver itkCommandObserverObjectTest)
itk_add_test(NAME itkAdaptorComparisonTest COMMAND ITKCommon1TestDriver itkAdaptorComparisonTest)
itk_add_test(NAME itkThreadedIndexedContainerPartitionerTest COMMAND ITKCommon2TestDriver itkThreadedIndexedContainerPartitionerTest)
itk_add_test(NAME itkThreadedIteratorRangePartitionerTest COMMAND ITKCommon2TestDriver itkThreadedIteratorRangePartitionerTest)
itk_add_test(NAME itkThreadedIteratorRangePartitionerTest2 COMMAND ITKCommon2TestDriver itkThreadedIteratorRangePartitionerTest2)
itk_add_test(NAME itkThreadedIteratorRangePartitionerTest3 COMMAND ITKCommon2TestDriver itkThreadedIteratorRangePartitionerTest3)

itk_add_test(NAME itkImageAdaptorPipeLineTest COMMAND ITKCommon1TestDriver itkImageAdaptorPipeLineTest)
itk_add_test(NAME itkThreadedImageRegionPartitionerTest COMMAND ITKCommon2TestDriver itkThreadedImageRegionPartitionerTest)
itk_add_test(NAME itkImportContainerTest COMMAND ITKCommon1TestDriver itkImportContainerTest)
itk_add_test(NAME itkImportImageTest COMMAND ITKCommon1TestDriver itkImportImageTest)
itk_add_test(NAME itkCovariantVectorGeometryTest COMMAND ITKCommon1TestDriver itkCovariantVectorGeometryTest)
itk_add_test(NAME itkDataTypeTest COMMAND ITKCommon1TestDriver itkDataTypeTest)
itk_add_test(NAME itkDecoratorTest COMMAND ITKCommon1TestDriver  itkDecoratorTest)
itk_add_test(NAME itkExtractImage3Dto2DTest COMMAND ITKCommon1TestDriver  itkExtractImage3Dto2DTest)
itk_add_test(NAME itkExtractImageTest COMMAND ITKCommon1TestDriver itkExtractImageTest)
itk_add_test(NAME itkFilterDispatchTest COMMAND ITKCommon1TestDriver itkFilterDispatchTest)
itk_add_test(NAME itkFloodFilledSpatialFunctionTest COMMAND ITKCommon1TestDriver itkFloodFilledSpatialFunctionTest)
itk_add_test(NAME itkFloodFillIteratorTest COMMAND ITKCommon1TestDriver itkFloodFillIteratorTest)
itk_add_test(NAME itkAnnulusOperatorTest COMMAND ITKCommon1TestDriver itkAnnulusOperatorTest)

itk_add_test(NAME itkColorTableTest1 COMMAND
             ITKCommon1TestDriver --redirectOutput ${TEMP}/itkColorTableTest1.txt
             itkColorTableTest 8)
set_tests_properties(itkColorTableTest1 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkColorTableTest1.txt)
itk_add_test(NAME itkColorTableTest2 COMMAND
             ITKCommon1TestDriver --redirectOutput ${TEMP}/itkColorTableTest2.txt
             itkColorTableTest 16)
set_tests_properties(itkColorTableTest2 PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkColorTableTest2.txt)

itk_add_test(NAME itkDirectoryTest COMMAND ITKCommon1TestDriver itkDirectoryTest ${TEMP})
itk_add_test(NAME itkDerivativeOperatorTest COMMAND ITKCommon1TestDriver itkDerivativeOperatorTest)
itk_add_test(NAME itkMultipleLogOutputTest COMMAND ITKCommon1TestDriver itkMultipleLogOutputTest ${TEMP}/test_multi.txt)
itk_add_test(NAME itkFixedArrayTest COMMAND ITKCommon2TestDriver itkFixedArrayTest)
itk_add_test(NAME itkImageTransformTest COMMAND ITKCommon2TestDriver itkImageTransformTest)
itk_add_test(NAME itkMinimumMaximumImageCalculatorTest COMMAND ITKCommon2TestDriver itkMinimumMaximumImageCalculatorTest)
itk_add_test(NAME itkFixedArrayTest2 COMMAND ITKCommon1TestDriver itkFixedArrayTest2)
itk_add_test(NAME itkArrayTest COMMAND ITKCommon1TestDriver itkArrayTest)
itk_add_test(NAME itkMersenneTwisterRandomVariateGeneratorTest COMMAND ITKCommon1TestDriver itkMersenneTwisterRandomVariateGeneratorTest)
itk_add_test(NAME itkArray2DTest COMMAND ITKCommon1TestDriver itkArray2DTest)
itk_add_test(NAME itkAutoPointerTest COMMAND ITKCommon2TestDriver itkAutoPointerTest)
itk_add_test(NAME itkTimeStampTest COMMAND ITKCommon1TestDriver itkTimeStampTest)
itk_add_test(NAME itkBoundingBoxTest COMMAND ITKCommon1TestDriver itkBoundingBoxTest)
itk_add_test(NAME itkBoundaryConditionTest COMMAND ITKCommon1TestDriver itkBoundaryConditionTest)
itk_add_test(NAME itkByteSwapTest COMMAND ITKCommon1TestDriver itkByteSwapTest)
itk_add_test(NAME itkBSplineInterpolationWeightFunctionTest COMMAND ITKCommon2TestDriver itkBSplineInterpolationWeightFunctionTest)
itk_add_test(NAME itkBSplineKernelFunctionTest COMMAND ITKCommon1TestDriver itkBSplineKernelFunctionTest)
itk_add_test(NAME VNLSparseLUSolverTraitsTest COMMAND ITKCommon1TestDriver VNLSparseLUSolverTraitsTest)
itk_add_test(NAME itkGaussianDerivativeOperatorTest
  COMMAND ITKCommon1TestDriver itkGaussianDerivativeOperatorTest)

itk_add_test(NAME itkConstNeighborhoodIteratorTest
      COMMAND ITKCommon2TestDriver --redirectOutput ${TEMP}/itkConstNeighborhoodIteratorTest.txt
    itkConstNeighborhoodIteratorTest)
set_tests_properties(itkConstNeighborhoodIteratorTest PROPERTIES ATTACHED_FILES_ON_FAIL ${ITK_TEST_OUTPUT_DIR}/itkConstNeighborhoodIteratorTest.txt)

itk_add_test(NAME itkShapedNeighborhoodIteratorTest
      COMMAND ITKCommon2TestDriver  --redirectOutput ${TEMP}/itkShapedNeighborhoodIteratorTest.txt
    itkShapedNeighborhoodIteratorTest)
set_tests_properties(itkShapedNeighborhoodIteratorTest PROPERTIES ATTACHED_FILES_ON_FAIL ${ITK_TEST_OUTPUT_DIR}/itkShapedNeighborhoodIteratorTest.txt)


itk_add_test(NAME itkConstShapedNeighborhoodIteratorTest COMMAND ITKCommon2TestDriver itkConstShapedNeighborhoodIteratorTest)
itk_add_test(NAME itkConstShapedNeighborhoodIteratorTest2 COMMAND ITKCommon2TestDriver itkConstShapedNeighborhoodIteratorTest2)
itk_add_test(NAME itkEventObjectTest COMMAND ITKCommon1TestDriver itkEventObjectTest)
itk_add_test(NAME itkMathCastWithRangeCheckTest COMMAND ITKCommon1TestDriver itkMathCastWithRangeCheckTest)
itk_add_test(NAME itkMathRoundProfileTest1
      COMMAND ITKCommon1TestDriver itkMathRoundProfileTest1)
itk_add_test(NAME itkMathRoundTest
      COMMAND ITKCommon1TestDriver itkMathRoundTest)
itk_add_test(NAME itkMathRoundTest2
      COMMAND ITKCommon1TestDriver itkMathRoundTest2)
itk_add_test(NAME itkModifiedTimeTest
      COMMAND ITKCommon1TestDriver itkModifiedTimeTest)
itk_add_test(NAME itkExceptionObjectTest COMMAND ITKCommon1TestDriver itkExceptionObjectTest)
itk_add_test(NAME itkFileOutputWindowTest COMMAND ITKCommon1TestDriver itkFileOutputWindowTest)
itk_add_test(NAME itkFiniteCylinderSpatialFunctionTest COMMAND ITKCommon2TestDriver itkFiniteCylinderSpatialFunctionTest)
itk_add_test(NAME itkGaussianSpatialFunctionTest1
      COMMAND ITKCommon1TestDriver
      itkGaussianSpatialFunctionTest 1.0 0)
itk_add_test(NAME itkGaussianSpatialFunctionTest2
      COMMAND ITKCommon1TestDriver
      itkGaussianSpatialFunctionTest 19.0 1)
itk_add_test(NAME itkCrossHelperTest COMMAND ITKCommon1TestDriver itkCrossHelperTest)
itk_add_test(NAME itkImageIteratorTest COMMAND ITKCommon1TestDriver itkImageIteratorTest)
itk_add_test(NAME itkImageIteratorWithIndexTest COMMAND ITKCommon1TestDriver itkImageIteratorWithIndexTest)
itk_add_test(NAME itkImageRegionIteratorTest COMMAND ITKCommon1TestDriver itkImageRegionIteratorTest)
itk_add_test(NAME itkImageScanlineIteratorTest1 COMMAND ITKCommon1TestDriver itkImageScanlineIteratorTest1)
itk_add_test(NAME itkImageRegionTest COMMAND ITKCommon1TestDriver itkImageRegionTest)
itk_add_test(NAME itkImageRegionExclusionIteratorWithIndexTest COMMAND ITKCommon2TestDriver itkImageRegionExclusionIteratorWithIndexTest)
itk_add_test(NAME itkImageReverseIteratorTest COMMAND ITKCommon1TestDriver itkImageReverseIteratorTest)
itk_add_test(NAME itkImageDuplicatorTest
      COMMAND ITKCommon1TestDriver itkImageDuplicatorTest)
itk_add_test(NAME itkImageDuplicatorTest2
      COMMAND ITKCommon1TestDriver --compare DATA{Baseline/vol-raw-big-dup2.nrrd} ${ITK_TEST_OUTPUT_DIR}/vol-raw-big-dup2.nrrd
      itkImageDuplicatorTest2 DATA{${ITK_DATA_ROOT}/Input/vol-raw-big.nrrd} ${ITK_TEST_OUTPUT_DIR}/vol-raw-big-dup2.nrrd)
itk_add_test(NAME itkImageIteratorsForwardBackwardTest
      COMMAND ITKCommon1TestDriver itkImageIteratorsForwardBackwardTest)
itk_add_test(NAME itkImageLinearIteratorTest
      COMMAND ITKCommon1TestDriver itkImageLinearIteratorTest)
itk_add_test(NAME itkImageRandomIteratorTest
      COMMAND ITKCommon1TestDriver itkImageRandomIteratorTest)
itk_add_test(NAME itkImageRandomIteratorTest2
      COMMAND ITKCommon1TestDriver itkImageRandomIteratorTest2
              ${ITK_TEST_OUTPUT_DIR}/itkImageRandomIteratorTest2Output.mha)
itk_add_test(NAME itkImageRandomNonRepeatingIteratorWithIndexTest
      COMMAND ITKCommon1TestDriver itkImageRandomNonRepeatingIteratorWithIndexTest)
itk_add_test(NAME itkImageRandomNonRepeatingIteratorWithIndexTest2
      COMMAND ITKCommon1TestDriver itkImageRandomNonRepeatingIteratorWithIndexTest2)
itk_add_test(NAME itkImageSliceIteratorTest
      COMMAND ITKCommon1TestDriver itkImageSliceIteratorTest)
itk_add_test(NAME itkImageTest COMMAND ITKCommon1TestDriver itkImageTest)
itk_add_test(NAME itkLightObjectTest COMMAND ITKCommon1TestDriver itkLightObjectTest)
itk_add_test(NAME itkLineIteratorTest COMMAND ITKCommon1TestDriver itkLineIteratorTest DATA{${BASELINE}/itkLineIteratorTest.txt} )
itk_add_test(NAME itkLoggerTest COMMAND ITKCommon1TestDriver itkLoggerTest ${TEMP}/test_logger.txt)
itk_add_test(NAME itkLoggerOutputTest COMMAND ITKCommon2TestDriver itkLoggerOutputTest ${TEMP}/test_loggerOutput.txt)
itk_add_test(NAME itkLoggerManagerTest COMMAND ITKCommon2TestDriver itkLoggerManagerTest ${TEMP}/test_LoggerManager.txt)
itk_add_test(NAME itkLoggerThreadWrapperTest COMMAND ITKCommon2TestDriver itkLoggerThreadWrapperTest ${TEMP}/test_LoggerThreadWrapper.txt)
itk_add_test(NAME itkMatrixTest COMMAND ITKCommon2TestDriver itkMatrixTest)
itk_add_test(NAME itkMapContainerTest COMMAND ITKCommon1TestDriver itkMapContainerTest)
itk_add_test(NAME itkIteratorTests COMMAND ITKCommon1TestDriver itkIteratorTests)
itk_add_test(NAME itkObjectFactoryTest COMMAND ITKCommon1TestDriver itkObjectFactoryTest)
itk_add_test(NAME itkVectorTest COMMAND ITKCommon1TestDriver itkVectorTest)
itk_add_test(NAME itkPointSetTest COMMAND ITKCommon1TestDriver itkPointSetTest)
itk_add_test(NAME itkBresenhamLineTest COMMAND ITKCommon1TestDriver itkBresenhamLineTest)
itk_add_test(NAME itkSparseFieldLayerTest COMMAND ITKCommon1TestDriver itkSparseFieldLayerTest)
itk_add_test(NAME itkDataObjectTest COMMAND ITKCommon1TestDriver itkDataObjectTest)
itk_add_test(NAME itkAtanRegularizedHeavisideStepFunctionTest1
 COMMAND ITKCommon1TestDriver itkAtanRegularizedHeavisideStepFunctionTest1)
itk_add_test(NAME itkHashTableTest COMMAND ITKCommon1TestDriver itkHashTableTest)
itk_add_test(NAME itkHeavisideStepFunctionTest1
 COMMAND ITKCommon1TestDriver itkHeavisideStepFunctionTest1)
itk_add_test(NAME itkSinRegularizedHeavisideStepFunctionTest1
 COMMAND ITKCommon1TestDriver itkSinRegularizedHeavisideStepFunctionTest1)
itk_add_test(NAME itkPixelAccessTest COMMAND ITKCommon1TestDriver itkPixelAccessTest)
itk_add_test(NAME itkPointGeometryTest COMMAND ITKCommon1TestDriver itkPointGeometryTest)
itk_add_test(NAME itkStdStreamStateSaveTest COMMAND ITKCommon1TestDriver itkStdStreamStateSaveTest)

#
# This test should be enabled if you suspect that the memory leak detector
# is not working
if(0)
  itk_add_test(NAME itkMemoryLeakTest COMMAND ITKCommon2TestDriver itkMemoryLeakTest)
endif()

itk_add_test(NAME itkMetaDataDictionaryTest COMMAND ITKCommon2TestDriver itkMetaDataDictionaryTest)

itk_add_test(NAME itkMultiThreaderBaseTestPlatform
  COMMAND ITKCommon2TestDriver itkMultiThreaderBaseTest)
set_tests_properties(itkMultiThreaderBaseTestPlatform
  PROPERTIES ENVIRONMENT "ITK_GLOBAL_DEFAULT_THREADER=Platform")
itk_add_test(NAME itkMultiThreaderBaseTestPool
  COMMAND ITKCommon2TestDriver itkMultiThreaderBaseTest)
set_tests_properties(itkMultiThreaderBaseTestPool
  PROPERTIES ENVIRONMENT "ITK_GLOBAL_DEFAULT_THREADER=Pool")
itk_add_test(NAME itkMultiThreaderBaseTest3
  COMMAND ITKCommon2TestDriver itkMultiThreaderBaseTest 3) # test with 3 threads

itk_add_test(NAME itkMultiThreaderTypeFromEnvironmentTestPlatform
  COMMAND ITKCommon2TestDriver itkMultiThreaderTypeFromEnvironmentTest PlatFORM)
set_tests_properties(itkMultiThreaderTypeFromEnvironmentTestPlatform
  PROPERTIES ENVIRONMENT "ITK_GLOBAL_DEFAULT_THREADER=plaTform")

itk_add_test(NAME itkMultiThreaderTypeFromEnvironmentTestPool
  COMMAND ITKCommon2TestDriver itkMultiThreaderTypeFromEnvironmentTest PooL)
set_tests_properties(itkMultiThreaderTypeFromEnvironmentTestPool
  PROPERTIES ENVIRONMENT "ITK_GLOBAL_DEFAULT_THREADER=pOoL") # tests letter case too

if(Module_ITKTBB) # ITK_USE_TBB is not yet defined here
  itk_add_test(NAME itkMultiThreaderBaseTestTBB
    COMMAND ITKCommon2TestDriver itkMultiThreaderBaseTest)
  set_tests_properties(itkMultiThreaderBaseTestTBB
    PROPERTIES ENVIRONMENT "ITK_GLOBAL_DEFAULT_THREADER=TBB")

  itk_add_test(NAME itkMultiThreaderTypeFromEnvironmentTestTBB
    COMMAND ITKCommon2TestDriver itkMultiThreaderTypeFromEnvironmentTest TBB)
  set_tests_properties(itkMultiThreaderTypeFromEnvironmentTestTBB
    PROPERTIES ENVIRONMENT "ITK_GLOBAL_DEFAULT_THREADER=tbb") # tests letter case too

  itk_add_test(NAME itkMultiThreaderParallelizeArrayTestTBB
    COMMAND ITKCommon2TestDriver itkMultiThreaderParallelizeArrayTest)
  set_tests_properties(itkMultiThreaderParallelizeArrayTestTBB
    PROPERTIES ENVIRONMENT "ITK_GLOBAL_DEFAULT_THREADER=TBB")
endif()

itk_add_test(NAME itkMultiThreaderParallelizeArrayTestPlatform
  COMMAND ITKCommon2TestDriver itkMultiThreaderParallelizeArrayTest)
set_tests_properties(itkMultiThreaderParallelizeArrayTestPlatform
  PROPERTIES ENVIRONMENT "ITK_GLOBAL_DEFAULT_THREADER=Platform")
itk_add_test(NAME itkMultiThreaderParallelizeArrayTestPool
  COMMAND ITKCommon2TestDriver itkMultiThreaderParallelizeArrayTest)
set_tests_properties(itkMultiThreaderParallelizeArrayTestPool
  PROPERTIES ENVIRONMENT "ITK_GLOBAL_DEFAULT_THREADER=Pool")
itk_add_test(NAME itkMultiThreaderParallelizeArrayTest3
  COMMAND ITKCommon2TestDriver itkMultiThreaderParallelizeArrayTest 3) # test with 3 threads

#test deprecated ITK_USE_THREADPOOL environment variable
itk_add_test(NAME itkMultiThreaderTypeFromEnvironmentTestOldPool
  COMMAND ITKCommon2TestDriver itkMultiThreaderTypeFromEnvironmentTest Pool)
set_tests_properties(itkMultiThreaderTypeFromEnvironmentTestOldPool
  PROPERTIES ENVIRONMENT "ITK_USE_THREADPOOL=pOoL")

itk_add_test(NAME itkMultiThreaderTypeFromEnvironmentTestOldPool2
  COMMAND ITKCommon2TestDriver itkMultiThreaderTypeFromEnvironmentTest Pool)
set_tests_properties(itkMultiThreaderTypeFromEnvironmentTestOldPool2
  PROPERTIES ENVIRONMENT "ITK_USE_THREADPOOL=ON")

itk_add_test(NAME itkMultiThreaderTypeFromEnvironmentTestOldPlatform
  COMMAND ITKCommon2TestDriver itkMultiThreaderTypeFromEnvironmentTest Platform)
set_tests_properties(itkMultiThreaderTypeFromEnvironmentTestOldPlatform
  PROPERTIES ENVIRONMENT "ITK_USE_THREADPOOL=OFF")

itk_add_test(NAME itkMultiThreadingEnvTest88 COMMAND
  ITKCommon2TestDriver
    --remove-env "ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS"
    itkMultiThreadingEnvironmentTest 88)
set_tests_properties(itkMultiThreadingEnvTest88 PROPERTIES ENVIRONMENT "NSLOTS=88")

itk_add_test(NAME itkMultiThreadingEnvTest43 COMMAND ITKCommon2TestDriver itkMultiThreadingEnvironmentTest 43)
set_tests_properties(itkMultiThreadingEnvTest43 PROPERTIES ENVIRONMENT "ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS=43")

itk_add_test(NAME itkMultiThreadingEnvTest123 COMMAND
  ITKCommon2TestDriver
    --remove-env "ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS"
    itkMultiThreadingEnvironmentTest 123)
set_tests_properties(itkMultiThreadingEnvTest123 PROPERTIES ENVIRONMENT "NSLOTS=9;FIRST_IGNORED=13;LAST_RESPECTED=123;ITK_NUMBER_OF_THREADS_ENV_LIST=FIRST_IGNORED:LAST_RESPECTED")

itk_add_test(NAME itkNeighborhoodAlgorithmTest COMMAND ITKCommon1TestDriver itkNeighborhoodAlgorithmTest)
itk_add_test(NAME itkNeighborhoodTest COMMAND ITKCommon2TestDriver itkNeighborhoodTest)
itk_add_test(NAME itkNeighborhoodIteratorTest COMMAND ITKCommon2TestDriver itkNeighborhoodIteratorTest)
itk_add_test(NAME itkNeighborhoodOperatorTest COMMAND ITKCommon1TestDriver itkNeighborhoodOperatorTest)
itk_add_test(NAME itkNumericsTest COMMAND ITKCommon1TestDriver itkNumericsTest)
itk_add_test(NAME itkNumericTraitsTest COMMAND  ITKCommon1TestDriver --full-output itkNumericTraitsTest)
itk_add_test(NAME itkIntTypesTest COMMAND ITKCommon1TestDriver itkIntTypesTest)
itk_add_test(NAME itkOctreeTest COMMAND ITKCommon2TestDriver itkOctreeTest)
itk_add_test(NAME itkNumberToStringTest COMMAND ITKCommon1TestDriver itkNumberToStringTest)

if(ITK_BUILD_SHARED_LIBS)
  itk_add_test(NAME itkObjectFactoryTest2 COMMAND ITKCommon2TestDriver itkObjectFactoryTest2 ${ITK_TEST_OUTPUT_DIR} ${ITK_TEST_OUTPUT_DIR})

set_tests_properties(itkObjectFactoryTest2 PROPERTIES RUN_SERIAL 1)
endif()

itk_add_test(NAME itkObjectFactoryTest3 COMMAND ITKCommon2TestDriver itkObjectFactoryTest3)
itk_add_test(NAME itkPeriodicBoundaryConditionTest COMMAND ITKCommon2TestDriver itkPeriodicBoundaryConditionTest)
itk_add_test(NAME itkPhasedArray3DSpecialCoordinatesImageTest COMMAND ITKCommon1TestDriver itkPhasedArray3DSpecialCoordinatesImageTest)
itk_add_test(NAME itkPriorityQueueTest COMMAND ITKCommon1TestDriver itkPriorityQueueTest)
itk_add_test(NAME itkRealTimeClockTest COMMAND ITKCommon1TestDriver itkRealTimeClockTest)
itk_add_test(NAME itkRealTimeStampTest COMMAND ITKCommon1TestDriver itkRealTimeStampTest)
itk_add_test(NAME itkRealTimeIntervalTest COMMAND ITKCommon1TestDriver itkRealTimeIntervalTest)
itk_add_test(NAME itkTimeProbeTest COMMAND ITKCommon1TestDriver itkTimeProbeTest)
itk_add_test(NAME itkTimeProbeTest2 COMMAND ITKCommon1TestDriver itkTimeProbeTest2)
itk_add_test(NAME itkRGBPixelTest COMMAND ITKCommon1TestDriver itkRGBPixelTest)
itk_add_test(NAME itkSparseImageTest COMMAND ITKCommon1TestDriver itkSparseImageTest)
itk_add_test(NAME itkSimpleFilterWatcherTest COMMAND ITKCommon1TestDriver itkSimpleFilterWatcherTest)
itk_add_test(NAME itkSliceIteratorTest COMMAND ITKCommon2TestDriver --redirectOutput ${TEMP}/itkSliceIteratorTest.txt itkSliceIteratorTest)
set_tests_properties(itkSliceIteratorTest PROPERTIES ATTACHED_FILES_ON_FAIL ${TEMP}/itkSliceIteratorTest.txt)
itk_add_test(NAME itkSmartPointerTest COMMAND ITKCommon2TestDriver itkSmartPointerTest)
itk_add_test(NAME itkSpatialFunctionTest COMMAND ITKCommon2TestDriver itkSpatialFunctionTest)
itk_add_test(NAME itkSTLContainerAdaptorTest COMMAND ITKCommon2TestDriver itkSTLContainerAdaptorTest)
itk_add_test(NAME itkStdStreamLogOutputTest COMMAND ITKCommon2TestDriver itkStdStreamLogOutputTest ${TEMP}/testStreamLogOutput.txt)
itk_add_test(NAME itkThreadLoggerTest COMMAND ITKCommon2TestDriver itkThreadLoggerTest ${TEMP}/test_threadLogger.txt)
itk_add_test(NAME itkThreadDefsTest COMMAND ITKCommon2TestDriver itkThreadDefsTest)
itk_add_test(NAME itkTimeProbesTest COMMAND ITKCommon2TestDriver itkTimeProbesTest)
itk_add_test(NAME itkVersorTest COMMAND ITKCommon2TestDriver itkVersorTest)
itk_add_test(NAME itkConicShellInteriorExteriorSpatialFunctionTest COMMAND ITKCommon2TestDriver itkConicShellInteriorExteriorSpatialFunctionTest)
itk_add_test(NAME itkEllipsoidInteriorExteriorSpatialFunctionTest COMMAND ITKCommon2TestDriver itkEllipsoidInteriorExteriorSpatialFunctionTest)
itk_add_test(NAME itkFrustumSpatialFunctionTest COMMAND ITKCommon2TestDriver itkFrustumSpatialFunctionTest)
itk_add_test(NAME itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest COMMAND ITKCommon1TestDriver itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest)
itk_add_test(NAME itkTorusInteriorExteriorSpatialFunctionTest COMMAND ITKCommon2TestDriver itkTorusInteriorExteriorSpatialFunctionTest)
itk_add_test(NAME itkSymmetricSecondRankTensorImageReadTest COMMAND ITKCommon1TestDriver itkSymmetricSecondRankTensorImageReadTest
              ${ITK_TEST_OUTPUT_DIR}/testSymmetricTensor.nrrd)
itk_add_test(NAME itkSymmetricSecondRankTensorImageWriteReadTest COMMAND ITKCommon1TestDriver itkSymmetricSecondRankTensorImageWriteReadTest
              ${ITK_TEST_OUTPUT_DIR}/testSymmetricTensorWriteRead.mha)
itk_add_test(NAME itkSymmetricSecondRankTensorTest COMMAND ITKCommon2TestDriver itkSymmetricSecondRankTensorTest)
itk_add_test(NAME itkSymmetricEigenAnalysisTest COMMAND ITKCommon1TestDriver itkSymmetricEigenAnalysisTest)
itk_add_test(NAME itkSTLThreadTest COMMAND ITKCommon2TestDriver itkSTLThreadTest)
itk_add_test(NAME itkStreamingImageFilterTest COMMAND ITKCommon1TestDriver itkStreamingImageFilterTest)
itk_add_test(NAME itkStreamingImageFilterTest2 COMMAND ITKCommon1TestDriver itkStreamingImageFilterTest2)
itk_add_test(NAME itkStreamingImageFilterTest3_1 COMMAND ITKCommon1TestDriver
    --compare DATA{${ITK_DATA_ROOT}/Input/CellsFluorescence1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_1.png
    itkStreamingImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/CellsFluorescence1.png} ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_1.png 10)
itk_add_test(NAME itkStreamingImageFilterTest3_2
      COMMAND ITKCommon1TestDriver
    --compare DATA{${ITK_DATA_ROOT}/Input/CellsFluorescence1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_2.png
    itkStreamingImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/CellsFluorescence1.png} ${ITK_TEST_OUTPUT_DIR}/itkStreamingImageFilterTest3_2.png 1000)
itk_add_test(NAME itkVariableLengthVectorTest COMMAND ITKCommon2TestDriver itkVariableLengthVectorTest)
itk_add_test(NAME itkVariableSizeMatrixTest COMMAND ITKCommon2TestDriver itkVariableSizeMatrixTest)
#itk_add_test(NAME itkQuaternionOrientationAdapterTest COMMAND ITKCommon2TestDriver itkQuaternionOrientationAdapterTest)
itk_add_test(NAME itkVectorGeometryTest COMMAND ITKCommon2TestDriver itkVectorGeometryTest)
itk_add_test(NAME itkZeroFluxBoundaryConditionTest COMMAND ITKCommon2TestDriver itkZeroFluxBoundaryConditionTest)
itk_add_test(NAME itkConstantBoundaryConditionTest COMMAND ITKCommon2TestDriver itkConstantBoundaryConditionTest)
 itk_add_test(NAME itkVNLRoundProfileTest1 COMMAND ITKCommon2TestDriver itkVNLRoundProfileTest1)
 itk_add_test(NAME itkMemoryProbesCollecterBaseTest COMMAND ITKCommon2TestDriver
   itkMemoryProbesCollecterBaseTest)
if( "${ITK_COMPUTER_MEMORY_SIZE}" GREATER 4 )
  itk_add_test(NAME itkImageFillBufferTest4.1 COMMAND ITKCommon2TestDriver itkImageFillBufferTest 4.1)
  set_property(TEST itkImageFillBufferTest4.1 APPEND PROPERTY LABELS RUNS_LONG)
  set_tests_properties( itkImageFillBufferTest4.1 PROPERTIES RESOURCE_LOCK MEMORY_SIZE )
endif()

itk_add_test(NAME itkImageAlgorithmCopyTest COMMAND ITKCommon2TestDriver itkImageAlgorithmCopyTest )
itk_add_test(NAME itkImageAlgorithmCopyTest2 COMMAND ITKCommon2TestDriver itkImageAlgorithmCopyTest2 )
itk_add_test(NAME itkOptimizerParametersTest COMMAND ITKCommon2TestDriver itkOptimizerParametersTest)
itk_add_test(NAME itkImageVectorOptimizerParametersHelperTest COMMAND ITKCommon2TestDriver itkImageVectorOptimizerParametersHelperTest)
itk_add_test(NAME itkCompensatedSummationTest COMMAND ITKCommon2TestDriver itkCompensatedSummationTest)
itk_add_test(NAME itkCompensatedSummationTest2 COMMAND ITKCommon2TestDriver itkCompensatedSummationTest2)



#
# this test depends on being able to raise C++ exceptions from
# the signal handler. This is apparently not something that
# works properly
if("${CMAKE_SYSTEM_NAME}" MATCHES "Darwin|Linux")
  itk_add_test(NAME itkFloatingPointExceptionsTest1 COMMAND ITKCommon1TestDriver
    itkFloatingPointExceptionsTest DivByZero)
  itk_add_test(NAME itkFloatingPointExceptionsTest2 COMMAND ITKCommon1TestDriver
    itkFloatingPointExceptionsTest ZeroDivByZero)
  itk_add_test(NAME itkFloatingPointExceptionsTest3 COMMAND ITKCommon1TestDriver
    itkFloatingPointExceptionsTest FPOverFlow)
  itk_add_test(NAME itkFloatingPointExceptionsTest4 COMMAND ITKCommon1TestDriver
    itkFloatingPointExceptionsTest FPUnderFlow)

  set_tests_properties(
    itkFloatingPointExceptionsTest1
    itkFloatingPointExceptionsTest2
    itkFloatingPointExceptionsTest3
    itkFloatingPointExceptionsTest4
    PROPERTIES WILL_FAIL TRUE
    )

endif()

add_executable(itkCMakeConfigurationTest itkCMakeConfigurationTest.cxx)
itk_module_target_label(itkCMakeConfigurationTest)
itk_add_test(NAME itkCMakeConfigurationTest
         COMMAND itkCMakeConfigurationTest ${CMAKE_BINARY_DIR})
itk_add_test(NAME itkDataObjectAndProcessObjectTest COMMAND ITKCommon2TestDriver itkDataObjectAndProcessObjectTest)
itk_add_test(NAME itkImageRegionConstIteratorWithOnlyIndexTest COMMAND ITKCommon2TestDriver itkImageRegionConstIteratorWithOnlyIndexTest)
itk_add_test(NAME itkImageRandomConstIteratorWithOnlyIndexTest COMMAND ITKCommon2TestDriver itkImageRandomConstIteratorWithOnlyIndexTest)
itk_add_test(NAME itkConstNeighborhoodIteratorWithOnlyIndexTest COMMAND ITKCommon2TestDriver itkConstNeighborhoodIteratorWithOnlyIndexTest)

itk_add_test(NAME itkImageToImageToleranceTest COMMAND
  ITKCommon2TestDriver itkImageTOImageToleranceTest)


itk_add_test(NAME itkRegionSplitterSlowDimensionTest COMMAND ITKCommon2TestDriver itkImageRegionSplitterSlowDimensionTest)
itk_add_test(NAME itkRegionSplitterDirectionTest COMMAND ITKCommon2TestDriver itkImageRegionSplitterDirectionTest)
itk_add_test(NAME itkRegionSplitterMultidimensionalTest COMMAND ITKCommon2TestDriver itkImageRegionSplitterMultidimensionalTest)

itk_add_test(NAME itkMetaDataObjectTest COMMAND ITKCommon2TestDriver itkMetaDataObjectTest)

itk_add_test(NAME itkMultithreadingTest COMMAND ITKCommon2TestDriver itkMultithreadingTest 100)

if(ITK_BUILD_SHARED_LIBS AND ITK_DYNAMIC_LOADING)
  macro(BuildClientTestLibrary _name _type)
    add_library(ClientTestLibrary${_name} ${_type} ClientTestLibrary${_name}.cxx)
    itk_module_target_label(ClientTestLibrary${_name})
    target_link_libraries(ClientTestLibrary${_name} LINK_PUBLIC ${ITKCommon_LIBRARIES})
    set_property(TARGET ClientTestLibrary${_name} PROPERTY LIBRARY_OUTPUT_DIRECTORY ${ITK_TEST_OUTPUT_DIR})
    generate_export_header(ClientTestLibrary${_name}
      EXPORT_FILE_NAME ${ITKCommon_BINARY_DIR}/ClientTestLibrary${_name}Export.h
      EXPORT_MACRO_NAME ClientTestLibrary${_name}_EXPORT
      NO_EXPORT_MACRO_NAME ClientTestLibrary${_name}_HIDDEN
      )
    set_target_properties(ClientTestLibrary${_name} PROPERTIES CXX_VISIBILITY_PRESET hidden)
    set_target_properties(ClientTestLibrary${_name} PROPERTIES C_VISIBILITY_PRESET hidden)
    set_target_properties(ClientTestLibrary${_name} PROPERTIES VISIBILITY_INLINES_HIDDEN 1)
  endmacro()
  BuildClientTestLibrary(A SHARED)
  BuildClientTestLibrary(B MODULE)
  BuildClientTestLibrary(C SHARED)
  target_link_libraries(ITKCommon2TestDriver LINK_PUBLIC ClientTestLibraryA)
  target_link_libraries(ITKCommon2TestDriver LINK_PUBLIC ClientTestLibraryC)
  itk_add_test(NAME itkDownCastTest COMMAND ITKCommon2TestDriver itkDownCastTest
    $<TARGET_FILE:ClientTestLibraryB>)
endif()

set(ITKCommonGTests
      itkAggregateTypesGTest.cxx
      itkBuildInformationGTest.cxx
      itkConnectedImageNeighborhoodShapeGTest.cxx
      itkConstantBoundaryImageNeighborhoodPixelAccessPolicyGTest.cxx
      itkFixedArrayGTest.cxx
      itkImageNeighborhoodOffsetsGTest.cxx
      itkImageBaseGTest.cxx
      itkImageBufferRangeGTest.cxx
      itkImageRegionRangeGTest.cxx
      itkImageIORegionGTest.cxx
      itkIndexGTest.cxx
      itkIndexRangeGTest.cxx
      itkMersenneTwisterRandomVariateGeneratorGTest.cxx
      itkNeighborhoodAllocatorGTest.cxx
      itkPointGTest.cxx
      itkShapedImageNeighborhoodRangeGTest.cxx
      itkSizeGTest.cxx
      itkSmartPointerGTest.cxx
      itkVectorContainerGTest.cxx
      itkCommonTypeTraitsGTest.cxx
      itkMetaDataDictionaryGTest.cxx
)
CreateGoogleTestDriver(ITKCommon "${ITKCommon-Test_LIBRARIES}" "${ITKCommonGTests}")
# If `-static` was passed to CMAKE_EXE_LINKER_FLAGS, compilation fails. No need to
# test this case.
if(NOT ITK_BUILD_SHARED_LIBS AND NOT CMAKE_EXE_LINKER_FLAGS MATCHES ".*-static.*")
  macro(BuildSharedTestLibrary _name _type)
    add_library(SharedTestLibrary${_name} ${_type} SharedTestLibrary${_name}.cxx)
    itk_module_target_label(SharedTestLibrary${_name})
    target_link_libraries(SharedTestLibrary${_name} LINK_PUBLIC ${ITKCommon_LIBRARIES})
    set_property(TARGET SharedTestLibrary${_name} PROPERTY LIBRARY_OUTPUT_DIRECTORY ${ITK_TEST_OUTPUT_DIR})
      generate_export_header(SharedTestLibrary${_name}
      EXPORT_FILE_NAME ${ITKCommon_BINARY_DIR}/SharedTestLibrary${_name}Export.h
      EXPORT_MACRO_NAME SharedTestLibrary${_name}_EXPORT
      NO_EXPORT_MACRO_NAME SharedTestLibrary${_name}_HIDDEN
      )
  endmacro()

  # Linking static ITK libraries twice, into two different binaries for the same process, is not recommended or supported.
  BuildSharedTestLibrary(A SHARED)
  BuildSharedTestLibrary(B SHARED)
  add_executable(itkObjectFactoryBasePrivateDestructor itkObjectFactoryBasePrivateDestructor.cxx )
  itk_module_target_label(itkObjectFactoryBasePrivateDestructor)
  target_link_libraries(itkObjectFactoryBasePrivateDestructor
      LINK_PUBLIC
      SharedTestLibraryA
      SharedTestLibraryB)
  itk_add_test(NAME itkObjectFactoryBasePrivateDestructor COMMAND itkObjectFactoryBasePrivateDestructor)
endif()
