itk_module_test()

create_test_sourcelist(TESTS1 ITK-CommonTestDriver1.cxx
itkAdaptorComparisonTest.cxx
itkCellInterfaceTest.cxx
itkCovariantVectorGeometryTest.cxx
itkDataTypeTest.cxx
itkDecoratorTest.cxx
itkExtractImage3Dto2DTest.cxx
itkExtractImageTest.cxx
itkFilterDispatchTest.cxx
itkMapContainerTest.cxx
itkImageReverseIteratorTest.cxx
itkTreeContainerTest2.cxx
itkTriangleCellTest.cxx
itkRGBPixelTest.cxx
itkLightObjectTest.cxx
itkBoundingBoxTest.cxx
itkBoundaryConditionTest.cxx
itkByteSwapTest.cxx
itkScalarToRGBPixelFunctorTest.cxx
itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest.cxx
itkArray2DTest.cxx
itkFloatingPointExceptionsTest.cxx
itkFixedArrayTest2.cxx
itkNeighborhoodAlgorithmTest.cxx
itkPhasedArray3DSpecialCoordinatesImageTest.cxx
itkFileOutputWindowTest.cxx
itkSymmetricEigenAnalysisTest.cxx
itkLoggerTest.cxx
itkDerivativeOperatorTest.cxx
itkColorTableTest.cxx
itkNumericTraitsTest.cxx
itkImageRegionTest.cxx
itkExceptionObjectTest.cxx
itkNeighborhoodOperatorTest.cxx
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
itkHeavisideStepFunctionTest1.cxx
itkSinRegularizedHeavisideStepFunctionTest1.cxx
)



create_test_sourcelist(TESTS2 ITK-CommonTestDriver2.cxx
itkMetaDataDictionaryTest.cxx
itkStdStreamLogOutputTest.cxx
itkOctreeTest.cxx
itkThreadLoggerTest.cxx
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
)


set(TestsToRun ${TESTS1})
list(REMOVE_ITEM TestsToRun ITK-CommonTestDriver1.cxx)
add_executable(ITK-CommonTestDriver1  ${TESTS1} )
itk_module_target_label(ITK-CommonTestDriver1)
target_link_libraries(ITK-CommonTestDriver1 ${ITK-Common_LIBRARIES})

set(TestsToRun ${TESTS2})
list(REMOVE_ITEM TestsToRun ITK-CommonTests2.cxx)
add_executable(ITK-CommonTestDriver2  ${TESTS2} )
itk_module_target_label(ITK-CommonTestDriver2)
target_link_libraries(ITK-CommonTestDriver2 ${ITK-Common_LIBRARIES})

#-----------------------------------------------------------------------------

set(BASELINE "${ITK_DATA_ROOT}/Baseline/Common")

set(TEMP ${ITK_TEST_OUTPUT_DIR})

#-----------------------------------------------------------------------------
add_executable( itkMathTest itkMathTest.cxx )
itk_module_target_label(itkMathTest)
itk_add_test(NAME itkMathTest COMMAND itkMathTest)
target_link_libraries(itkMathTest ${ITK-Common_LIBRARIES})

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
  add_dependencies(ITK-CommonTestDriver2 FactoryTestLib)
endif(ITK_BUILD_SHARED_LIBS)

itk_add_test(NAME itkCommonHeaderTest COMMAND ITK-CommonTestDriver1 itkCommonHeaderTest)
itk_add_test(NAME itkAdaptorComparisonTest COMMAND ITK-CommonTestDriver1 itkAdaptorComparisonTest)
itk_add_test(NAME itkCellInterfaceTest COMMAND ITK-CommonTestDriver1 itkCellInterfaceTest)
itk_add_test(NAME itkCovariantVectorGeometryTest COMMAND ITK-CommonTestDriver1 itkCovariantVectorGeometryTest)
itk_add_test(NAME itkDataTypeTest COMMAND ITK-CommonTestDriver1 itkDataTypeTest)
itk_add_test(NAME itkDecoratorTest COMMAND ITK-CommonTestDriver1  itkDecoratorTest)
itk_add_test(NAME itkExtractImage3Dto2DTest COMMAND ITK-CommonTestDriver1  itkExtractImage3Dto2DTest)
itk_add_test(NAME itkExtractImageTest COMMAND ITK-CommonTestDriver1 itkExtractImageTest)
itk_add_test(NAME itkFilterDispatchTest COMMAND ITK-CommonTestDriver1 itkFilterDispatchTest)
itk_add_test(NAME itkAnnulusOperatorTest COMMAND ITK-CommonTestDriver1 itkAnnulusOperatorTest)
itk_add_test(NAME itkColorTableTest COMMAND ITK-CommonTestDriver1 itkColorTableTest)
itk_add_test(NAME itkDirectoryTest COMMAND ITK-CommonTestDriver1 itkDirectoryTest ${TEMP})
itk_add_test(NAME itkDerivativeOperatorTest COMMAND ITK-CommonTestDriver1 itkDerivativeOperatorTest)
itk_add_test(NAME itkMultipleLogOutputTest COMMAND ITK-CommonTestDriver1 itkMultipleLogOutputTest ${TEMP}/test_multi.txt)
itk_add_test(NAME itkFixedArrayTest COMMAND ITK-CommonTestDriver2 itkFixedArrayTest)
itk_add_test(NAME itkImageTransformTest COMMAND ITK-CommonTestDriver2 itkImageTransformTest)
itk_add_test(NAME itkMinimumMaximumImageCalculatorTest COMMAND ITK-CommonTestDriver2 itkMinimumMaximumImageCalculatorTest)
itk_add_test(NAME itkFixedArrayTest2 COMMAND ITK-CommonTestDriver1 itkFixedArrayTest2)
itk_add_test(NAME itkArrayTest COMMAND ITK-CommonTestDriver1 itkArrayTest)
itk_add_test(NAME itkArray2DTest COMMAND ITK-CommonTestDriver1 itkArray2DTest)
itk_add_test(NAME itkAutoPointerTest COMMAND ITK-CommonTestDriver2 itkAutoPointerTest)
if(NOT MINGW)
itk_add_test(NAME itkBarrierTest COMMAND ITK-CommonTestDriver2 itkBarrierTest)
itk_add_test(NAME itkConditionVariableTest COMMAND ITK-CommonTestDriver1 itkConditionVariableTest)
endif(NOT MINGW)
itk_add_test(NAME itkTimeStampTest COMMAND ITK-CommonTestDriver2 itkTimeStampTest)
itk_add_test(NAME itkBoundingBoxTest COMMAND ITK-CommonTestDriver1 itkBoundingBoxTest)
itk_add_test(NAME itkBoundaryConditionTest COMMAND ITK-CommonTestDriver1 itkBoundaryConditionTest)
itk_add_test(NAME itkByteSwapTest COMMAND ITK-CommonTestDriver1 itkByteSwapTest)
itk_add_test(NAME itkBSplineInterpolationWeightFunctionTest COMMAND ITK-CommonTestDriver2 itkBSplineInterpolationWeightFunctionTest)
itk_add_test(NAME itkBSplineKernelFunctionTest COMMAND ITK-CommonTestDriver1 itkBSplineKernelFunctionTest)
itk_add_test(NAME itkConstNeighborhoodIteratorTest COMMAND ITK-CommonTestDriver2 itkConstNeighborhoodIteratorTest)
itk_add_test(NAME itkShapedNeighborhoodIteratorTest COMMAND ITK-CommonTestDriver2 itkShapedNeighborhoodIteratorTest)
itk_add_test(NAME itkConstShapedNeighborhoodIteratorTest COMMAND ITK-CommonTestDriver2 itkConstShapedNeighborhoodIteratorTest)
itk_add_test(NAME itkConstShapedNeighborhoodIteratorTest2 COMMAND ITK-CommonTestDriver2 itkConstShapedNeighborhoodIteratorTest2)
itk_add_test(NAME itkEventObjectTest COMMAND ITK-CommonTestDriver1 itkEventObjectTest)
itk_add_test(NAME itkExceptionObjectTest COMMAND ITK-CommonTestDriver1 itkExceptionObjectTest)
itk_add_test(NAME itkFileOutputWindowTest COMMAND ITK-CommonTestDriver1 itkFileOutputWindowTest)
itk_add_test(NAME itkFiniteCylinderSpatialFunctionTest COMMAND ITK-CommonTestDriver2 itkFiniteCylinderSpatialFunctionTest)
itk_add_test(NAME itkGaussianSpatialFunctionTest COMMAND ITK-CommonTestDriver1 itkGaussianSpatialFunctionTest)
itk_add_test(NAME itkCrossHelperTest COMMAND ITK-CommonTestDriver1 itkCrossHelperTest)
itk_add_test(NAME itkImageIteratorTest COMMAND ITK-CommonTestDriver1 itkImageIteratorTest)
itk_add_test(NAME itkImageIteratorWithIndexTest COMMAND ITK-CommonTestDriver1 itkImageIteratorWithIndexTest)
itk_add_test(NAME itkImageRegionIteratorTest COMMAND ITK-CommonTestDriver1 itkImageRegionIteratorTest)
itk_add_test(NAME itkImageRegionTest COMMAND ITK-CommonTestDriver1 itkImageRegionTest)
itk_add_test(NAME itkImageRegionExclusionIteratorWithIndexTest COMMAND ITK-CommonTestDriver2 itkImageRegionExclusionIteratorWithIndexTest)
itk_add_test(NAME itkImageReverseIteratorTest COMMAND ITK-CommonTestDriver1 itkImageReverseIteratorTest)
itk_add_test(NAME itkImageTest COMMAND ITK-CommonTestDriver1 itkImageTest)
itk_add_test(NAME itkLightObjectTest COMMAND ITK-CommonTestDriver1 itkLightObjectTest)
itk_add_test(NAME itkLineIteratorTest COMMAND ITK-CommonTestDriver1 itkLineIteratorTest ${BASELINE}/itkLineIteratorTest.txt )
itk_add_test(NAME itkLoggerTest COMMAND ITK-CommonTestDriver1 itkLoggerTest ${TEMP}/test_logger.txt)
itk_add_test(NAME itkLoggerOutputTest COMMAND ITK-CommonTestDriver2 itkLoggerOutputTest ${TEMP}/test_loggerOutput.txt)
itk_add_test(NAME itkLoggerManagerTest COMMAND ITK-CommonTestDriver2 itkLoggerManagerTest ${TEMP}/test_LoggerManager.txt)
itk_add_test(NAME itkMatrixTest COMMAND ITK-CommonTestDriver2 itkMatrixTest)
itk_add_test(NAME itkMapContainerTest COMMAND ITK-CommonTestDriver1 itkMapContainerTest)
itk_add_test(NAME itkObjectFactoryTest COMMAND ITK-CommonTestDriver1 itkObjectFactoryTest)
itk_add_test(NAME itkVectorTest COMMAND ITK-CommonTestDriver1 itkVectorTest)
itk_add_test(NAME itkPointSetTest COMMAND ITK-CommonTestDriver1 itkPointSetTest)
itk_add_test(NAME itkBresenhamLineTest COMMAND ITK-CommonTestDriver1 itkBresenhamLineTest)
itk_add_test(NAME itkSparseFieldLayerTest COMMAND ITK-CommonTestDriver1 itkSparseFieldLayerTest)
itk_add_test(NAME itkDataObjectTest COMMAND ITK-CommonTestDriver1 itkDataObjectTest)
itk_add_test(NAME itkAtanRegularizedHeavisideStepFunctionTest1
 COMMAND ITK-CommonTestDriver1 itkAtanRegularizedHeavisideStepFunctionTest1)
itk_add_test(NAME itkHeavisideStepFunctionTest1
 COMMAND ITK-CommonTestDriver1 itkHeavisideStepFunctionTest1)
itk_add_test(NAME itkSinRegularizedHeavisideStepFunctionTest1
 COMMAND ITK-CommonTestDriver1 itkSinRegularizedHeavisideStepFunctionTest1)

#
# This test should be enabled if you suspect that the memory leak detector
# is not working
if(0)
  itk_add_test(NAME itkMemoryLeakTest COMMAND ITK-CommonTestDriver2 itkMemoryLeakTest)
endif(0)

itk_add_test(NAME itkMetaDataDictionaryTest COMMAND ITK-CommonTestDriver2 itkMetaDataDictionaryTest)
itk_add_test(NAME itkMultiThreaderTest COMMAND ITK-CommonTestDriver2 itkMultiThreaderTest)
itk_add_test(NAME itkNeighborhoodAlgorithmTest COMMAND ITK-CommonTestDriver1 itkNeighborhoodAlgorithmTest)
itk_add_test(NAME itkNeighborhoodTest COMMAND ITK-CommonTestDriver2 itkNeighborhoodTest)
itk_add_test(NAME itkNeighborhoodIteratorTest COMMAND ITK-CommonTestDriver2 itkNeighborhoodIteratorTest)
itk_add_test(NAME itkNeighborhoodOperatorTest COMMAND ITK-CommonTestDriver1 itkNeighborhoodOperatorTest)
itk_add_test(NAME itkNumericTraitsTest COMMAND ITK-CommonTestDriver1 itkNumericTraitsTest)
itk_add_test(NAME itkIntTypesTest COMMAND ITK-CommonTestDriver1 itkIntTypesTest)
itk_add_test(NAME itkOctreeTest COMMAND ITK-CommonTestDriver2 itkOctreeTest)


if(ITK_BUILD_SHARED_LIBS)
itk_add_test(NAME itkObjectFactoryTest2 COMMAND ITK-CommonTestDriver2 itkObjectFactoryTest2 ${ITK_TEST_OUTPUT_DIR})
set_tests_properties(itkObjectFactoryTest2 PROPERTIES RUN_SERIAL 1)
endif(ITK_BUILD_SHARED_LIBS)

itk_add_test(NAME itkObjectFactoryTest3 COMMAND ITK-CommonTestDriver2 itkObjectFactoryTest3)
itk_add_test(NAME itkPeriodicBoundaryConditionTest COMMAND ITK-CommonTestDriver2 itkPeriodicBoundaryConditionTest)
itk_add_test(NAME itkPhasedArray3DSpecialCoordinatesImageTest COMMAND ITK-CommonTestDriver1 itkPhasedArray3DSpecialCoordinatesImageTest)
itk_add_test(NAME itkRealTimeClockTest COMMAND ITK-CommonTestDriver1 itkRealTimeClockTest)
itk_add_test(NAME itkRealTimeStampTest COMMAND ITK-CommonTestDriver1 itkRealTimeStampTest)
itk_add_test(NAME itkRealTimeIntervalTest COMMAND ITK-CommonTestDriver1 itkRealTimeIntervalTest)
itk_add_test(NAME itkTriangleCellTest COMMAND ITK-CommonTestDriver1 itkTriangleCellTest)
itk_add_test(NAME itkRGBPixelTest COMMAND ITK-CommonTestDriver1 itkRGBPixelTest)
itk_add_test(NAME itkScalarToRGBPixelFunctorTest COMMAND ITK-CommonTestDriver1 itkScalarToRGBPixelFunctorTest)
itk_add_test(NAME itkSizeTest COMMAND ITK-CommonTestDriver2 itkSizeTest)
itk_add_test(NAME itkSliceIteratorTest COMMAND ITK-CommonTestDriver2 itkSliceIteratorTest)
itk_add_test(NAME itkSmartPointerTest COMMAND ITK-CommonTestDriver2 itkSmartPointerTest)
itk_add_test(NAME itkSpatialFunctionTest COMMAND ITK-CommonTestDriver2 itkSpatialFunctionTest)
itk_add_test(NAME itkSTLContainerAdaptorTest COMMAND ITK-CommonTestDriver2 itkSTLContainerAdaptorTest)
itk_add_test(NAME itkStdStreamLogOutputTest COMMAND ITK-CommonTestDriver2 itkStdStreamLogOutputTest ${TEMP}/testStreamLogOutput.txt)
itk_add_test(NAME itkThreadLoggerTest COMMAND ITK-CommonTestDriver2 itkThreadLoggerTest ${TEMP}/test_threadLogger.txt)
itk_add_test(NAME itkTreeContainerTest COMMAND ITK-CommonTestDriver2 itkTreeContainerTest)
itk_add_test(NAME itkTreeContainerTest2 COMMAND ITK-CommonTestDriver1 itkTreeContainerTest2)
itk_add_test(NAME itkVersorTest COMMAND ITK-CommonTestDriver2 itkVersorTest)
itk_add_test(NAME itkEllipsoidInteriorExteriorSpatialFunctionTest COMMAND ITK-CommonTestDriver2 itkEllipsoidInteriorExteriorSpatialFunctionTest)
itk_add_test(NAME itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest COMMAND ITK-CommonTestDriver1 itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest)
itk_add_test(NAME itkSymmetricSecondRankTensorTest COMMAND ITK-CommonTestDriver2 itkSymmetricSecondRankTensorTest)
itk_add_test(NAME itkSymmetricEigenAnalysisTest COMMAND ITK-CommonTestDriver1 itkSymmetricEigenAnalysisTest)
itk_add_test(NAME itkVariableLengthVectorTest COMMAND ITK-CommonTestDriver2 itkVariableLengthVectorTest)
itk_add_test(NAME itkVariableSizeMatrixTest COMMAND ITK-CommonTestDriver2 itkVariableSizeMatrixTest)
#itk_add_test(NAME itkQuaternionOrientationAdapterTest COMMAND ITK-CommonTestDriver2 itkQuaternionOrientationAdapterTest)

if( "${ITK_COMPUTER_MEMORY_SIZE}" GREATER 4 )
  itk_add_test(NAME itkImageFillBufferTest4.1 COMMAND ITK-CommonTestDriver2 itkImageFillBufferTest 4.1)
endif()

#
# this test depends on being able to raise C++ exceptions from
# the signal handler. This is apparently not something that
# works properly
if("${CMAKE_SYSTEM_NAME}" MATCHES "Linux")
  itk_add_test(NAME itkFloatingPointExceptionsTest1 COMMAND ITK-CommonTestDriver1
    itkFloatingPointExceptionsTest DivByZero)
  itk_add_test(NAME itkFloatingPointExceptionsTest2 COMMAND ITK-CommonTestDriver1
    itkFloatingPointExceptionsTest ZeroDivByZero)
  itk_add_test(NAME itkFloatingPointExceptionsTest3 COMMAND ITK-CommonTestDriver1
    itkFloatingPointExceptionsTest FPOverFlow)
  itk_add_test(NAME itkFloatingPointExceptionsTest4 COMMAND ITK-CommonTestDriver1
    itkFloatingPointExceptionsTest FPUnderFlow)
  itk_add_test(NAME itkFloatingPointExceptionsTest5 COMMAND ITK-CommonTestDriver1
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
