itk_module_test()

create_test_sourcelist(TESTS1 ITK-CommonTestDriver1.cxx
itkMapContainerTest.cxx
itkImageReverseIteratorTest.cxx
itkTreeContainerTest2.cxx
itkRGBPixelTest.cxx
itkLightObjectTest.cxx
itkBoundingBoxTest.cxx
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
target_link_libraries(ITK-CommonTestDriver1 ${ITK-Common_LIBRARIES})

set(TestsToRun ${TESTS2})
list(REMOVE_ITEM TestsToRun ITK-CommonTests2.cxx)
add_executable(ITK-CommonTestDriver2  ${TESTS2} )
target_link_libraries(ITK-CommonTestDriver2 ${ITK-Common_LIBRARIES})

#-----------------------------------------------------------------------------

set(BASELINE "${ITK_DATA_ROOT}/Baseline/Common")

set(TEMP ${ITK_TEST_OUTPUT_DIR})

#-----------------------------------------------------------------------------
add_executable( itkMathTest itkMathTest.cxx )
add_test(NAME itkMathTest COMMAND itkMathTest)
target_link_libraries(itkMathTest ${ITK-Common_LIBRARIES})

add_executable(itkSystemInformation itkSystemInformation.cxx)
target_link_libraries(itkSystemInformation ${ITK-Common_LIBRARIES})
add_test(NAME SystemInformation COMMAND itkSystemInformation)


if(ITK_OBJCXX_COMPILER_WORKS)
  add_executable(itkCommonHeaderObjCxxTest itkCommonHeaderObjCxxTest.mm)
  target_link_libraries(itkCommonHeaderObjCxxTest ${ITK-Common_LIBRARIES})
  add_test(NAME itkCommonHeaderObjCxxTest COMMAND itkCommonHeaderObjCxxTest)
endif(ITK_OBJCXX_COMPILER_WORKS)


if(ITK_BUILD_SHARED_LIBS)
  ## Create a library to test ITK loadable factories
  set(FactoryTest_Source itkFactoryTestLib.cxx)
  add_library(FactoryTestLib MODULE ${FactoryTest_Source})
  target_link_libraries(FactoryTestLib ${ITK-Common_LIBRARIES})
  set_property(TARGET FactoryTestLib PROPERTY LIBRARY_OUTPUT_DIRECTORY ${ITK_TEST_OUTPUT_DIR})
  add_dependencies(ITK-CommonTestDriver2 FactoryTestLib)
endif(ITK_BUILD_SHARED_LIBS)

add_test(NAME itkCommonHeaderTest COMMAND ITK-CommonTestDriver1 itkCommonHeaderTest)
add_test(NAME itkAnnulusOperatorTest COMMAND ITK-CommonTestDriver1 itkAnnulusOperatorTest)
add_test(NAME itkColorTableTest COMMAND ITK-CommonTestDriver1 itkColorTableTest)
add_test(NAME itkDirectoryTest COMMAND ITK-CommonTestDriver1 itkDirectoryTest ${TEMP})
add_test(NAME itkDerivativeOperatorTest COMMAND ITK-CommonTestDriver1 itkDerivativeOperatorTest)
add_test(NAME itkMultipleLogOutputTest COMMAND ITK-CommonTestDriver1 itkMultipleLogOutputTest ${TEMP}/test_multi.txt)
add_test(NAME itkFixedArrayTest COMMAND ITK-CommonTestDriver2 itkFixedArrayTest)
add_test(NAME itkImageTransformTest COMMAND ITK-CommonTestDriver2 itkImageTransformTest)
add_test(NAME itkMinimumMaximumImageCalculatorTest COMMAND ITK-CommonTestDriver2 itkMinimumMaximumImageCalculatorTest)
add_test(NAME itkFixedArrayTest2 COMMAND ITK-CommonTestDriver1 itkFixedArrayTest2)
add_test(NAME itkArrayTest COMMAND ITK-CommonTestDriver1 itkArrayTest)
add_test(NAME itkArray2DTest COMMAND ITK-CommonTestDriver1 itkArray2DTest)
add_test(NAME itkAutoPointerTest COMMAND ITK-CommonTestDriver2 itkAutoPointerTest)
if(NOT MINGW)
add_test(NAME itkBarrierTest COMMAND ITK-CommonTestDriver2 itkBarrierTest)
add_test(NAME itkConditionVariableTest COMMAND ITK-CommonTestDriver1 itkConditionVariableTest)
endif(NOT MINGW)
add_test(NAME itkTimeStampTest COMMAND ITK-CommonTestDriver2 itkTimeStampTest)
add_test(NAME itkBoundingBoxTest COMMAND ITK-CommonTestDriver1 itkBoundingBoxTest)
add_test(NAME itkBSplineInterpolationWeightFunctionTest COMMAND ITK-CommonTestDriver2 itkBSplineInterpolationWeightFunctionTest)
add_test(NAME itkBSplineKernelFunctionTest COMMAND ITK-CommonTestDriver1 itkBSplineKernelFunctionTest)
add_test(NAME itkConstNeighborhoodIteratorTest COMMAND ITK-CommonTestDriver2 itkConstNeighborhoodIteratorTest)
add_test(NAME itkShapedNeighborhoodIteratorTest COMMAND ITK-CommonTestDriver2 itkShapedNeighborhoodIteratorTest)
add_test(NAME itkConstShapedNeighborhoodIteratorTest COMMAND ITK-CommonTestDriver2 itkConstShapedNeighborhoodIteratorTest)
add_test(NAME itkConstShapedNeighborhoodIteratorTest2 COMMAND ITK-CommonTestDriver2 itkConstShapedNeighborhoodIteratorTest2)
add_test(NAME itkEventObjectTest COMMAND ITK-CommonTestDriver1 itkEventObjectTest)
add_test(NAME itkExceptionObjectTest COMMAND ITK-CommonTestDriver1 itkExceptionObjectTest)
add_test(NAME itkFileOutputWindowTest COMMAND ITK-CommonTestDriver1 itkFileOutputWindowTest)
add_test(NAME itkFiniteCylinderSpatialFunctionTest COMMAND ITK-CommonTestDriver2 itkFiniteCylinderSpatialFunctionTest)
add_test(NAME itkGaussianSpatialFunctionTest COMMAND ITK-CommonTestDriver1 itkGaussianSpatialFunctionTest)
add_test(NAME itkCrossHelperTest COMMAND ITK-CommonTestDriver1 itkCrossHelperTest)
add_test(NAME itkImageIteratorTest COMMAND ITK-CommonTestDriver1 itkImageIteratorTest)
add_test(NAME itkImageIteratorWithIndexTest COMMAND ITK-CommonTestDriver1 itkImageIteratorWithIndexTest)
add_test(NAME itkImageRegionIteratorTest COMMAND ITK-CommonTestDriver1 itkImageRegionIteratorTest)
add_test(NAME itkImageRegionTest COMMAND ITK-CommonTestDriver1 itkImageRegionTest)
add_test(NAME itkImageRegionExclusionIteratorWithIndexTest COMMAND ITK-CommonTestDriver2 itkImageRegionExclusionIteratorWithIndexTest)
add_test(NAME itkImageReverseIteratorTest COMMAND ITK-CommonTestDriver1 itkImageReverseIteratorTest)
add_test(NAME itkImageTest COMMAND ITK-CommonTestDriver1 itkImageTest)
add_test(NAME itkLightObjectTest COMMAND ITK-CommonTestDriver1 itkLightObjectTest)
add_test(NAME itkLineIteratorTest COMMAND ITK-CommonTestDriver1 itkLineIteratorTest ${BASELINE}/itkLineIteratorTest.txt )
add_test(NAME itkLoggerTest COMMAND ITK-CommonTestDriver1 itkLoggerTest ${TEMP}/test_logger.txt)
add_test(NAME itkLoggerOutputTest COMMAND ITK-CommonTestDriver2 itkLoggerOutputTest ${TEMP}/test_loggerOutput.txt)
add_test(NAME itkLoggerManagerTest COMMAND ITK-CommonTestDriver2 itkLoggerManagerTest ${TEMP}/test_LoggerManager.txt)
add_test(NAME itkMatrixTest COMMAND ITK-CommonTestDriver2 itkMatrixTest)
add_test(NAME itkMapContainerTest COMMAND ITK-CommonTestDriver1 itkMapContainerTest)
add_test(NAME itkObjectFactoryTest COMMAND ITK-CommonTestDriver1 itkObjectFactoryTest)
add_test(NAME itkVectorTest COMMAND ITK-CommonTestDriver1 itkVectorTest)
add_test(NAME itkPointSetTest COMMAND ITK-CommonTestDriver1 itkPointSetTest)
add_test(NAME itkBresenhamLineTest COMMAND ITK-CommonTestDriver1 itkBresenhamLineTest)
add_test(NAME itkSparseFieldLayerTest COMMAND ITK-CommonTestDriver1 itkSparseFieldLayerTest)
add_test(NAME itkDataObjectTest COMMAND ITK-CommonTestDriver1 itkDataObjectTest)
#
# This test should be enabled if you suspect that the memory leak detector
# is not working
if(0)
  add_test(NAME itkMemoryLeakTest COMMAND ITK-CommonTestDriver2 itkMemoryLeakTest)
endif(0)

add_test(NAME itkMetaDataDictionaryTest COMMAND ITK-CommonTestDriver2 itkMetaDataDictionaryTest)
add_test(NAME itkMultiThreaderTest COMMAND ITK-CommonTestDriver2 itkMultiThreaderTest)
add_test(NAME itkNeighborhoodAlgorithmTest COMMAND ITK-CommonTestDriver1 itkNeighborhoodAlgorithmTest)
add_test(NAME itkNeighborhoodTest COMMAND ITK-CommonTestDriver2 itkNeighborhoodTest)
add_test(NAME itkNeighborhoodIteratorTest COMMAND ITK-CommonTestDriver2 itkNeighborhoodIteratorTest)
add_test(NAME itkNeighborhoodOperatorTest COMMAND ITK-CommonTestDriver1 itkNeighborhoodOperatorTest)
add_test(NAME itkNumericTraitsTest COMMAND ITK-CommonTestDriver1 itkNumericTraitsTest)
add_test(NAME itkIntTypesTest COMMAND ITK-CommonTestDriver1 itkIntTypesTest)
add_test(NAME itkOctreeTest COMMAND ITK-CommonTestDriver2 itkOctreeTest)


if(ITK_BUILD_SHARED_LIBS)
add_test(NAME itkObjectFactoryTest2 COMMAND ITK-CommonTestDriver2 itkObjectFactoryTest2 ${ITK_TEST_OUTPUT_DIR})
set_tests_properties(itkObjectFactoryTest2 PROPERTIES RUN_SERIAL 1)
endif(ITK_BUILD_SHARED_LIBS)

add_test(NAME itkPeriodicBoundaryConditionTest COMMAND ITK-CommonTestDriver2 itkPeriodicBoundaryConditionTest)
add_test(NAME itkPhasedArray3DSpecialCoordinatesImageTest COMMAND ITK-CommonTestDriver1 itkPhasedArray3DSpecialCoordinatesImageTest)
add_test(NAME itkRealTimeClockTest COMMAND ITK-CommonTestDriver1 itkRealTimeClockTest)
add_test(NAME itkRealTimeStampTest COMMAND ITK-CommonTestDriver1 itkRealTimeStampTest)
add_test(NAME itkRealTimeIntervalTest COMMAND ITK-CommonTestDriver1 itkRealTimeIntervalTest)
add_test(NAME itkRGBPixelTest COMMAND ITK-CommonTestDriver1 itkRGBPixelTest)
add_test(NAME itkScalarToRGBPixelFunctorTest COMMAND ITK-CommonTestDriver1 itkScalarToRGBPixelFunctorTest)
add_test(NAME itkSizeTest COMMAND ITK-CommonTestDriver2 itkSizeTest)
add_test(NAME itkSliceIteratorTest COMMAND ITK-CommonTestDriver2 itkSliceIteratorTest)
add_test(NAME itkSmartPointerTest COMMAND ITK-CommonTestDriver2 itkSmartPointerTest)
add_test(NAME itkSpatialFunctionTest COMMAND ITK-CommonTestDriver2 itkSpatialFunctionTest)
add_test(NAME itkSTLContainerAdaptorTest COMMAND ITK-CommonTestDriver2 itkSTLContainerAdaptorTest)
add_test(NAME itkStdStreamLogOutputTest COMMAND ITK-CommonTestDriver2 itkStdStreamLogOutputTest ${TEMP}/testStreamLogOutput.txt)
add_test(NAME itkThreadLoggerTest COMMAND ITK-CommonTestDriver2 itkThreadLoggerTest ${TEMP}/test_threadLogger.txt)
add_test(NAME itkTreeContainerTest COMMAND ITK-CommonTestDriver2 itkTreeContainerTest)
add_test(NAME itkTreeContainerTest2 COMMAND ITK-CommonTestDriver1 itkTreeContainerTest2)
add_test(NAME itkVersorTest COMMAND ITK-CommonTestDriver2 itkVersorTest)
add_test(NAME itkEllipsoidInteriorExteriorSpatialFunctionTest COMMAND ITK-CommonTestDriver2 itkEllipsoidInteriorExteriorSpatialFunctionTest)
add_test(NAME itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest COMMAND ITK-CommonTestDriver1 itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest)
add_test(NAME itkSymmetricSecondRankTensorTest COMMAND ITK-CommonTestDriver2 itkSymmetricSecondRankTensorTest)
add_test(NAME itkSymmetricEigenAnalysisTest COMMAND ITK-CommonTestDriver1 itkSymmetricEigenAnalysisTest)
add_test(NAME itkVariableLengthVectorTest COMMAND ITK-CommonTestDriver2 itkVariableLengthVectorTest)
add_test(NAME itkVariableSizeMatrixTest COMMAND ITK-CommonTestDriver2 itkVariableSizeMatrixTest)
#add_test(NAME itkQuaternionOrientationAdapterTest COMMAND ITK-CommonTestDriver2 itkQuaternionOrientationAdapterTest)

if( "${ITK_COMPUTER_MEMORY_SIZE}" GREATER 4 )
  add_test(NAME itkImageFillBufferTest4.1 COMMAND ITK-CommonTestDriver2 itkImageFillBufferTest 4.1)
endif()

#
# this test depends on being able to raise C++ exceptions from
# the signal handler. This is apparently not something that
# works properly
if("${CMAKE_SYSTEM_NAME}" MATCHES "Linux")
  add_test(NAME itkFloatingPointExceptionsTest1 COMMAND ITK-CommonTestDriver1
    itkFloatingPointExceptionsTest DivByZero)
  add_test(NAME itkFloatingPointExceptionsTest2 COMMAND ITK-CommonTestDriver1
    itkFloatingPointExceptionsTest ZeroDivByZero)
  add_test(NAME itkFloatingPointExceptionsTest3 COMMAND ITK-CommonTestDriver1
    itkFloatingPointExceptionsTest FPOverFlow)
  add_test(NAME itkFloatingPointExceptionsTest4 COMMAND ITK-CommonTestDriver1
    itkFloatingPointExceptionsTest FPUnderFlow)
  add_test(NAME itkFloatingPointExceptionsTest5 COMMAND ITK-CommonTestDriver1
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
target_link_libraries(itkSystemInformationTest  ${ITK-Common_LIBRARIES})
add_test(NAME itkSystemInformationTest
         COMMAND itkSystemInformationTest ${CMAKE_BINARY_DIR})
