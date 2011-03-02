itk_module_test()
create_test_sourcelist(TESTS1 ITK-Common-tests1.cxx
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
itkShapedNeighborhoodIteratorTest.cxx
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



create_test_sourcelist(TESTS2 ITK-Common-tests2.cxx
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
)


set (TestsTorun ${TESTS1})
remove(TestsToRun ITK-Common-tests1.cxx)
add_executable(ITK-Common-tests1  ${TESTS1} )
target_link_libraries(ITK-Common-tests1 ${ITK-Common_LIBRARIES})

set (TestsTorun ${TESTS2})
remove(TestsToRun ITK-CommonTests2.cxx)
add_executable(ITK-Common-tests2  ${TESTS2} )
target_link_libraries(ITK-Common-tests2 ${ITK-Common_LIBRARIES})



#####################################################################################
if(ITK_CSWIG_TCL)
  add_test(itkBasicClasses ${ITKWISH_EXECUTABLE}
           itkBasicClasses.tcl)
endif(ITK_CSWIG_TCL)
#-----------------------------------------------------------------------------
# Setup the system information test.  Write out some basic failsafe
# information in case the test doesn't run.

set(COMMON_TESTS1 "${ITK_EXECUTABLE_PATH}/ITK-Common-tests1")
set(COMMON_TESTS2 "${ITK_EXECUTABLE_PATH}/ITK-Common-tests2")
set(BASELINE "${ITK_DATA_ROOT}/Baseline/Common")

set(TEMP "${ITKTesting_BINARY_DIR}/Temporary")
set(EXAMPLES_DATA_ROOT "${ITK_SOURCE_DIR}/Examples/Data")


#-----------------------------------------------------------------------------
if(ITK_USE_FLOATINGPOINTEXCEPTIONS)
  set_source_files_properties(itkFloatingPointExceptionsTest.cxx
    COMPILE_FLAGS -DITK_USE_FPE)
endif(ITK_USE_FLOATINGPOINTEXCEPTIONS)

add_executable( itkMathTest itkMathTest.cxx )
add_test( itkMathTest  "${ITK_EXECUTABLE_PATH}/itkMathTest" )
target_link_libraries(itkMathTest ${ITK-Common_LIBRARIES})

add_executable(itkSystemInformation itkSystemInformation.cxx)
target_link_libraries(itkSystemInformation ${ITK-Common_LIBRARIES})
set(SYSTEM_INFORMATION ${ITK_EXECUTABLE_PATH}/itkSystemInformation)
add_test(SystemInformation ${SYSTEM_INFORMATION})


if(ITK_OBJCXX_COMPILER_WORKS)
  set(COMMON_OBJCXX_TEST itkCommonHeaderObjCxxTest)
  add_test(itkCommonHeaderObjCxxTest ${COMMON_OBJCXX_TEST})
  add_executable(itkCommonHeaderObjCxxTest itkCommonHeaderObjCxxTest.mm)
  target_link_libraries(itkCommonHeaderObjCxxTest ${ITK-Common_LIBRARIES})
endif(ITK_OBJCXX_COMPILER_WORKS)


if(ITK_BUILD_SHARED_LIBS)
  ## Create a library to test ITK loadable factories
  set(FactoryTest_Source itkFactoryTestLib.cxx)
  set(LIBRARY_OUTPUT_PATH "${ITKTesting_BINARY_DIR}/Temporary")
  add_library(FactoryTestLib SHARED ${FactoryTest_Source})
  target_link_libraries(FactoryTestLib ${ITK-Common_LIBRARIES})
  add_dependencies(ITK-Common-tests2 FactoryTestLib)
endif(ITK_BUILD_SHARED_LIBS)

add_test(itkCommonHeaderTest ${COMMON_TESTS1} itkCommonHeaderTest)
add_test(itkAnnulusOperatorTest ${COMMON_TESTS1} itkAnnulusOperatorTest)
add_test(itkColorTableTest ${COMMON_TESTS1} itkColorTableTest)
add_test(itkDirectoryTest ${COMMON_TESTS1} itkDirectoryTest ${TEMP})
add_test(itkDerivativeOperatorTest ${COMMON_TESTS1} itkDerivativeOperatorTest)
add_test(itkMultipleLogOutputTest ${COMMON_TESTS1} itkMultipleLogOutputTest ${TEMP}/test_multi.txt)
add_test(itkFixedArrayTest ${COMMON_TESTS2} itkFixedArrayTest)
add_test(itkMinimumMaximumImageCalculatorTest ${COMMON_TESTS2} itkMinimumMaximumImageCalculatorTest)
add_test(itkFixedArrayTest2 ${COMMON_TESTS1} itkFixedArrayTest2)
add_test(itkArrayTest ${COMMON_TESTS1} itkArrayTest)
add_test(itkArray2DTest ${COMMON_TESTS1} itkArray2DTest)
add_test(itkAutoPointerTest ${COMMON_TESTS2} itkAutoPointerTest)
if(NOT MINGW)
add_test(itkBarrierTest ${COMMON_TESTS2} itkBarrierTest)
add_test(itkConditionVariableTest ${COMMON_TESTS1} itkConditionVariableTest)
endif(NOT MINGW)
add_test(itkTimeStampTest ${COMMON_TESTS2} itkTimeStampTest)
add_test(itkBoundingBoxTest ${COMMON_TESTS1} itkBoundingBoxTest)
add_test(itkBSplineInterpolationWeightFunctionTest ${COMMON_TESTS2} itkBSplineInterpolationWeightFunctionTest)
add_test(itkBSplineKernelFunctionTest ${COMMON_TESTS1} itkBSplineKernelFunctionTest)
add_test(itkConstNeighborhoodIteratorTest ${COMMON_TESTS2} itkConstNeighborhoodIteratorTest)
add_test(itkConstShapedNeighborhoodIteratorTest ${COMMON_TESTS2} itkConstShapedNeighborhoodIteratorTest)
add_test(itkConstShapedNeighborhoodIteratorTest2 ${COMMON_TESTS2} itkConstShapedNeighborhoodIteratorTest2)
add_test(itkEventObjectTest ${COMMON_TESTS1} itkEventObjectTest)
add_test(itkExceptionObjectTest ${COMMON_TESTS1} itkExceptionObjectTest)
add_test(itkFileOutputWindowTest ${COMMON_TESTS1} itkFileOutputWindowTest)
add_test(itkFiniteCylinderSpatialFunctionTest ${COMMON_TESTS2} itkFiniteCylinderSpatialFunctionTest)
add_test(itkGaussianSpatialFunctionTest ${COMMON_TESTS1} itkGaussianSpatialFunctionTest)
add_test(itkCrossHelperTest ${COMMON_TESTS1} itkCrossHelperTest)
add_test(itkImageIteratorTest ${COMMON_TESTS1} itkImageIteratorTest)
add_test(itkImageIteratorWithIndexTest ${COMMON_TESTS1} itkImageIteratorWithIndexTest)
add_test(itkImageRegionIteratorTest ${COMMON_TESTS1} itkImageRegionIteratorTest)
add_test(itkImageRegionTest ${COMMON_TESTS1} itkImageRegionTest)
add_test(itkImageRegionExclusionIteratorWithIndexTest ${COMMON_TESTS2} itkImageRegionExclusionIteratorWithIndexTest)
add_test(itkImageReverseIteratorTest ${COMMON_TESTS1} itkImageReverseIteratorTest)
add_test(itkImageTest ${COMMON_TESTS1} itkImageTest)
add_test(itkLightObjectTest ${COMMON_TESTS1} itkLightObjectTest)
add_test(itkLineIteratorTest ${COMMON_TESTS1} itkLineIteratorTest ${BASELINE}/itkLineIteratorTest.txt )
add_test(itkLoggerTest ${COMMON_TESTS1} itkLoggerTest ${TEMP}/test_logger.txt)
add_test(itkLoggerOutputTest ${COMMON_TESTS2} itkLoggerOutputTest ${TEMP}/test_loggerOutput.txt)
add_test(itkLoggerManagerTest ${COMMON_TESTS2} itkLoggerManagerTest ${TEMP}/test_LoggerManager.txt)
add_test(itkMatrixTest ${COMMON_TESTS2} itkMatrixTest)
add_test(itkMapContainerTest ${COMMON_TESTS1} itkMapContainerTest)
add_test(itkObjectFactoryTest ${COMMON_TESTS1} itkObjectFactoryTest)
add_test(itkVectorTest ${COMMON_TESTS1} itkVectorTest)
add_test(itkPointSetTest ${COMMON_TESTS1} itkPointSetTest)
add_test(itkBresenhamLineTest ${COMMON_TESTS1} itkBresenhamLineTest)
add_test(itkSparseFieldLayerTest ${COMMON_TESTS1} itkSparseFieldLayerTest)
add_test(itkDataObjectTest ${COMMON_TESTS1} itkDataObjectTest)
#
# This test should be enabled if you suspect that the memory leak detector
# is not working
if(0)
  add_test(itkMemoryLeakTest ${COMMON_TESTS2} itkMemoryLeakTest)
endif(0)

add_test(itkMetaDataDictionaryTest ${COMMON_TESTS2} itkMetaDataDictionaryTest)
add_test(itkMultiThreaderTest ${COMMON_TESTS2} itkMultiThreaderTest)
add_test(itkNeighborhoodAlgorithmTest ${COMMON_TESTS1} itkNeighborhoodAlgorithmTest)
add_test(itkNeighborhoodTest ${COMMON_TESTS2} itkNeighborhoodTest)
add_test(itkNeighborhoodIteratorTest ${COMMON_TESTS2} itkNeighborhoodIteratorTest)
add_test(itkNeighborhoodOperatorTest ${COMMON_TESTS1} itkNeighborhoodOperatorTest)
add_test(itkNumericTraitsTest ${COMMON_TESTS1} itkNumericTraitsTest)
add_test(itkIntTypesTest ${COMMON_TESTS1} itkIntTypesTest)
add_test(itkOctreeTest ${COMMON_TESTS2} itkOctreeTest)


if(ITK_BUILD_SHARED_LIBS)
add_test(itkObjectFactoryTest2 ${COMMON_TESTS2} itkObjectFactoryTest2 ${ITK_TEST_OUTPUT_DIR})
set_tests_properties(itkObjectFactoryTest2 PROPERTIES RUN_SERIAL 1)
endif(ITK_BUILD_SHARED_LIBS)

add_test(itkPeriodicBoundaryConditionTest ${COMMON_TESTS2} itkPeriodicBoundaryConditionTest)
add_test(itkPhasedArray3DSpecialCoordinatesImageTest ${COMMON_TESTS1} itkPhasedArray3DSpecialCoordinatesImageTest)
add_test(itkRealTimeClockTest ${COMMON_TESTS1} itkRealTimeClockTest)
add_test(itkRealTimeStampTest ${COMMON_TESTS1} itkRealTimeStampTest)
add_test(itkRealTimeIntervalTest ${COMMON_TESTS1} itkRealTimeIntervalTest)
add_test(itkRGBPixelTest ${COMMON_TESTS1} itkRGBPixelTest)
add_test(itkScalarToRGBPixelFunctorTest ${COMMON_TESTS1} itkScalarToRGBPixelFunctorTest)
add_test(itkShapedNeighborhoodIteratorTest ${COMMON_TESTS1} itkShapedNeighborhoodIteratorTest)
add_test(itkSizeTest ${COMMON_TESTS2} itkSizeTest)
add_test(itkSliceIteratorTest ${COMMON_TESTS2} itkSliceIteratorTest)
add_test(itkSmartPointerTest ${COMMON_TESTS2} itkSmartPointerTest)
add_test(itkSpatialFunctionTest ${COMMON_TESTS2} itkSpatialFunctionTest)
add_test(itkSTLContainerAdaptorTest ${COMMON_TESTS2} itkSTLContainerAdaptorTest)
add_test(itkStdStreamLogOutputTest ${COMMON_TESTS2} itkStdStreamLogOutputTest ${TEMP}/testStreamLogOutput.txt)
add_test(itkThreadLoggerTest ${COMMON_TESTS2} itkThreadLoggerTest ${TEMP}/test_threadLogger.txt)
add_test(itkTreeContainerTest ${COMMON_TESTS2} itkTreeContainerTest)
add_test(itkTreeContainerTest2 ${COMMON_TESTS1} itkTreeContainerTest2)
add_test(itkVersorTest ${COMMON_TESTS2} itkVersorTest)
add_test(itkEllipsoidInteriorExteriorSpatialFunctionTest ${COMMON_TESTS2} itkEllipsoidInteriorExteriorSpatialFunctionTest)
add_test(itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest ${COMMON_TESTS1} itkSymmetricEllipsoidInteriorExteriorSpatialFunctionTest)
add_test(itkSymmetricSecondRankTensorTest ${COMMON_TESTS2} itkSymmetricSecondRankTensorTest)
add_test(itkSymmetricEigenAnalysisTest ${COMMON_TESTS1} itkSymmetricEigenAnalysisTest)
add_test(itkVariableLengthVectorTest ${COMMON_TESTS2} itkVariableLengthVectorTest)
add_test(itkVariableSizeMatrixTest ${COMMON_TESTS2} itkVariableSizeMatrixTest)
#add_test(itkQuaternionOrientationAdapterTest ${COMMON_TESTS2} itkQuaternionOrientationAdapterTest)

if( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 4 )
  add_test(itkImageFillBufferTest4.1 ${COMMON_TESTS2} itkImageFillBufferTest 4.1)
endif( ${ITK_COMPUTER_MEMORY_SIZE} GREATER 4 )

#
# this test depends on being able to raise C++ exceptions from
# the signal handler. This is apparently not something that
# works properly
if(ITK_USE_FLOATINGPOINTEXCEPTIONS)
  add_test(itkFloatingPointExceptionsTest1 ${COMMON_TESTS2}
    itkFloatingPointExceptionsTest DivByZero)
  add_test(itkFloatingPointExceptionsTest2 ${COMMON_TESTS2}
    itkFloatingPointExceptionsTest ZeroDivByZero)
  add_test(itkFloatingPointExceptionsTest3 ${COMMON_TESTS2}
    itkFloatingPointExceptionsTest FPOverFlow)
  add_test(itkFloatingPointExceptionsTest4 ${COMMON_TESTS2}
    itkFloatingPointExceptionsTest FPUnderFlow)
  add_test(itkFloatingPointExceptionsTest5 ${COMMON_TESTS2}
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

set(ITK_SYSTEM_INFORMATION_DIR ${ITK_BINARY_DIR})

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

configure_file(itkSystemInformationTest.h.in  ${ITK-Common_BINARY_DIR}/itkSystemInformationTest.h)
set(SYSTEM_INFORMATION_TEST ${ITK_EXECUTABLE_PATH}/itkSystemInformationTest)
add_executable(itkSystemInformationTest itkSystemInformationTest.cxx)
target_link_libraries(itkSystemInformationTest  ${ITK-Common_LIBRARIES})
add_test(itkSystemInformationTest ${SYSTEM_INFORMATION_TEST})
