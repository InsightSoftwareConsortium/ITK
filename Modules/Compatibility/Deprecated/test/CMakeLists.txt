itk_module_test()
set(ITKDeprecatedTests
itkAtomicIntTest.cxx
itkBarrierTest.cxx
itkConditionVariableTest.cxx
itkSimpleFastMutexLockTest.cxx
itkVectorCentralDifferenceImageFunctionTest.cxx
itkVectorResampleImageFilterTest.cxx
itkTreeContainerTest.cxx
itkTreeContainerTest2.cxx
)

if (NOT ITK_LEGACY_REMOVE)
  list(APPEND ITKDeprecatedTests itkSpawnThreadTest.cxx)
endif()

CreateTestDriver(ITKDeprecated "${ITKDeprecated-Test_LIBRARIES}" "${ITKDeprecatedTests}")

set(BASELINE "${ITK_DATA_ROOT}/Baseline/Common")
set(TEMP ${ITK_TEST_OUTPUT_DIR})

itk_add_test(NAME itkAtomicIntTest COMMAND ITKDeprecatedTestDriver itkAtomicIntTest)

if(NOT MINGW)
itk_add_test(NAME itkBarrierTest COMMAND ITKDeprecatedTestDriver itkBarrierTest)
itk_add_test(NAME itkBarrierTest107 COMMAND ITKDeprecatedTestDriver itkBarrierTest 107)
itk_add_test(NAME itkConditionVariableTest COMMAND ITKDeprecatedTestDriver itkConditionVariableTest)
endif()

itk_add_test(NAME itkSimpleFastMutexLockTest COMMAND ITKDeprecatedTestDriver itkSimpleFastMutexLockTest)
# short timeout because failing test will hang and test is quite small
set_tests_properties(itkSimpleFastMutexLockTest PROPERTIES TIMEOUT 60)

if (NOT ITK_LEGACY_REMOVE)
  itk_add_test(NAME itkSpawnThreadTest COMMAND ITKDeprecatedTestDriver itkSpawnThreadTest 100)
endif()

itk_add_test(NAME itkVectorCentralDifferenceImageFunctionTest
      COMMAND ITKDeprecatedTestDriver itkVectorCentralDifferenceImageFunctionTest)

itk_add_test(NAME itkVectorResampleImageFilterTest
      COMMAND ITKDeprecatedTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorResampleImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png
    itkVectorResampleImageFilterTest ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png)

itk_add_test(NAME itkTreeContainerTest COMMAND ITKDeprecatedTestDriver itkTreeContainerTest)
itk_add_test(NAME itkTreeContainerTest2 COMMAND ITKDeprecatedTestDriver itkTreeContainerTest2)
