itk_module_test()
set(ITKDeprecatedTests
itkAtomicIntTest.cxx
itkBarrierTest.cxx
itkConditionVariableTest.cxx
itkSimpleFastMutexLockTest.cxx
itkVectorAnisotropicDiffusionImageFilterTest.cxx
itkVectorCentralDifferenceImageFunctionTest.cxx
itkVectorConfidenceConnectedImageFilterTest.cxx
itkVectorConnectedComponentImageFilterTest.cxx
itkVectorExpandImageFilterTest.cxx
itkVectorGradientMagnitudeImageFilterTest1.cxx
itkVectorGradientMagnitudeImageFilterTest2.cxx
itkVectorGradientMagnitudeImageFilterTest3.cxx
itkVectorMeanImageFunctionTest.cxx
itkVectorResampleImageFilterTest.cxx
itkVectorThresholdSegmentationLevelSetImageFilterTest.cxx
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

itk_add_test(NAME itkVectorAnisotropicDiffusionImageFilterTest
      COMMAND ITKDeprecatedTestDriver itkVectorAnisotropicDiffusionImageFilterTest)
itk_add_test(NAME itkVectorCentralDifferenceImageFunctionTest
      COMMAND ITKDeprecatedTestDriver itkVectorCentralDifferenceImageFunctionTest)
itk_add_test(NAME itkVectorConfidenceConnectedImageFilterTest
      COMMAND ITKDeprecatedTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorConfidenceConnectedImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest.png
    itkVectorConfidenceConnectedImageFilterTest DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png} ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest.png 72 44 23 67 5.0 6)
itk_add_test(NAME itkVectorConfidenceConnectedImageFilterTest2
      COMMAND ITKDeprecatedTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest2.png 06aa8c7e4e2aeb3a3028d5b9d5fe6e27
    itkVectorConfidenceConnectedImageFilterTest
      DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png}
      ${ITK_TEST_OUTPUT_DIR}/VectorConfidenceConnectedImageFilterTest2.png 9999 9999 9999 9999 5.0 6)
itk_add_test(NAME itkVectorConnectedComponentImageFilterTest
      COMMAND ITKDeprecatedTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorConnectedComponentImageFilterTest.png,:}
              ${ITK_TEST_OUTPUT_DIR}/VectorConnectedComponentImageFilterTest.png
    itkVectorConnectedComponentImageFilterTest ${ITK_TEST_OUTPUT_DIR}/VectorConnectedComponentImageFilterTest.png)
itk_add_test(NAME itkVectorExpandImageFilterTest
      COMMAND ITKDeprecatedTestDriver itkVectorExpandImageFilterTest)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest1a
      COMMAND ITKDeprecatedTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1a.png}
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png
    itkVectorGradientMagnitudeImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png} ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1a.png 0)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest1b
      COMMAND ITKDeprecatedTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest1b.png}
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png
    itkVectorGradientMagnitudeImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png} ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest1b.png 1)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest2
      COMMAND ITKDeprecatedTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png
    itkVectorGradientMagnitudeImageFilterTest2 DATA{${ITK_DATA_ROOT}/Input/VHFColor.mhd,VHFColor.raw} ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2.png 0 5)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest2b
      COMMAND ITKDeprecatedTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorGradientMagnitudeImageFilterTest2b.png}
              ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png
    itkVectorGradientMagnitudeImageFilterTest2 DATA{${ITK_DATA_ROOT}/Input/VHFColor.mhd,VHFColor.raw} ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest2b.png 1 5)
itk_add_test(NAME itkVectorGradientMagnitudeImageFilterTest3
      COMMAND ITKDeprecatedTestDriver itkVectorGradientMagnitudeImageFilterTest3
              DATA{${ITK_DATA_ROOT}/Input/VHFColor.mhd,VHFColor.raw} ${ITK_TEST_OUTPUT_DIR}/VectorGradientMagnitudeImageFilterTest3.mha 1)
itk_add_test(NAME itkVectorMeanImageFunctionTest
      COMMAND ITKDeprecatedTestDriver itkVectorMeanImageFunctionTest)
itk_add_test(NAME itkVectorResampleImageFilterTest
      COMMAND ITKDeprecatedTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/BasicFilters/VectorResampleImageFilterTest.png}
              ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png
    itkVectorResampleImageFilterTest ${ITK_TEST_OUTPUT_DIR}/VectorResampleImageFilterTest.png)
itk_add_test(NAME itkVectorThresholdSegmentationLevelSetImageFilterTest
      COMMAND ITKDeprecatedTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/Algorithms/VectorThresholdSegmentationLevelSetImageFilterTest.png,:}
              ${ITK_TEST_OUTPUT_DIR}/VectorThresholdSegmentationLevelSetImageFilterTest.png
    --compareNumberOfPixelsTolerance 55
    itkVectorThresholdSegmentationLevelSetImageFilterTest DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSliceInitialLevelSet.png} DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png} ${ITK_TEST_OUTPUT_DIR}/VectorThresholdSegmentationLevelSetImageFilterTest.png 5.0 50)
