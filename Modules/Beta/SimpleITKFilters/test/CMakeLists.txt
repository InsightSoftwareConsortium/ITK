itk_module_test()

set(SimpleITKFiltersTests
  itkObjectnessMeasureImageFilterTest.cxx
  itkHessianImageFilterTest.cxx
  )

CreateTestDriver(SimpleITKFilters "${SimpleITKFilters_LIBRARIES}" "${SimpleITKFiltersTests}")

itk_add_test(NAME itkObjectnessMeasureImageFilterTest1
  COMMAND ${itk-module}TestDriver
    --compareIntensityTolerance .001
    --compare
      ${ITK_TEST_OUTPUT_DIR}/ObjectnessMeasureImageFilterTestOutput1.nii
      DATA{Baseline/ObjectnessMeasureImageFilterTest1.nii}
    itkObjectnessMeasureImageFilterTest
      DATA{Input/DSA.png}
      ${ITK_TEST_OUTPUT_DIR}/ObjectnessMeasureImageFilterTestOutput1.nii
      1 0
    )

itk_add_test(NAME itkObjectnessMeasureImageFilterTest2
  COMMAND ${itk-module}TestDriver
    --compareIntensityTolerance .001
    --compare
      ${ITK_TEST_OUTPUT_DIR}/ObjectnessMeasureImageFilterTestOutput2.nii
      DATA{Baseline/ObjectnessMeasureImageFilterTest2.nii}
    itkObjectnessMeasureImageFilterTest
      DATA{Input/DSA.png}
      ${ITK_TEST_OUTPUT_DIR}/ObjectnessMeasureImageFilterTestOutput2.nii
      0 0
    )

itk_add_test(NAME itkObjectnessMeasureImageFilterTest_compatibility1
      COMMAND ${itk-module}TestDriver
  --compareNumberOfPixelsTolerance 7000
  --compareIntensityTolerance .01
  --compare
      ${ITK_TEST_OUTPUT_DIR}/itkObjectnessMeasureImageFilterTest_compatibility1.mha
      DATA{Input/itkHessianToObjectnessMeasureImageFilterTest.mha}
  itkObjectnessMeasureImageFilterTest
    DATA{Input/DSA.png}
    ${ITK_TEST_OUTPUT_DIR}/itkObjectnessMeasureImageFilterTest_compatibility1.mha
    1 0)

itk_add_test(NAME itkObjectnessMeasureImageFilterTest_compatibility2
      COMMAND ${itk-module}TestDriver
  --compareNumberOfPixelsTolerance 7000
  --compareIntensityTolerance .01
  --compare
    ${ITK_TEST_OUTPUT_DIR}/itkObjectnessMeasureImageFilterTest_compatibility2.mha
    DATA{Baseline/itkHessianToObjectnessMeasureImageFilterTest2.mha}
  itkObjectnessMeasureImageFilterTest
    DATA{Input/DSA.png}
    ${ITK_TEST_OUTPUT_DIR}/itkObjectnessMeasureImageFilterTest_compatibility2.mha
    0 0)

add_test(NAME itkHessianImageFilterTest
      COMMAND ${itk-module}TestDriver itkHessianImageFilterTest )


set(SimpleITKFiltersGTest
  itkHessianImageFilterGTest.cxx
        itkDICOMOrientImageFilterGTest.cxx
        itkMaskedAssignImageFilterGTest.cxx)

CreateGoogleTestDriver(SimpleITKFilters "${SimpleITKFilters-Test_LIBRARIES}" "${SimpleITKFiltersGTest}")
