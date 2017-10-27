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
      DATA{Baseline/ObjectnessMeasureImageFilterTest1.nii}
      ${ITK_TEST_OUTPUT_DIR}/ObjectnessMeasureImageFilterTestOutput1.nii
    itkObjectnessMeasureImageFilterTest
      DATA{Input/DSA.png}
      ${ITK_TEST_OUTPUT_DIR}/ObjectnessMeasureImageFilterTestOutput1.nii
      1 0
    )

itk_add_test(NAME itkObjectnessMeasureImageFilterTest2
  COMMAND ${itk-module}TestDriver
    --compareIntensityTolerance .001
    --compare
      DATA{Baseline/ObjectnessMeasureImageFilterTest2.nii}
      ${ITK_TEST_OUTPUT_DIR}/ObjectnessMeasureImageFilterTestOutput2.nii
    itkObjectnessMeasureImageFilterTest
      DATA{Input/DSA.png}
      ${ITK_TEST_OUTPUT_DIR}/ObjectnessMeasureImageFilterTestOutput2.nii
      0 0
    )

add_test(NAME itkHessianImageFilterTest
      COMMAND ${itk-module}TestDriver itkHessianImageFilterTest )


set(SimpleITKFiltersGTest
  itkHessianImageFilterGTest.cxx)

CreateGoogleTestDriver(SimpleITKFilters "${SimpleITKFilters-Test_LIBRARIES}" "${SimpleITKFiltersGTest}")
