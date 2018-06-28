itk_module_test()

set(SimpleITKFiltersTests
  itkObjectnessMeasureImageFilterTest.cxx
  itkHessianImageFilterTest.cxx
  itkSLICImageFilterTest.cxx
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


itk_add_test(NAME itkSLICImageFilterTest1
  COMMAND ${itk-module}TestDriver
    --compare "DATA{Baseline/itkSLICImageFilterTest1.png,:}"
              "${ITK_TEST_OUTPUT_DIR}/itkSLICImageFilterTestOutput1.png"
   itkSLICImageFilterTest
    "DATA{Input/VM1111Shrink-RGB.png}"
    "${ITK_TEST_OUTPUT_DIR}/itkSLICImageFilterTestOutput1.png" 25 0 )


itk_add_test(NAME itkSLICImageFilterTest2
  COMMAND ${itk-module}TestDriver
    --compare "DATA{Baseline/itkSLICImageFilterTest2.png}"
              "${ITK_TEST_OUTPUT_DIR}/itkSLICImageFilterTestOutput2.png"
  itkSLICImageFilterTest
    "DATA{Input/VM1111Shrink-LAB.mha}"
    "${ITK_TEST_OUTPUT_DIR}/itkSLICImageFilterTestOutput2.png" 25 0 )



itk_add_test(NAME itkSLICImageFilterTest3
  COMMAND ${itk-module}TestDriver
    --compare "DATA{Baseline/itkSLICImageFilterTest3.png}"
              "${ITK_TEST_OUTPUT_DIR}/itkSLICImageFilterTestOutput3.png"
  itkSLICImageFilterTest
    "DATA{Input/VM1111Shrink-LAB.mha}"
    "${ITK_TEST_OUTPUT_DIR}/itkSLICImageFilterTestOutput3.png" 25 1)


add_test(NAME itkHessianImageFilterTest
      COMMAND ${itk-module}TestDriver itkHessianImageFilterTest )


set(SimpleITKFiltersGTest
  itkHessianImageFilterGTest.cxx
  itkSLICImageFilterGTest.cxx)

CreateGoogleTestDriver(SimpleITKFilters "${SimpleITKFilters-Test_LIBRARIES}" "${SimpleITKFiltersGTest}")
