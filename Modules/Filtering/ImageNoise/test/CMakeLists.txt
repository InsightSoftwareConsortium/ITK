itk_module_test()
set(ITKImageNoiseTests
  itkAdditiveGaussianNoiseImageFilterTest.cxx
  itkShotNoiseImageFilterTest.cxx
  itkSpeckleNoiseImageFilterTest.cxx
  itkSaltAndPepperNoiseImageFilterTest.cxx
  itkPeakSignalToNoiseRatioCalculatorTest.cxx
)


CreateTestDriver(ITKImageNoise  "${ITKImageNoise-Test_LIBRARIES}" "${ITKImageNoiseTests}")

itk_add_test(NAME itkAdditiveGaussianNoiseImageFilterTest
  COMMAND ITKImageNoiseTestDriver
  itkAdditiveGaussianNoiseImageFilterTest
   DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
   ${ITK_TEST_OUTPUT_DIR}/itkAdditiveGaussianNoiseImageFilterTest.png
   29.85 0)
itk_add_test(NAME itkAdditiveGaussianNoiseImageFilterTestPSNR
  COMMAND ITKImageNoiseTestDriver
  itkPeakSignalToNoiseRatioCalculatorTest
   DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
   ${ITK_TEST_OUTPUT_DIR}/itkAdditiveGaussianNoiseImageFilterTest.png
   20 1)
set_tests_properties(itkAdditiveGaussianNoiseImageFilterTestPSNR
  PROPERTIES
   DEPENDS itkAdditiveGaussianNoiseImageFilterTest
   REQUIRED_FILES ${ITK_TEST_OUTPUT_DIR}/itkAdditiveGaussianNoiseImageFilterTest.png)
itk_add_test(NAME itkShotNoiseImageFilterTest
  COMMAND ITKImageNoiseTestDriver
  itkShotNoiseImageFilterTest
   DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkShotNoiseImageFilterTest.png
      0.105 )
itk_add_test(NAME itkShotNoiseImageFilterTestPSNR
  COMMAND ITKImageNoiseTestDriver
  itkPeakSignalToNoiseRatioCalculatorTest
   DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
   ${ITK_TEST_OUTPUT_DIR}/itkShotNoiseImageFilterTest.png
   20 1)
set_tests_properties(itkShotNoiseImageFilterTestPSNR
  PROPERTIES
   DEPENDS itkShotNoiseImageFilterTest
   REQUIRED_FILES ${ITK_TEST_OUTPUT_DIR}/itkShotNoiseImageFilterTest.png)
itk_add_test(NAME itkSpeckleNoiseImageFilterTest
  COMMAND ITKImageNoiseTestDriver
  itkSpeckleNoiseImageFilterTest
   DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkSpeckleNoiseImageFilterTest.png
      0.261)
itk_add_test(NAME itkSpeckleNoiseImageFilterTestPSNR
  COMMAND ITKImageNoiseTestDriver
  itkPeakSignalToNoiseRatioCalculatorTest
   DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
   ${ITK_TEST_OUTPUT_DIR}/itkSpeckleNoiseImageFilterTest.png
   20 1)
set_tests_properties(itkSpeckleNoiseImageFilterTestPSNR
  PROPERTIES
   DEPENDS itkSpeckleNoiseImageFilterTest
   REQUIRED_FILES ${ITK_TEST_OUTPUT_DIR}/itkSpeckleNoiseImageFilterTest.png)
itk_add_test(NAME itkSaltAndPepperNoiseImageFilterTest
  COMMAND ITKImageNoiseTestDriver
  itkSaltAndPepperNoiseImageFilterTest
   DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
      ${ITK_TEST_OUTPUT_DIR}/itkSaltAndPepperNoiseImageFilterTest.png
      0.0257)
itk_add_test(NAME itkSaltAndPepperNoiseImageFilterTestPSNR
  COMMAND ITKImageNoiseTestDriver
  itkPeakSignalToNoiseRatioCalculatorTest
   DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
   ${ITK_TEST_OUTPUT_DIR}/itkSaltAndPepperNoiseImageFilterTest.png
   20 1)
set_tests_properties(itkSaltAndPepperNoiseImageFilterTestPSNR
  PROPERTIES
   DEPENDS itkSaltAndPepperNoiseImageFilterTest
   REQUIRED_FILES ${ITK_TEST_OUTPUT_DIR}/itkSaltAndPepperNoiseImageFilterTest.png)
