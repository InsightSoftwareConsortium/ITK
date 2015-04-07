itk_module_test()
set(
  ParabolicMorphologyTests
  itkParaErodeTest.cxx
  #itkParaDilateTest.cxx
  #itkParaOpenTest.cxx
  #itkParaSpacingTest.cxx
  #itkParaSharpenTest.cxx
  #itkParaDTTest.cxx
)

set(INPUT_IMAGE ${CMAKE_CURRENT_SOURCE_DIR}/images/cthead1.png)

set(ITK_TEST_DRIVER itkTestDriver)

createtestdriver(ParabolicMorphology "${ParabolicMorphology-Test_LIBRARIES}" "${ParabolicMorphologyTests}")

## both intersection and contact point erosion
itk_add_test(
  NAME itkParaErodeTest1
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    outEInta.png
    outECPa.png
    --compare
    outEInta.png
    baseline/outECPa.png
    itkParaErodeTest
    ${INPUT_IMAGE}
    outEInta.png
    outECPa.png
)
