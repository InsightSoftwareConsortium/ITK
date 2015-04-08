itk_module_test()
set(
  ParabolicMorphologyTests
  itkParaErodeTest.cxx
  itkParaDilateTest.cxx
  itkParaOpenTest.cxx
  itkParaSpacingTest.cxx
  #itkParaSharpenTest.cxx
  #itkParaDTTest.cxx
)

set(INPUT_IMAGE ${CMAKE_CURRENT_SOURCE_DIR}/images/cthead1.png)

set(ITK_TEST_DRIVER itkTestDriver)

createtestdriver(ParabolicMorphology "${ParabolicMorphology-Test_LIBRARIES}" "${ParabolicMorphologyTests}")

## both intersection and contact point erosion
## default scale
itk_add_test(
  NAME itkParaErodeTest2D_1
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    outEInta.png
    outECPa.png
    --compare
    outEInta.png
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/outECPa.png
    itkParaErodeTest
    ${INPUT_IMAGE}
    outEInta.png
    outECPa.png
)

## small scale
itk_add_test(
  NAME itkParaErodeTest2D_2
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    outEIntb.png
    outECPb.png
    --compare
    outEIntb.png
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/outECPb.png
    itkParaErodeTest
    ${INPUT_IMAGE}
    outEIntb.png
    outECPb.png
    0.2
)

## large scale
itk_add_test(
  NAME itkParaErodeTest2D_3
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    outEIntc.png
    outECPc.png
    --compare
    outEIntc.png
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/outECPc.png
    itkParaErodeTest
    ${INPUT_IMAGE}
    outEIntc.png
    outECPc.png
    5
)

## default scale
itk_add_test(
  NAME itkParaDilateTest2D_1
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    outDInta.png
    outDCPa.png
    --compare
    outDInta.png
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/outDCPa.png
    itkParaDilateTest
    ${INPUT_IMAGE}
    outDInta.png
    outDCPa.png
)

## small scale
itk_add_test(
  NAME itkParaDilateTest2D_2
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    outDIntb.png
    outDCPb.png
    --compare
    outDIntb.png
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/outDCPb.png
    itkParaDilateTest
    ${INPUT_IMAGE}
    outDIntb.png
    outDCPb.png
    0.2
)

## large scale
itk_add_test(
  NAME itkParaDilateTest2D_3
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    outDIntc.png
    outDCPc.png
    --compare
    outDIntc.png
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/outDCPc.png
    itkParaDilateTest
    ${INPUT_IMAGE}
    outDIntc.png
    outDCPc.png
    5
)

## Opening
itk_add_test(
  NAME itkParaOpenTest2D_1
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    openCP.png
    openInt.png
    --compare
    openInt.png
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/openInt.png
    itkParaOpenTest
    ${INPUT_IMAGE}
    openInt.png
    openCP.png
)

itk_add_test(
  NAME itkParaSpacingTest2D_1
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    osp1.png
    osp2.png
    itkParaOpenTest
    ${INPUT_IMAGE}
    osp1.png
    osp2.png
)
