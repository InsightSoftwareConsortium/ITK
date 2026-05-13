itk_module_test()
set(
  ParabolicMorphologyTests
  itkParaErodeTest.cxx
  itkParaDilateTest.cxx
  itkParaOpenTest.cxx
  itkParaSpacingTest.cxx
  itkParaSharpenTest.cxx
  itkParaDTTest.cxx
  itkBinaryDilateParabolicTest.cxx
  itkBinaryErodeParabolicTest.cxx
  itkBinaryOpenParabolicTest.cxx
  itkBinaryCloseParabolicTest.cxx
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
    itkParaSpacingTest
    ${INPUT_IMAGE}
    osp1.png
    osp2.png
)

## sharpening
itk_add_test(
  NAME itkParaSharpenTest2D_1
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    sharp1.mha
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/sharp1.mha
    itkParaSharpenTest
    1
    sharp1.mha
)

itk_add_test(
  NAME itkParaSharpenTest2D_3
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    sharp3.mha
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/sharp3.mha
    itkParaSharpenTest
    3
    sharp3.mha
)

itk_add_test(
  NAME itkParaSharpenTest2D_10
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    sharp10.mha
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/sharp10.mha
    itkParaSharpenTest
    10
    sharp10.mha
)

itk_add_test(
  NAME itkParaSharpenTest2D_100
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    sharp100.mha
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/sharp100.mha
    itkParaSharpenTest
    100
    sharp100.mha
)

## distance transform
itk_add_test(
  NAME itkParaDTTest2D_0
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    dist1.mha
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/dist1.mha
    itkParaDTTest
    ${INPUT_IMAGE}
    100
    0
    dist1.mha
)

itk_add_test(
  NAME itkParaDTTest2D_255
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    dist255.mha
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/dist255.mha
    itkParaDTTest
    ${INPUT_IMAGE}
    100
    255
    dist255.mha
)

## Binary morphology
itk_add_test(
  NAME itkBinaryDilateParabolic2D_10
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    dilatebinary10.mha
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/dilatebinary10.mha
    itkBinaryDilateParabolicTest
    ${INPUT_IMAGE}
    150
    10
    dilatebinary10.mha
)

itk_add_test(
  NAME itkBinaryErodeParabolic2D_10
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    erodebinary10.mha
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/erodebinary10.mha
    itkBinaryErodeParabolicTest
    ${INPUT_IMAGE}
    120
    10
    erodebinary10.mha
)

itk_add_test(
  NAME itkBinaryOpenParabolic2D_10
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    openbinary10.mha
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/openbinary10.mha
    itkBinaryOpenParabolicTest
    ${INPUT_IMAGE}
    130
    10
    openbinary10.mha
)

itk_add_test(
  NAME itkBinaryCloseParabolic2D_10
  COMMAND
    ParabolicMorphologyTestDriver
    --compare
    closebinary10.mha
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/closebinary10.mha
    itkBinaryCloseParabolicTest
    ${INPUT_IMAGE}
    150
    10
    closebinary10.mha
)
