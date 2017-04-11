itk_module_test()

set(INPUTDATA ${CMAKE_CURRENT_SOURCE_DIR}/Data)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

set(ITK_TEST_DRIVER itkTestDriver)

set(
  TextureFeaturesTests
  ScalarImageToRunLengthFeaturesImageFilterCreationTest.cxx
  ScalarImageToRunLengthFeaturesImageFilterTest.cxx
  ScalarImageToRunLengthFeaturesImageFilterTestSeparateFeatures.cxx
  ScalarImageToRunLengthFeaturesImageFilterTestWithoutMask.cxx
)

set(TestOutput ${ITK_TEST_OUTPUT_DIR})

createtestdriver(TextureFeatures "${TextureFeatures-Test_LIBRARIES}" "${TextureFeaturesTests}")

itk_add_test(
  NAME ScalarImageToRunLengthFeaturesImageFilterCreationTest
  COMMAND
    TextureFeaturesTestDriver
    ScalarImageToRunLengthFeaturesImageFilterCreationTest
)

itk_add_test(
  NAME ScalarImageToRunLengthFeaturesImageFilterTestWithoutMask1
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{${INPUTDATA}/resultTestWithoutMask1.nrrd}
    ${TEMP}/resultTestWithoutMask1.nrrd
    ScalarImageToRunLengthFeaturesImageFilterTestWithoutMask
    DATA{${INPUTDATA}/Scan_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultTestWithoutMask1.nrrd
    10
    0
    4200
    0
    0.4
    2
)

itk_add_test(
  NAME ScalarImageToRunLengthFeaturesImageFilterTestWithoutMask2
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{${INPUTDATA}/resultTestWithoutMask2.nrrd}
    ${TEMP}/resultTestWithoutMask2.nrrd
    ScalarImageToRunLengthFeaturesImageFilterTestWithoutMask
    DATA{${INPUTDATA}/Scan_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultTestWithoutMask2.nrrd
    10
    1000
    4200
    0
    0.4
    4
)

itk_add_test(
  NAME ScalarImageToRunLengthFeaturesImageFilterTestWithoutMask3
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{${INPUTDATA}/resultTestWithoutMask3.nrrd}
    ${TEMP}/resultTestWithoutMask3.nrrd
    ScalarImageToRunLengthFeaturesImageFilterTestWithoutMask
    DATA{${INPUTDATA}/Scan_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultTestWithoutMask3.nrrd
    10
    0
    4200
    0
    1
    6
)

itk_add_test(
  NAME ScalarImageToRunLengthFeaturesImageFilterPartialImage1
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{${INPUTDATA}/resultPartialImage1.nrrd}
    ${TEMP}/resultPartialImage1.nrrd
    ScalarImageToRunLengthFeaturesImageFilterTest
    DATA{${INPUTDATA}/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{${INPUTDATA}/SegmC_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultPartialImage1.nrrd
    10
    0
    4200
    0
    0.4
    2
)

itk_add_test(
  NAME ScalarImageToRunLengthFeaturesImageFilterPartialImage2
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{${INPUTDATA}/resultPartialImage2.nrrd}
    ${TEMP}/resultPartialImage2.nrrd
    ScalarImageToRunLengthFeaturesImageFilterTest
    DATA{${INPUTDATA}/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{${INPUTDATA}/SegmC_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultPartialImage2.nrrd
    10
    1000
    4200
    0
    0.4
    4
)

itk_add_test(
  NAME ScalarImageToRunLengthFeaturesImageFilterTestWholeImage
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{${INPUTDATA}/resultWholeImage.nrrd}
    ${TEMP}/resultWholeImage.nrrd
    ScalarImageToRunLengthFeaturesImageFilterTest
    DATA{${INPUTDATA}/Scan_CBCT_13R.nrrd}
    DATA{${INPUTDATA}/SegmC_CBCT_13R.nrrd}
    ${TEMP}/resultWholeImage.nrrd
    10
    0
    4200
    0
    0.4
    2
)

itk_add_test(
  NAME ScalarImageToRunLengthFeaturesImageFilterTestSeparateFeatures
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{${INPUTDATA}/resultSeparateFeatures_1.nrrd}
    ${TEMP}/resultSeparateFeatures_1.nrrd
    --compare
    DATA{${INPUTDATA}/resultSeparateFeatures_2.nrrd}
    ${TEMP}/resultSeparateFeatures_2.nrrd
    --compare
    DATA{${INPUTDATA}/resultSeparateFeatures_3.nrrd}
    ${TEMP}/resultSeparateFeatures_3.nrrd
    --compare
    DATA{${INPUTDATA}/resultSeparateFeatures_4.nrrd}
    ${TEMP}/resultSeparateFeatures_4.nrrd
    --compare
    DATA{${INPUTDATA}/resultSeparateFeatures_5.nrrd}
    ${TEMP}/resultSeparateFeatures_5.nrrd
    --compare
    DATA{${INPUTDATA}/resultSeparateFeatures_6.nrrd}
    ${TEMP}/resultSeparateFeatures_6.nrrd
    --compare
    DATA{${INPUTDATA}/resultSeparateFeatures_7.nrrd}
    ${TEMP}/resultSeparateFeatures_7.nrrd
    --compare
    DATA{${INPUTDATA}/resultSeparateFeatures_8.nrrd}
    ${TEMP}/resultSeparateFeatures_8.nrrd
    --compare
    DATA{${INPUTDATA}/resultSeparateFeatures_9.nrrd}
    ${TEMP}/resultSeparateFeatures_9.nrrd
    --compare
    DATA{${INPUTDATA}/resultSeparateFeatures_0.nrrd}
    ${TEMP}/resultSeparateFeatures_0.nrrd
    ScalarImageToRunLengthFeaturesImageFilterTestSeparateFeatures
    DATA{${INPUTDATA}/Scan_CBCT_13R.nrrd}
    DATA{${INPUTDATA}/SegmC_CBCT_13R.nrrd}
    ${TEMP}/resultSeparateFeatures
    10
    0
    4200
    0
    0.4
    2
)
