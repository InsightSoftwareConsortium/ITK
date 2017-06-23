itk_module_test()

set(INPUTDATA ${CMAKE_CURRENT_SOURCE_DIR}/Data)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

set(ITK_TEST_DRIVER itkTestDriver)

set(
  TextureFeaturesTests
  RunLengthTextureFeaturesImageFilterInstantiationTest.cxx
  RunLengthTextureFeaturesImageFilterTest.cxx
  RunLengthTextureFeaturesImageFilterTestSeparateFeatures.cxx
  RunLengthTextureFeaturesImageFilterTestWithoutMask.cxx
  RunLengthTextureFeaturesImageFilterTestWithVectorImage.cxx
  RunLengthTextureFeaturesImageFilterTestVectorImageSeparateFeatures.cxx
  CoocurenceTextureFeaturesImageFilterInstantiationTest.cxx
  CoocurenceTextureFeaturesImageFilterTest.cxx
  CoocurenceTextureFeaturesImageFilterTestSeparateFeatures.cxx
  CoocurenceTextureFeaturesImageFilterTestWithoutMask.cxx
  CoocurenceTextureFeaturesImageFilterTestWithVectorImage.cxx
  CoocurenceTextureFeaturesImageFilterTestVectorImageSeparateFeatures.cxx
)

set(TestOutput ${ITK_TEST_OUTPUT_DIR})

createtestdriver(TextureFeatures "${TextureFeatures-Test_LIBRARIES}" "${TextureFeaturesTests}")

itk_add_test(
  NAME RunLengthTextureFeaturesImageFilterInstantiationTest
  COMMAND
    TextureFeaturesTestDriver
    RunLengthTextureFeaturesImageFilterInstantiationTest
    DATA{Input/Scan_CBCT_13R.nrrd}
    DATA{Input/SegmC_CBCT_13R.nrrd}
)

itk_add_test(
  NAME RunLengthTextureFeaturesImageFilterTestWithoutMask1
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultTestWithoutMask1.nrrd}
    ${TEMP}/resultTestWithoutMask1.nrrd
    RunLengthTextureFeaturesImageFilterTestWithoutMask
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultTestWithoutMask1.nrrd
    10
    0
    4200
    0
    0.7
    2
)

itk_add_test(
  NAME RunLengthTextureFeaturesImageFilterTestWithoutMask2
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultTestWithoutMask2.nrrd}
    ${TEMP}/resultTestWithoutMask2.nrrd
    RunLengthTextureFeaturesImageFilterTestWithoutMask
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultTestWithoutMask2.nrrd
    10
    0
    4200
    0
    1.25
    4
)

itk_add_test(
  NAME RunLengthTextureFeaturesImageFilterTestWithoutMask3
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultTestWithoutMask3.nrrd}
    ${TEMP}/resultTestWithoutMask3.nrrd
    RunLengthTextureFeaturesImageFilterTestWithoutMask
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultTestWithoutMask3.nrrd
    10
    0
    4200
    0
    1.8
    6
)

itk_add_test(
  NAME RunLengthTextureFeaturesImageFilterVectorlImage1
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultPartialImage1.nrrd}
    ${TEMP}/resultVectorImage1.nrrd
    RunLengthTextureFeaturesImageFilterTestWithVectorImage
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultVectorImage1.nrrd
    10
    0
    4200
    0
    0.7
    2
)

itk_add_test(
  NAME RunLengthTextureFeaturesImageFilterVectorImage2
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultPartialImage2.nrrd}
    ${TEMP}/resultVectorImage2.nrrd
    RunLengthTextureFeaturesImageFilterTestWithVectorImage
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultVectorImage2.nrrd
    10
    0
    4200
    0
    1.25
    4
)

itk_add_test(
  NAME RunLengthTextureFeaturesImageFilterPartialImage1
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultPartialImage1.nrrd}
    ${TEMP}/resultPartialImage1.nrrd
    RunLengthTextureFeaturesImageFilterTest
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultPartialImage1.nrrd
    10
    0
    4200
    0
    0.7
    2
)

itk_add_test(
  NAME RunLengthTextureFeaturesImageFilterPartialImage2
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultPartialImage2.nrrd}
    ${TEMP}/resultPartialImage2.nrrd
    RunLengthTextureFeaturesImageFilterTest
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultPartialImage2.nrrd
    10
    0
    4200
    0
    1.25
    4
)

itk_add_test(
  NAME RunLengthTextureFeaturesImageFilterTestWholeImage
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultWholeImage.nrrd}
    ${TEMP}/resultWholeImage.nrrd
    RunLengthTextureFeaturesImageFilterTest
    DATA{Input/Scan_CBCT_13R.nrrd}
    DATA{Input/SegmC_CBCT_13R.nrrd}
    ${TEMP}/resultWholeImage.nrrd
    10
    0
    4200
    0
    0.7
    2
)

itk_add_test(
  NAME RunLengthTextureFeaturesImageFilterTestSeparateFeatures
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultSeparateFeatures_1.nrrd}
    ${TEMP}/resultSeparateFeatures_1.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_2.nrrd}
    ${TEMP}/resultSeparateFeatures_2.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_3.nrrd}
    ${TEMP}/resultSeparateFeatures_3.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_4.nrrd}
    ${TEMP}/resultSeparateFeatures_4.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_5.nrrd}
    ${TEMP}/resultSeparateFeatures_5.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_6.nrrd}
    ${TEMP}/resultSeparateFeatures_6.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_7.nrrd}
    ${TEMP}/resultSeparateFeatures_7.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_8.nrrd}
    ${TEMP}/resultSeparateFeatures_8.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_9.nrrd}
    ${TEMP}/resultSeparateFeatures_9.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_0.nrrd}
    ${TEMP}/resultSeparateFeatures_0.nrrd
    RunLengthTextureFeaturesImageFilterTestSeparateFeatures
    DATA{Input/Scan_CBCT_13R.nrrd}
    DATA{Input/SegmC_CBCT_13R.nrrd}
    ${TEMP}/resultSeparateFeatures
    10
    0
    4200
    0
    1.25
    4
)

itk_add_test(
  NAME RunLengthTextureFeaturesImageFilterTestVectorImageSeparateFeatures
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultSeparateFeatures_1.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_1.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_2.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_2.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_3.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_3.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_4.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_4.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_5.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_5.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_6.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_6.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_7.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_7.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_8.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_8.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_9.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_9.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_0.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_0.nrrd
    RunLengthTextureFeaturesImageFilterTestVectorImageSeparateFeatures
    DATA{Input/Scan_CBCT_13R.nrrd}
    DATA{Input/SegmC_CBCT_13R.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures
    10
    0
    4200
    0
    1.25
    4
)

itk_add_test(
  NAME CoocurenceTextureFeaturesImageFilterInstantiationTest
  COMMAND
    TextureFeaturesTestDriver
    CoocurenceTextureFeaturesImageFilterInstantiationTest
    DATA{Input/Scan_CBCT_13R.nrrd}
    DATA{Input/SegmC_CBCT_13R.nrrd}
)

itk_add_test(
  NAME CoocurenceTextureFeaturesImageFilterTestWithoutMask1
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultTestWithoutMask4.nrrd}
    ${TEMP}/resultTestWithoutMask4.nrrd
    CoocurenceTextureFeaturesImageFilterTestWithoutMask
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultTestWithoutMask4.nrrd
    10
    0
    4200
    2
)

itk_add_test(
  NAME CoocurenceTextureFeaturesImageFilterTestWithoutMask2
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultTestWithoutMask5.nrrd}
    ${TEMP}/resultTestWithoutMask5.nrrd
    CoocurenceTextureFeaturesImageFilterTestWithoutMask
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultTestWithoutMask5.nrrd
    10
    0
    4200
    4
)

itk_add_test(
  NAME CoocurenceTextureFeaturesImageFilterTestWithoutMask3
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultTestWithoutMask6.nrrd}
    ${TEMP}/resultTestWithoutMask6.nrrd
    CoocurenceTextureFeaturesImageFilterTestWithoutMask
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultTestWithoutMask6.nrrd
    10
    0
    4200
    6
)

itk_add_test(
  NAME CoocurenceTextureFeaturesImageFilterVectorlImage1
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultPartialImage3.nrrd}
    ${TEMP}/resultVectorImage3.nrrd
    CoocurenceTextureFeaturesImageFilterTestWithVectorImage
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultVectorImage3.nrrd
    10
    0
    4200
    2
)

itk_add_test(
  NAME CoocurenceTextureFeaturesImageFilterVectorImage2
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultPartialImage4.nrrd}
    ${TEMP}/resultVectorImage4.nrrd
    CoocurenceTextureFeaturesImageFilterTestWithVectorImage
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultVectorImage4.nrrd
    10
    0
    4200
    4
)

itk_add_test(
  NAME CoocurenceTextureFeaturesImageFilterPartialImage1
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultPartialImage3.nrrd}
    ${TEMP}/resultPartialImage3.nrrd
    CoocurenceTextureFeaturesImageFilterTest
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultPartialImage3.nrrd
    10
    0
    4200
    2
)

itk_add_test(
  NAME CoocurenceTextureFeaturesImageFilterPartialImage2
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultPartialImage4.nrrd}
    ${TEMP}/resultPartialImage4.nrrd
    CoocurenceTextureFeaturesImageFilterTest
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${TEMP}/resultPartialImage4.nrrd
    10
    0
    4200
    4
)

itk_add_test(
  NAME CoocurenceTextureFeaturesImageFilterTestWholeImage
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultWholeImage2.nrrd}
    ${TEMP}/resultWholeImage2.nrrd
    CoocurenceTextureFeaturesImageFilterTest
    DATA{Input/Scan_CBCT_13R.nrrd}
    DATA{Input/SegmC_CBCT_13R.nrrd}
    ${TEMP}/resultWholeImage2.nrrd
    10
    0
    4200
    2
)

itk_add_test(
  NAME CoocurenceTextureFeaturesImageFilterTestSeparateFeatures
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultSeparateFeatures_11.nrrd}
    ${TEMP}/resultSeparateFeatures_11.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_12.nrrd}
    ${TEMP}/resultSeparateFeatures_12.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_13.nrrd}
    ${TEMP}/resultSeparateFeatures_13.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_14.nrrd}
    ${TEMP}/resultSeparateFeatures_14.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_15.nrrd}
    ${TEMP}/resultSeparateFeatures_15.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_16.nrrd}
    ${TEMP}/resultSeparateFeatures_16.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_17.nrrd}
    ${TEMP}/resultSeparateFeatures_17.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_18.nrrd}
    ${TEMP}/resultSeparateFeatures_18.nrrd
    CoocurenceTextureFeaturesImageFilterTestSeparateFeatures
    DATA{Input/Scan_CBCT_13R.nrrd}
    DATA{Input/SegmC_CBCT_13R.nrrd}
    ${TEMP}/resultSeparateFeatures
    10
    0
    4200
    4
)

itk_add_test(
  NAME CoocurenceTextureFeaturesImageFilterTestVectorImageSeparateFeatures
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultSeparateFeatures_11.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_11.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_12.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_12.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_13.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_13.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_14.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_14.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_15.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_15.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_16.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_16.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_17.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_17.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_18.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures_18.nrrd
    CoocurenceTextureFeaturesImageFilterTestVectorImageSeparateFeatures
    DATA{Input/Scan_CBCT_13R.nrrd}
    DATA{Input/SegmC_CBCT_13R.nrrd}
    ${TEMP}/resultVectorImageSeparateFeatures
    10
    0
    4200
    4
)
