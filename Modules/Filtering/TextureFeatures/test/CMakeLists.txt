itk_module_test()

set(
  TextureFeaturesTests
  ScalarImageToRunLengthFeaturesImageFilterInstantiationTest.cxx
  ScalarImageToRunLengthFeaturesImageFilterTest.cxx
  ScalarImageToRunLengthFeaturesImageFilterTestSeparateFeatures.cxx
  ScalarImageToRunLengthFeaturesImageFilterTestWithoutMask.cxx
  ScalarImageToRunLengthFeaturesImageFilterTestWithVectorImage.cxx
  ScalarImageToRunLengthFeaturesImageFilterTestVectorImageSeparateFeatures.cxx
  ScalarImageToTextureFeaturesImageFilterInstantiationTest.cxx
  ScalarImageToTextureFeaturesImageFilterTest.cxx
  ScalarImageToTextureFeaturesImageFilterTestSeparateFeatures.cxx
  ScalarImageToTextureFeaturesImageFilterTestWithoutMask.cxx
  ScalarImageToTextureFeaturesImageFilterTestWithVectorImage.cxx
)

set(
  TextureFeaturesTests
  RunLengthTextureFeaturesImageFilterInstantiationTest.cxx
  RunLengthTextureFeaturesImageFilterTest.cxx
  RunLengthTextureFeaturesImageFilterTestSeparateFeatures.cxx
  RunLengthTextureFeaturesImageFilterTestWithoutMask.cxx
  RunLengthTextureFeaturesImageFilterTestWithVectorImage.cxx
  RunLengthTextureFeaturesImageFilterTestVectorImageSeparateFeatures.cxx
  CoocurrenceTextureFeaturesImageFilterInstantiationTest.cxx
  CoocurrenceTextureFeaturesImageFilterTest.cxx
  CoocurrenceTextureFeaturesImageFilterTestSeparateFeatures.cxx
  CoocurrenceTextureFeaturesImageFilterTestWithoutMask.cxx
  CoocurrenceTextureFeaturesImageFilterTestWithVectorImage.cxx
  CoocurrenceTextureFeaturesImageFilterTestVectorImageSeparateFeatures.cxx
  itkFirstOrderTextureFeaturesImageFilterTest.cxx
)

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
    ${ITK_TEST_OUTPUT_DIR}/resultTestWithoutMask1.nrrd
    RunLengthTextureFeaturesImageFilterTestWithoutMask
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultTestWithoutMask1.nrrd
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
    ${ITK_TEST_OUTPUT_DIR}/resultTestWithoutMask2.nrrd
    RunLengthTextureFeaturesImageFilterTestWithoutMask
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultTestWithoutMask2.nrrd
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
    ${ITK_TEST_OUTPUT_DIR}/resultTestWithoutMask3.nrrd
    RunLengthTextureFeaturesImageFilterTestWithoutMask
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultTestWithoutMask3.nrrd
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
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImage1.nrrd
    RunLengthTextureFeaturesImageFilterTestWithVectorImage
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImage1.nrrd
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
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImage2.nrrd
    RunLengthTextureFeaturesImageFilterTestWithVectorImage
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImage2.nrrd
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
    ${ITK_TEST_OUTPUT_DIR}/resultPartialImage1.nrrd
    RunLengthTextureFeaturesImageFilterTest
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultPartialImage1.nrrd
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
    ${ITK_TEST_OUTPUT_DIR}/resultPartialImage2.nrrd
    RunLengthTextureFeaturesImageFilterTest
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultPartialImage2.nrrd
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
    ${ITK_TEST_OUTPUT_DIR}/resultWholeImage.nrrd
    RunLengthTextureFeaturesImageFilterTest
    DATA{Input/Scan_CBCT_13R.nrrd}
    DATA{Input/SegmC_CBCT_13R.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultWholeImage.nrrd
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
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_1.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_2.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_2.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_3.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_3.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_4.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_4.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_5.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_5.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_6.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_6.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_7.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_7.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_8.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_8.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_9.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_9.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_0.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_0.nrrd
    RunLengthTextureFeaturesImageFilterTestSeparateFeatures
    DATA{Input/Scan_CBCT_13R.nrrd}
    DATA{Input/SegmC_CBCT_13R.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures
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
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_1.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_2.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_2.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_3.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_3.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_4.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_4.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_5.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_5.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_6.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_6.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_7.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_7.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_8.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_8.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_9.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_9.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_0.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_0.nrrd
    RunLengthTextureFeaturesImageFilterTestVectorImageSeparateFeatures
    DATA{Input/Scan_CBCT_13R.nrrd}
    DATA{Input/SegmC_CBCT_13R.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures
    10
    0
    4200
    0
    1.25
    4
)

itk_add_test(
  NAME CoocurrenceTextureFeaturesImageFilterInstantiationTest
  COMMAND
    TextureFeaturesTestDriver
    CoocurrenceTextureFeaturesImageFilterInstantiationTest
    DATA{Input/Scan_CBCT_13R.nrrd}
    DATA{Input/SegmC_CBCT_13R.nrrd}
)

itk_add_test(
  NAME CoocurrenceTextureFeaturesImageFilterTestWithoutMask1
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultTestWithoutMask4.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultTestWithoutMask4.nrrd
    CoocurrenceTextureFeaturesImageFilterTestWithoutMask
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultTestWithoutMask4.nrrd
    10
    0
    4200
    2
)

itk_add_test(
  NAME CoocurrenceTextureFeaturesImageFilterTestWithoutMask2
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultTestWithoutMask5.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultTestWithoutMask5.nrrd
    CoocurrenceTextureFeaturesImageFilterTestWithoutMask
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultTestWithoutMask5.nrrd
    10
    0
    4200
    4
)

itk_add_test(
  NAME CoocurrenceTextureFeaturesImageFilterTestWithoutMask3
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultTestWithoutMask6.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultTestWithoutMask6.nrrd
    CoocurrenceTextureFeaturesImageFilterTestWithoutMask
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultTestWithoutMask6.nrrd
    10
    0
    4200
    6
)

itk_add_test(
  NAME CoocurrenceTextureFeaturesImageFilterVectorlImage1
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultPartialImage3.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImage3.nrrd
    CoocurrenceTextureFeaturesImageFilterTestWithVectorImage
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImage3.nrrd
    10
    0
    4200
    2
)

itk_add_test(
  NAME CoocurrenceTextureFeaturesImageFilterVectorImage2
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultPartialImage4.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImage4.nrrd
    CoocurrenceTextureFeaturesImageFilterTestWithVectorImage
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImage4.nrrd
    10
    0
    4200
    4
)

itk_add_test(
  NAME CoocurrenceTextureFeaturesImageFilterPartialImage1
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultPartialImage3.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultPartialImage3.nrrd
    CoocurrenceTextureFeaturesImageFilterTest
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultPartialImage3.nrrd
    10
    0
    4200
    2
)

itk_add_test(
  NAME CoocurrenceTextureFeaturesImageFilterPartialImage2
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultPartialImage4.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultPartialImage4.nrrd
    CoocurrenceTextureFeaturesImageFilterTest
    DATA{Input/Scan_CBCT_13R_D1_crop.nrrd}
    DATA{Input/SegmC_CBCT_13R_D1_crop.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultPartialImage4.nrrd
    10
    0
    4200
    4
)

itk_add_test(
  NAME CoocurrenceTextureFeaturesImageFilterTestWholeImage
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultWholeImage2.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultWholeImage2.nrrd
    CoocurrenceTextureFeaturesImageFilterTest
    DATA{Input/Scan_CBCT_13R.nrrd}
    DATA{Input/SegmC_CBCT_13R.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultWholeImage2.nrrd
    10
    0
    4200
    2
)

itk_add_test(
  NAME CoocurrenceTextureFeaturesImageFilterTestSeparateFeatures
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultSeparateFeatures_11.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_11.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_12.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_12.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_13.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_13.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_14.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_14.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_15.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_15.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_16.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_16.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_17.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_17.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_18.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures_18.nrrd
    CoocurrenceTextureFeaturesImageFilterTestSeparateFeatures
    DATA{Input/Scan_CBCT_13R.nrrd}
    DATA{Input/SegmC_CBCT_13R.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultSeparateFeatures
    10
    0
    4200
    4
)

itk_add_test(
  NAME CoocurrenceTextureFeaturesImageFilterTestVectorImageSeparateFeatures
  COMMAND
    TextureFeaturesTestDriver
    --compare
    DATA{Baseline/resultSeparateFeatures_11.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_11.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_12.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_12.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_13.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_13.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_14.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_14.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_15.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_15.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_16.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_16.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_17.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_17.nrrd
    --compare
    DATA{Baseline/resultSeparateFeatures_18.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures_18.nrrd
    CoocurrenceTextureFeaturesImageFilterTestVectorImageSeparateFeatures
    DATA{Input/Scan_CBCT_13R.nrrd}
    DATA{Input/SegmC_CBCT_13R.nrrd}
    ${ITK_TEST_OUTPUT_DIR}/resultVectorImageSeparateFeatures
    10
    0
    4200
    4
)

itk_add_test(
  NAME itkFirstOrderTextureFeaturesImageFilterTest1
  COMMAND
    TextureFeaturesTestDriver
    itkFirstOrderTextureFeaturesImageFilterTest
    DATA{Input/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/itkTextureFeatureImageFilterTest1.mha
    5
)

if(NOT "${ITK_VERSION_MAJOR}.${ITK_VERSION_MINOR}" VERSION_LESS "4.12")
  set(TextureFeaturesGTests itkFirstOrderTextureFeaturesImageFilterGTest.cxx)

  creategoogletestdriver(TextureFeatures "${TextureFeatures-Test_LIBRARIES}" "${TextureFeaturesGTests}")
endif()
