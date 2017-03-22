itk_module_test()

set(INPUT_IMAGE ${CMAKE_CURRENT_SOURCE_DIR}/Baseline/Scan_CBCT_1L.nrrd)

set(ITK_TEST_DRIVER itkTestDriver)

set(TextureFeaturesTests FilterCreationTest.cxx)

createtestdriver(TextureFeatures "${ITKTextureFeatures-Test_LIBRARIES}" "${TextureFeaturesTests}")

itk_add_test(
  NAME TextureFeaturesTest1
  COMMAND
    TextureFeaturesTestDriver
    --compare
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/Scan_CBCT_1L.nrrd
    ${CMAKE_CURRENT_SOURCE_DIR}/baseline/SegmC_CBCT_1L.nrrd
    FilterCreationTest
    ${INPUT_IMAGE}
)
