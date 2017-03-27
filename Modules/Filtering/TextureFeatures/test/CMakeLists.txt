itk_module_test()

set(INPUT_IMAGE ${CMAKE_CURRENT_SOURCE_DIR}/Baseline/CroppedVolume.nrrd)

set(ITK_TEST_DRIVER itkTestDriver)

set(TextureFeaturesTests FilterCreationTest.cxx)

set(DART_TESTING_TIMEOUT 9000)
set(CTEST_TEST_TIMEOUT 9000)

createtestdriver(TextureFeatures "${TextureFeatures-Test_LIBRARIES}" "${TextureFeaturesTests}")

itk_add_test(
  NAME TextureFeaturesTest1
  COMMAND
    TextureFeaturesTestDriver
    --compare
    ${CMAKE_CURRENT_SOURCE_DIR}/Baseline/Crop.nrrd
    ${CMAKE_CURRENT_SOURCE_DIR}/Baseline/Crop.nrrd
    FilterCreationTest
    ${INPUT_IMAGE}
)
