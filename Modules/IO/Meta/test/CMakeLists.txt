itk_module_test()
set(ITK-IO-MetaTests
itkMetaImageIOMetaDataTest.cxx
itkIOMetaHeaderTest.cxx
itkMetaImageIOGzTest.cxx
itkMetaImageIOTest.cxx
)

CreateTestDriver(ITK-IO-Meta  "${ITK-IO-Meta-Test_LIBRARIES}" "${ITK-IO-MetaTests}")

add_test(NAME itkIOMetaHeaderTest
      COMMAND ITK-IO-MetaTestDriver itkIOMetaHeaderTest)
add_test(NAME itkMetaImageIOMetaDataTest
      COMMAND ITK-IO-MetaTestDriver itkMetaImageIOMetaDataTest
              ${ITK_TEST_OUTPUT_DIR}/MetaImageIOMetaDataTest.mhd)
add_test(NAME itkMetaImageIOGzTest
      COMMAND ITK-IO-MetaTestDriver itkMetaImageIOGzTest
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME itkMetaImageIOTest
      COMMAND ITK-IO-MetaTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/HeadMRVolume.mhd
              ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.mhd
    itkMetaImageIOTest ${ITK_DATA_ROOT}/Input/HeadMRVolume.mhd ${ITK_TEST_OUTPUT_DIR}/HeadMRVolume.mhd)
add_test(NAME itkMetaImageIOShouldFailTest
      COMMAND ITK-IO-MetaTestDriver itkMetaImageIOTest
              ${ITK_DATA_ROOT}/Input/MetaImageError.mhd 1)
