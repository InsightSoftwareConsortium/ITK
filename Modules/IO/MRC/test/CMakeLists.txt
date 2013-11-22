itk_module_test()
set(ITKIOMRCTests
itkMRCImageIOTest.cxx
itkMRCImageIOTest2.cxx
)


CreateTestDriver(ITKIOMRC  "${ITKIOMRC-Test_LIBRARIES}" "${ITKIOMRCTests}")

itk_add_test(NAME itkMRCImageIOTest
      COMMAND ITKIOMRCTestDriver itkMRCImageIOTest
              ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkMRCImageIOTest2a
      COMMAND ITKIOMRCTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkMRCImageIOTest2a.mrc
                  5eac5b2827533e8df8b061ff0ab33718
    itkMRCImageIOTest2
        DATA{${ITK_DATA_ROOT}/Input/tilt_series_little.mrc}
        ${ITK_TEST_OUTPUT_DIR}/itkMRCImageIOTest2a.mrc
        5eac5b2827533e8df8b061ff0ab33718)
itk_add_test(NAME itkMRCImageIOTest2b
      COMMAND ITKIOMRCTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkMRCImageIOTest2b.mrc
                  5eac5b2827533e8df8b061ff0ab33718
    itkMRCImageIOTest2
        DATA{${ITK_DATA_ROOT}/Input/tilt_series_big.mrc}
        ${ITK_TEST_OUTPUT_DIR}/itkMRCImageIOTest2b.mrc
        5eac5b2827533e8df8b061ff0ab33718)
itk_add_test(NAME itkMRCImageIOTest2c
  COMMAND ITKIOMRCTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkMRCImageIOTest2c.mrc
                  38a2dcfc08812e04b1528f15ca3d2ab7
    itkMRCImageIOTest2
        DATA{${ITK_DATA_ROOT}/Input/RGBTestImageLZW.tif}
        ${ITK_TEST_OUTPUT_DIR}/itkMRCImageIOTest2c.mrc
        38a2dcfc08812e04b1528f15ca3d2ab7)
