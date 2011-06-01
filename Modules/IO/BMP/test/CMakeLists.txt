itk_module_test()
set(ITK-IO-BMPTests
itkBMPImageIOTest2.cxx
itkIOBMPHeaderTest.cxx
itkBMPImageIOTest.cxx
)

CreateTestDriver(ITK-IO-BMP  "${ITK-IO-BMP-Test_LIBRARIES}" "${ITK-IO-BMPTests}")

itk_add_test(NAME itkIOBMPHeaderTest
      COMMAND ITK-IO-BMPTestDriver itkIOBMPHeaderTest)
itk_add_test(NAME itkBMPImageIOTest5
      COMMAND ITK-IO-BMPTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/smallRGBA.mha
              ${ITK_TEST_OUTPUT_DIR}/smallRGBA.mha
    itkBMPImageIOTest2 ${ITK_DATA_ROOT}/Input/smallRGBA.bmp ${ITK_TEST_OUTPUT_DIR}/smallRGBA.mha)
itk_add_test(NAME itkBMPImageIOTest6
      COMMAND ITK-IO-BMPTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/smallRGBA.mha
              ${ITK_TEST_OUTPUT_DIR}/smallRGBA.bmp
    itkBMPImageIOTest2 ${ITK_DATA_ROOT}/Input/smallRGBA.bmp ${ITK_TEST_OUTPUT_DIR}/smallRGBA.bmp)
itk_add_test(NAME itkBMPImageIOTest
      COMMAND ITK-IO-BMPTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.bmp
              ${ITK_TEST_OUTPUT_DIR}/cthead1.bmp
    itkBMPImageIOTest ${ITK_DATA_ROOT}/Input/cthead1.bmp ${ITK_TEST_OUTPUT_DIR}/cthead1.bmp)
itk_add_test(NAME itkBMPImageIOTest2
      COMMAND ITK-IO-BMPTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead254x254.bmp
              ${ITK_TEST_OUTPUT_DIR}/cthead254x254.bmp
    itkBMPImageIOTest ${ITK_DATA_ROOT}/Input/cthead254x254.bmp ${ITK_TEST_OUTPUT_DIR}/cthead254x254.bmp)
itk_add_test(NAME itkBMPImageIOTest3
      COMMAND ITK-IO-BMPTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/image_grayscale.bmp
              ${ITK_TEST_OUTPUT_DIR}/image_grayscale.bmp
    itkBMPImageIOTest ${ITK_DATA_ROOT}/Input/image_grayscale.bmp ${ITK_TEST_OUTPUT_DIR}/image_grayscale.bmp)
itk_add_test(NAME itkBMPImageIOTest4
      COMMAND ITK-IO-BMPTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/image_color.bmp
              ${ITK_TEST_OUTPUT_DIR}/image_color.bmp
    itkBMPImageIOTest ${ITK_DATA_ROOT}/Input/image_color.bmp ${ITK_TEST_OUTPUT_DIR}/image_color.bmp)
