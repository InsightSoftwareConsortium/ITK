itk_module_test()
set(ITKIOBMPTests
itkBMPImageIOTest2.cxx
itkBMPImageIOTest.cxx
)

CreateTestDriver(ITKIOBMP  "${ITKIOBMP-Test_LIBRARIES}" "${ITKIOBMPTests}")

itk_add_test(NAME itkBMPImageIOTest5
      COMMAND ITKIOBMPTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/smallRGBA.mha
              ${ITK_TEST_OUTPUT_DIR}/smallRGBA.mha
    itkBMPImageIOTest2 ${ITK_DATA_ROOT}/Input/smallRGBA.bmp ${ITK_TEST_OUTPUT_DIR}/smallRGBA.mha)
itk_add_test(NAME itkBMPImageIOTest6
      COMMAND ITKIOBMPTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/smallRGBA.mha
              ${ITK_TEST_OUTPUT_DIR}/smallRGBA.bmp
    itkBMPImageIOTest2 ${ITK_DATA_ROOT}/Input/smallRGBA.bmp ${ITK_TEST_OUTPUT_DIR}/smallRGBA.bmp)
itk_add_test(NAME itkBMPImageIOTest
      COMMAND ITKIOBMPTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.bmp
              ${ITK_TEST_OUTPUT_DIR}/cthead1.bmp
    itkBMPImageIOTest ${ITK_DATA_ROOT}/Input/cthead1.bmp ${ITK_TEST_OUTPUT_DIR}/cthead1.bmp)
itk_add_test(NAME itkBMPImageIOTest2
      COMMAND ITKIOBMPTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead254x254.bmp
              ${ITK_TEST_OUTPUT_DIR}/cthead254x254.bmp
    itkBMPImageIOTest ${ITK_DATA_ROOT}/Input/cthead254x254.bmp ${ITK_TEST_OUTPUT_DIR}/cthead254x254.bmp)
itk_add_test(NAME itkBMPImageIOTest3
      COMMAND ITKIOBMPTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/image_grayscale.bmp
              ${ITK_TEST_OUTPUT_DIR}/image_grayscale.bmp
    itkBMPImageIOTest ${ITK_DATA_ROOT}/Input/image_grayscale.bmp ${ITK_TEST_OUTPUT_DIR}/image_grayscale.bmp)
itk_add_test(NAME itkBMPImageIOTest4
      COMMAND ITKIOBMPTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/image_color.bmp
              ${ITK_TEST_OUTPUT_DIR}/image_color.bmp
    itkBMPImageIOTest ${ITK_DATA_ROOT}/Input/image_color.bmp ${ITK_TEST_OUTPUT_DIR}/image_color.bmp)
