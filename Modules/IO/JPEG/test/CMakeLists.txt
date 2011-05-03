itk_module_test()
set(ITK-IO-JPEGTests
itkIOJPEGHeaderTest.cxx
itkJPEGImageIOTest.cxx
)

CreateTestDriver(ITK-IO-JPEG  "${ITK-IO-JPEG-Test_LIBRARIES}" "${ITK-IO-JPEGTests}")

itk_add_test(NAME itkIOJPEGHeaderTest
      COMMAND ITK-IO-JPEGTestDriver itkIOJPEGHeaderTest)
itk_add_test(NAME itkJPEGImageIOTest
      COMMAND ITK-IO-JPEGTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1.png
    itkJPEGImageIOTest ${ITK_DATA_ROOT}/Input/cthead1.jpg ${ITK_TEST_OUTPUT_DIR}/cthead1.png)
itk_add_test(NAME itkJPEGImageIOTest2
      COMMAND ITK-IO-JPEGTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.jpg
              ${ITK_TEST_OUTPUT_DIR}/cthead1.jpg
    itkJPEGImageIOTest ${ITK_DATA_ROOT}/Input/cthead1.jpg ${ITK_TEST_OUTPUT_DIR}/cthead1.jpg)
