itk_module_test()
set(ITKIOJPEGTests
itkIOJPEGHeaderTest.cxx
itkJPEGImageIOTest.cxx
)

CreateTestDriver(ITKIOJPEG  "${ITKIOJPEG-Test_LIBRARIES}" "${ITKIOJPEGTests}")

itk_add_test(NAME itkIOJPEGHeaderTest
      COMMAND ITKIOJPEGTestDriver itkIOJPEGHeaderTest)
itk_add_test(NAME itkJPEGImageIOTest
      COMMAND ITKIOJPEGTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1.png
    itkJPEGImageIOTest ${ITK_DATA_ROOT}/Input/cthead1.jpg ${ITK_TEST_OUTPUT_DIR}/cthead1.png)
itk_add_test(NAME itkJPEGImageIOTest2
      COMMAND ITKIOJPEGTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/IO/cthead1.jpg
              ${ITK_TEST_OUTPUT_DIR}/cthead1.jpg
    itkJPEGImageIOTest ${ITK_DATA_ROOT}/Input/cthead1.jpg ${ITK_TEST_OUTPUT_DIR}/cthead1.jpg)
