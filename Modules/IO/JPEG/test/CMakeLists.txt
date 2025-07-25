itk_module_test()
set(
  ITKIOJPEGTests
  itkJPEGImageIOTest.cxx
  itkJPEGImageIOTest2.cxx
  itkJPEGImageIODegenerateCasesTest.cxx
  itkJPEGImageIOBrokenCasesTest.cxx
  itkJPEGImageIOCMYKTest.cxx
)

createtestdriver(ITKIOJPEG "${ITKIOJPEG-Test_LIBRARIES}" "${ITKIOJPEGTests}")

itk_add_test(
  NAME itkJPEGImageIOTest
  COMMAND
    ITKIOJPEGTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Baseline/IO/cthead1.png}
    ${ITK_TEST_OUTPUT_DIR}/cthead1.png
    itkJPEGImageIOTest
    DATA{${ITK_DATA_ROOT}/Input/cthead1.jpg}
    ${ITK_TEST_OUTPUT_DIR}/cthead1.png
)
itk_add_test(
  NAME itkJPEGImageIOTest2
  COMMAND
    ITKIOJPEGTestDriver
    --compare
    DATA{${ITK_DATA_ROOT}/Baseline/IO/cthead1.jpg}
    ${ITK_TEST_OUTPUT_DIR}/cthead1.jpg
    itkJPEGImageIOTest
    DATA{${ITK_DATA_ROOT}/Input/cthead1.jpg}
    ${ITK_TEST_OUTPUT_DIR}/cthead1.jpg
)
itk_add_test(
  NAME itkJPEGImageIOSpacing
  COMMAND
    ITKIOJPEGTestDriver
    itkJPEGImageIOTest2
    ${ITK_TEST_OUTPUT_DIR}/itkJPEGImageIOSpacing.jpg
)
itk_add_test(
  NAME itkJPEGImageIOTestCorruptedImage
  COMMAND
    ITKIOJPEGTestDriver
    itkJPEGImageIODegenerateCasesTest
    DATA{Input/corrupted_image.jpg}
)
itk_add_test(
  NAME itkJPEGImageIOTestCorruptedImage2
  COMMAND
    ITKIOJPEGTestDriver
    itkJPEGImageIOBrokenCasesTest
    DATA{Input/corrupted2.jpg}
)
itk_add_test(
  NAME itkJPEGImageIOTestCMYKImage
  COMMAND
    ITKIOJPEGTestDriver
    itkJPEGImageIOCMYKTest
    DATA{Input/cmyk.jpg}
)
