itk_module_test()
set(ITKIOPNGTests
itkPNGImageIOTest.cxx
itkPNGImageIOTest2.cxx
itkPNGImageIOTestPalette.cxx
)

CreateTestDriver(ITKIOPNG  "${ITKIOPNG-Test_LIBRARIES}" "${ITKIOPNGTests}")

itk_add_test(NAME itkPNGImageIOTest1Grey
      COMMAND ITKIOPNGTestDriver
    itkPNGImageIOTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest1Grey.png 0 4 1)
itk_add_test(NAME itkPNGImageIOTest1Color
      COMMAND ITKIOPNGTestDriver
    itkPNGImageIOTest DATA{${ITK_DATA_ROOT}/Input/peppers.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest1Color.png 0 4 1)
itk_add_test(NAME itkPNGImageIOTest1Palette
      COMMAND ITKIOPNGTestDriver
    itkPNGImageIOTest DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest1Palette.png 0 4 1)
itk_add_test(NAME itkPNGImageIOTest1Palette2Scalar
      COMMAND ITKIOPNGTestDriver
    itkPNGImageIOTest DATA{Input/HeliconiusNumataPalette.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest1Palette2Scalar.png 1 9 0)
itk_add_test(NAME itkPNGImageIOTest1PaletteExpanded
      COMMAND ITKIOPNGTestDriver
    itkPNGImageIOTest DATA{Input/HeliconiusNumataPalette.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest1PaletteExpanded.png 1 9 1)
itk_add_test(NAME itkPNGImageIOTest2GreyAlpha
      COMMAND ITKIOPNGTestDriver
    --compare DATA{Baseline/itkPNGImageIOTest2GreyAlpha.png}
              ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest2GreyAlpha.png
    itkPNGImageIOTest2 DATA{Input/GrayAlpha.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest2GreyAlpha.png 0 4 1)
itk_add_test(NAME itkPNGImageIOTest2Palette
      COMMAND ITKIOPNGTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest2Palette.png
              4a4133ec26e5c83a5cbd9188067b1633
    itkPNGImageIOTest2 DATA{Input/HeliconiusNumataPalette.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest2Palette.png 1 9 0)
itk_add_test(NAME itkPNGImageIOTest3PaletteExpanded
      COMMAND ITKIOPNGTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3PaletteExpanded.png
              1e1a89a70b7cb472f55c450909df7b77
    itkPNGImageIOTestPalette DATA{Input/HeliconiusNumataPalette.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3PaletteExpanded.png 1 0)
itk_add_test(NAME itkPNGImageIOTest3PaletteNotExpanded
      COMMAND ITKIOPNGTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3PaletteNotExpanded.png
              4a4133ec26e5c83a5cbd9188067b1633
    itkPNGImageIOTestPalette DATA{Input/HeliconiusNumataPalette.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3PaletteNotExpanded.png 0 1)
itk_add_test(NAME itkPNGImageIOTest3PaletteNotExpandedGrey
      COMMAND ITKIOPNGTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3PaletteNotExpandedGrey.png
              4a4133ec26e5c83a5cbd9188067b1633
    itkPNGImageIOTestPalette DATA{Input/HeliconiusNumataPalette.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3PaletteNotExpandedGrey.png 0 0)
