itk_module_test()
set(ITKIOPNGTests
itkPNGImageIOTest.cxx
itkPNGImageIOTest2.cxx
itkPNGImageIOTest3.cxx
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


# expand + RGB image
itk_add_test(NAME itkPNGImageIOTest3RGBExpanded
      COMMAND ITKIOPNGTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3RGBExpanded.png
              1e1a89a70b7cb472f55c450909df7b77
    itkPNGImageIOTestPalette DATA{Input/HeliconiusNumataPaletteRGB.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3RGBExpanded.png 1 0)

# not expand + RGB image
itk_add_test(NAME itkPNGImageIOTest3RGBNotExpanded
      COMMAND ITKIOPNGTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3RGBNotExpanded.png
              1e1a89a70b7cb472f55c450909df7b77
    itkPNGImageIOTestPalette DATA{Input/HeliconiusNumataPaletteRGB.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3RGBNotExpanded.png 0 0)

# not expand + palette
itk_add_test(NAME itkPNGImageIOTest3PaletteNotExpanded
      COMMAND ITKIOPNGTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3PaletteNotExpanded.png
              27c6dc313b42f2fd8a271c7b1c44765d
    itkPNGImageIOTestPalette DATA{Input/HeliconiusNumataPalette.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3PaletteNotExpanded.png 0 1)

# expand + palette image
itk_add_test(NAME itkPNGImageIOTest3PaletteExpanded
      COMMAND ITKIOPNGTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3PaletteExpanded.png
              1e1a89a70b7cb472f55c450909df7b77
    itkPNGImageIOTestPalette DATA{Input/HeliconiusNumataPalette.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3PaletteExpanded.png 1 1)

# not expand + grey palette image
itk_add_test(NAME itkPNGImageIOTest3GreyPaletteNotExpandedGrey
      COMMAND ITKIOPNGTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3GreyPaletteNotExpandedGrey.png
              63e9a4ca5f533f98a0ba2253036349f9
    itkPNGImageIOTestPalette DATA{Input/HeliconiusNumataPaletteGrey.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3GreyPaletteNotExpandedGrey.png 0 1)

# expand + grey palette image
itk_add_test(NAME itkPNGImageIOTest3GreyPaletteExpandedGrey
      COMMAND ITKIOPNGTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3GreyPaletteExpandedGrey.png
              a77e7f6919266c88e61ee7896c675044
    itkPNGImageIOTestPalette DATA{Input/HeliconiusNumataPaletteGrey.png} ${ITK_TEST_OUTPUT_DIR}/itkPNGImageIOTest3GreyPaletteExpandedGrey.png 1 1)

itk_add_test(NAME itkPNGImageIOTestCorrupt
      COMMAND ITKIOPNGTestDriver
    itkPNGImageIOTest3 DATA{Input/cthead1-257-corrupt.png})

itk_add_test(NAME itkPNGImageIOTestCorrupt2
      COMMAND ITKIOPNGTestDriver
    itkPNGImageIOTest3 DATA{Input/corrupted.png})
