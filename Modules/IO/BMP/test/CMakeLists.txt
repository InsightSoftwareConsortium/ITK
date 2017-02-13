itk_module_test()
set(ITKIOBMPTests
itkBMPImageIOTest2.cxx
itkBMPImageIOTest.cxx
itkBMPImageIOTest3.cxx
itkBMPImageIOTest4.cxx
itkBMPImageIOTest5.cxx
itkBMPImageIOTestPalette.cxx
)

CreateTestDriver(ITKIOBMP  "${ITKIOBMP-Test_LIBRARIES}" "${ITKIOBMPTests}")

itk_add_test(NAME itkBMPImageIOTest5
      COMMAND ITKIOBMPTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/smallRGBA.mha}
              ${ITK_TEST_OUTPUT_DIR}/smallRGBA.mha
    itkBMPImageIOTest2 DATA{${ITK_DATA_ROOT}/Input/smallRGBA.bmp} ${ITK_TEST_OUTPUT_DIR}/smallRGBA.mha)
itk_add_test(NAME itkBMPImageIOTest6
      COMMAND ITKIOBMPTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/smallRGBA.mha}
              ${ITK_TEST_OUTPUT_DIR}/smallRGBA.bmp
    itkBMPImageIOTest2 DATA{${ITK_DATA_ROOT}/Input/smallRGBA.bmp} ${ITK_TEST_OUTPUT_DIR}/smallRGBA.bmp)
itk_add_test(NAME itkBMPImageIOTest
      COMMAND ITKIOBMPTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/cthead1.bmp}
              ${ITK_TEST_OUTPUT_DIR}/cthead1.bmp
    itkBMPImageIOTest DATA{${ITK_DATA_ROOT}/Input/cthead1.bmp} ${ITK_TEST_OUTPUT_DIR}/cthead1.bmp)
itk_add_test(NAME itkBMPImageIOTest2
      COMMAND ITKIOBMPTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/cthead254x254.bmp}
              ${ITK_TEST_OUTPUT_DIR}/cthead254x254.bmp
    itkBMPImageIOTest DATA{${ITK_DATA_ROOT}/Input/cthead254x254.bmp} ${ITK_TEST_OUTPUT_DIR}/cthead254x254.bmp)
itk_add_test(NAME itkBMPImageIOTest3
      COMMAND ITKIOBMPTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/image_grayscale.bmp}
              ${ITK_TEST_OUTPUT_DIR}/image_grayscale.bmp
    itkBMPImageIOTest DATA{${ITK_DATA_ROOT}/Input/image_grayscale.bmp} ${ITK_TEST_OUTPUT_DIR}/image_grayscale.bmp)
itk_add_test(NAME itkBMPImageIOTest4
      COMMAND ITKIOBMPTestDriver
    --compare DATA{${ITK_DATA_ROOT}/Baseline/IO/image_color.bmp}
              ${ITK_TEST_OUTPUT_DIR}/image_color.bmp
    itkBMPImageIOTest DATA{${ITK_DATA_ROOT}/Input/image_color.bmp} ${ITK_TEST_OUTPUT_DIR}/image_color.bmp)
itk_add_test(NAME itkBMPImageIOTest7
      COMMAND ITKIOBMPTestDriver
    itkBMPImageIOTest3 DATA{${ITK_DATA_ROOT}/Input/grayscale_lowerleft.bmp} DATA{${ITK_DATA_ROOT}/Input/grayscale_upperleft.bmp})
itk_add_test(NAME itkBMPImageIOTest8
      COMMAND ITKIOBMPTestDriver
    itkBMPImageIOTest4 DATA{${ITK_DATA_ROOT}/Input/color_lowerleft.bmp} DATA{${ITK_DATA_ROOT}/Input/color_upperleft.bmp})
itk_add_test(NAME itkBMPImageIOTest9
      COMMAND ITKIOBMPTestDriver
    itkBMPImageIOTest5 DATA{${ITK_DATA_ROOT}/Input/grayscale_lowerleft_rle.bmp} DATA{${ITK_DATA_ROOT}/Input/grayscale_lowerleft.bmp})


itk_add_test(NAME itkBMPImageIOTestPaletteExpanded
      COMMAND ITKIOBMPTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkBMPImageIOTestPaletteExpanded.bmp
              8bb8e63723835db9d92690351a71783f
    itkBMPImageIOTestPalette DATA{Input/HeliconiusNumataPalette.bmp} ${ITK_TEST_OUTPUT_DIR}/itkBMPImageIOTestPaletteExpanded.bmp 1 0)
itk_add_test(NAME itkBMPImageIOTestPaletteNotExpanded
      COMMAND ITKIOBMPTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkBMPImageIOTestPaletteNotExpanded.bmp
              93a72ac86af5191d777707135c98999d
    itkBMPImageIOTestPalette DATA{Input/HeliconiusNumataPalette.bmp} ${ITK_TEST_OUTPUT_DIR}/itkBMPImageIOTestPaletteNotExpanded.bmp 0 1)
itk_add_test(NAME itkBMPImageIOTestPaletteNotExpandedGrey
      COMMAND ITKIOBMPTestDriver
    --compare-MD5 ${ITK_TEST_OUTPUT_DIR}/itkBMPImageIOTestPaletteNotExpandedGrey.bmp
              93a72ac86af5191d777707135c98999d
    itkBMPImageIOTestPalette DATA{Input/HeliconiusNumataPalette.bmp} ${ITK_TEST_OUTPUT_DIR}/itkBMPImageIOTestPaletteNotExpandedGrey.bmp 0 0)
