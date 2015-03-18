itk_module_test()
set(ITKColormapTests
itkCustomColormapFunctionTest.cxx
itkScalarToRGBColormapImageFilterTest.cxx
)

CreateTestDriver(ITKColormap  "${ITKColormap-Test_LIBRARIES}" "${ITKColormapTests}")

itk_add_test(NAME itkCustomColormapFunctionTest
      COMMAND ITKColormapTestDriver
    itkCustomColormapFunctionTest
              DATA{Input/bone.txt} 106)

itk_add_test(NAME RGBColormapTest_red
      COMMAND ITKColormapTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_red.png
              DATA{Baseline/RGBColormapTest_red.png}
    itkScalarToRGBColormapImageFilterTest
              DATA{Input/Colormap_Grey.png} ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_red.png red)
itk_add_test(NAME RGBColormapTest_green
      COMMAND ITKColormapTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_green.png
              DATA{Baseline/RGBColormapTest_green.png}
    itkScalarToRGBColormapImageFilterTest
              DATA{Input/Colormap_Grey.png} ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_green.png green)
itk_add_test(NAME RGBColormapTest_blue
      COMMAND ITKColormapTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_blue.png
              DATA{Baseline/RGBColormapTest_blue.png}
    itkScalarToRGBColormapImageFilterTest
              DATA{Input/Colormap_Grey.png} ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_blue.png blue)
itk_add_test(NAME RGBColormapTest_hot
      COMMAND ITKColormapTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_hot.png
              DATA{Baseline/RGBColormapTest_hot.png}
    itkScalarToRGBColormapImageFilterTest
              DATA{Input/Colormap_Grey.png} ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_hot.png hot)
itk_add_test(NAME RGBColormapTest_cool
      COMMAND ITKColormapTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_cool.png
              DATA{Baseline/RGBColormapTest_cool.png}
    itkScalarToRGBColormapImageFilterTest
              DATA{Input/Colormap_Grey.png} ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_cool.png cool)
itk_add_test(NAME RGBColormapTest_spring
      COMMAND ITKColormapTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_spring.png
              DATA{Baseline/RGBColormapTest_spring.png}
    itkScalarToRGBColormapImageFilterTest
              DATA{Input/Colormap_Grey.png} ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_spring.png spring)
itk_add_test(NAME RGBColormapTest_summer
      COMMAND ITKColormapTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_summer.png
              DATA{Baseline/RGBColormapTest_summer.png}
    itkScalarToRGBColormapImageFilterTest
              DATA{Input/Colormap_Grey.png} ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_summer.png summer)
itk_add_test(NAME RGBColormapTest_autumn
      COMMAND ITKColormapTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_autumn.png
              DATA{Baseline/RGBColormapTest_autumn.png}
    itkScalarToRGBColormapImageFilterTest
              DATA{Input/Colormap_Grey.png} ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_autumn.png autumn)
itk_add_test(NAME RGBColormapTest_winter
      COMMAND ITKColormapTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_winter.png
              DATA{Baseline/RGBColormapTest_winter.png}
    itkScalarToRGBColormapImageFilterTest
              DATA{Input/Colormap_Grey.png} ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_winter.png winter)
itk_add_test(NAME RGBColormapTest_copper
      COMMAND ITKColormapTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_copper.png
              DATA{Baseline/RGBColormapTest_copper.png}
    itkScalarToRGBColormapImageFilterTest
              DATA{Input/Colormap_Grey.png} ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_copper.png copper)
itk_add_test(NAME RGBColormapTest_hsv
      COMMAND ITKColormapTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_hsv.png
              DATA{Baseline/RGBColormapTest_hsv.png}
    itkScalarToRGBColormapImageFilterTest
              DATA{Input/Colormap_Grey.png} ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_hsv.png hsv)
itk_add_test(NAME RGBColormapTest_jet
      COMMAND ITKColormapTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_jet.png
              DATA{Baseline/RGBColormapTest_jet.png}
    itkScalarToRGBColormapImageFilterTest
              DATA{Input/Colormap_Grey.png} ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_jet.png jet)
itk_add_test(NAME RGBColormapTest_overunder
      COMMAND ITKColormapTestDriver
    --compare ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_overunder.png
              DATA{Baseline/RGBColormapTest_overunder.png}
    itkScalarToRGBColormapImageFilterTest
              DATA{Input/Colormap_Grey.png} ${ITK_TEST_OUTPUT_DIR}/RGBColormapTest_overunder.png overunder)
