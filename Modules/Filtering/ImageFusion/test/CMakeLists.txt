itk_module_test()
set(ITKImageFusionTests
itkScalarToRGBPixelFunctorTest.cxx
itkLabelOverlayImageFilterTest.cxx
itkLabelToRGBImageFilterTest.cxx
itkLabelMapToRGBImageFilterTest1.cxx
itkLabelMapToRGBImageFilterTest2.cxx
itkLabelMapContourOverlayImageFilterTest1.cxx
itkLabelMapContourOverlayImageFilterTest2.cxx
itkLabelMapContourOverlayImageFilterTest3.cxx
itkLabelMapOverlayImageFilterTest1.cxx
itkLabelMapOverlayImageFilterTest2.cxx
itkLabelMapOverlayImageFilterTest3.cxx
)

CreateTestDriver(ITKImageFusion  "${ITKImageFusion-Test_LIBRARIES}" "${ITKImageFusionTests}")

itk_add_test(NAME itkScalarToRGBPixelFunctorTest
      COMMAND ITKImageFusionTestDriver itkScalarToRGBPixelFunctorTest)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_0
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/cthead1-0_0Overlay.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_0Overlay.png
    itkLabelOverlayImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} 0.0 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_0Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_1
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/cthead1-0_1Overlay.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_1Overlay.png
    itkLabelOverlayImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} 0.1 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_1Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_2
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/cthead1-0_2Overlay.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_2Overlay.png
    itkLabelOverlayImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} 0.2 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_2Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_3
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/cthead1-0_3Overlay.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_3Overlay.png
    itkLabelOverlayImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} 0.3 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_3Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_4
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/cthead1-0_4Overlay.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_4Overlay.png
    itkLabelOverlayImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} 0.4 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_4Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_5
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/cthead1-0_5Overlay.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_5Overlay.png
    itkLabelOverlayImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} 0.5 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_5Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_6
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/cthead1-0_6Overlay.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_6Overlay.png
    itkLabelOverlayImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} 0.6 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_6Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_7
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/cthead1-0_7Overlay.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_7Overlay.png
    itkLabelOverlayImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} 0.7 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_7Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_8
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/cthead1-0_8Overlay.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_8Overlay.png
    itkLabelOverlayImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} 0.8 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_8Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_9
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/cthead1-0_9Overlay.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_9Overlay.png
    itkLabelOverlayImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} 0.9 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_9Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-1_0
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/cthead1-1_0Overlay.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1-1_0Overlay.png
    itkLabelOverlayImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} 1.0 ${ITK_TEST_OUTPUT_DIR}/cthead1-1_0Overlay.png)
itk_add_test(NAME itkLabelToRGBImageFilterTest
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/cthead1Label-color.png}
              ${ITK_TEST_OUTPUT_DIR}/cthead1Label-color.png
    itkLabelToRGBImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} ${ITK_TEST_OUTPUT_DIR}/cthead1Label-color.png)
itk_add_test(NAME itkLabelMapToRGBImageFilterTest1
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/itkLabelMapToRGBImageFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapToRGBImageFilterTest1.png
    itkLabelMapToRGBImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapToRGBImageFilterTest1.png)
itk_add_test(NAME itkLabelMapToRGBImageFilterTest2
      COMMAND ITKImageFusionTestDriver
  --compare DATA{Baseline/itkLabelMapToRGBImageFilterTest1.png}
            ${ITK_TEST_OUTPUT_DIR}/itkLabelMapToRGBImageFilterTest2.png
  itkLabelMapToRGBImageFilterTest2 DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png}
  ${ITK_TEST_OUTPUT_DIR}/itkLabelMapToRGBImageFilterTest2.png)
itk_add_test(NAME itkLabelMapContourOverlayImageFilterTest0
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/itkLabelMapContourOverlayImageFilterTest0.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest0.png
    itkLabelMapContourOverlayImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} DATA{${ITK_DATA_ROOT}/Input/SpotsInverted.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest0.png 0.6 0 0 10 1)
itk_add_test(NAME itkLabelMapContourOverlayImageFilterTest1
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/itkLabelMapContourOverlayImageFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest1.png
    itkLabelMapContourOverlayImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} DATA{${ITK_DATA_ROOT}/Input/SpotsInverted.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest1.png 0.7 1 2 15 0)
itk_add_test(NAME itkLabelMapContourOverlayImageFilterTest2
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/itkLabelMapContourOverlayImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest2.png
    itkLabelMapContourOverlayImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} DATA{${ITK_DATA_ROOT}/Input/SpotsInverted.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest2.png 0.8 2 3 12 1)
itk_add_test(NAME itkLabelMapContourOverlayImageFilterTest3
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/itkLabelMapContourOverlayImageFilterTest2.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest3.png
    itkLabelMapContourOverlayImageFilterTest2 DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} DATA{${ITK_DATA_ROOT}/Input/SpotsInverted.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest3.png 0.8 2 3 12 1)
itk_add_test(NAME itkLabelMapContourOverlayImageFilterTest4
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/itkLabelMapContourOverlayImageFilterTest4.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest4.png
    itkLabelMapContourOverlayImageFilterTest3 DATA{${ITK_DATA_ROOT}/Input/SpotsLabeled.png} DATA{${ITK_DATA_ROOT}/Input/SpotsInverted.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapContourOverlayImageFilterTest4.png 0.7 1 2 15 0)

itk_add_test(NAME itkLabelMapOverlayImageFilterTest1
      COMMAND ITKImageFusionTestDriver
    --compare DATA{Baseline/itkLabelMapOverlayImageFilterTest1.png}
              ${ITK_TEST_OUTPUT_DIR}/itkLabelMapOverlayImageFilterTest1.png
    itkLabelMapOverlayImageFilterTest1 DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png} DATA{${ITK_DATA_ROOT}/Input/cthead1.png} ${ITK_TEST_OUTPUT_DIR}/itkLabelMapOverlayImageFilterTest1.png 0.3)
itk_add_test(NAME itkLabelMapOverlayImageFilterTest2
      COMMAND ITKImageFusionTestDriver
  --compare DATA{Baseline/itkLabelMapOverlayImageFilterTest1.png}
            ${ITK_TEST_OUTPUT_DIR}/itkLabelMapOverlayImageFilterTest2.png
  itkLabelMapOverlayImageFilterTest2
  DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png}
  DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
  ${ITK_TEST_OUTPUT_DIR}/itkLabelMapOverlayImageFilterTest2.png 0.3)
itk_add_test(NAME itkLabelMapOverlayImageFilterTest3
      COMMAND ITKImageFusionTestDriver
  --compare DATA{Baseline/itkLabelMapOverlayImageFilterTest3.png}
            ${ITK_TEST_OUTPUT_DIR}/itkLabelMapOverlayImageFilterTest3.png
  itkLabelMapOverlayImageFilterTest3
  DATA{${ITK_DATA_ROOT}/Input/cthead1Label.png}
  DATA{${ITK_DATA_ROOT}/Input/cthead1.png}
  ${ITK_TEST_OUTPUT_DIR}/itkLabelMapOverlayImageFilterTest3.png 0.3)
