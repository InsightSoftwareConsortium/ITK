itk_module_test()
set(ITKImageLabelTests
itkChangeLabelImageFilterTest.cxx
itkLabelContourImageFilterTest.cxx
itkBinaryContourImageFilterTest.cxx
itkLabelOverlayImageFilterTest.cxx
itkLabelToRGBImageFilterTest.cxx
itkImageLabelHeaderTest.cxx
)

CreateTestDriver(ITKImageLabel  "${ITKImageLabel-Test_LIBRARIES}" "${ITKImageLabelTests}")

itk_add_test(NAME itkImageLabelHeaderTest
      COMMAND ITKImageLabelTestDriver itkImageLabelHeaderTest)
itk_add_test(NAME itkChangeLabelImageFilterTest
      COMMAND ITKImageLabelTestDriver itkChangeLabelImageFilterTest)
itk_add_test(NAME itkLabelContourImageFilterTest0
      COMMAND ITKImageLabelTestDriver
      --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelContourImageFilterTest0.png
      ${ITK_TEST_OUTPUT_DIR}/itkLabelContourImageFilterTest0.png
      itkLabelContourImageFilterTest
      ${ITK_DATA_ROOT}/Input/2th_cthead1.png
      ${ITK_TEST_OUTPUT_DIR}/itkLabelContourImageFilterTest0.png
      0 0)
itk_add_test(NAME itkLabelContourImageFilterTest1
      COMMAND ITKImageLabelTestDriver
      --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelContourImageFilterTest1.png
      ${ITK_TEST_OUTPUT_DIR}/itkLabelContourImageFilterTest1.png
      itkLabelContourImageFilterTest
      ${ITK_DATA_ROOT}/Input/2th_cthead1.png
      ${ITK_TEST_OUTPUT_DIR}/itkLabelContourImageFilterTest1.png
      1 0)
itk_add_test(NAME itkBinaryContourImageFilterTest0
      COMMAND ITKImageLabelTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBinaryContourImageFilterTest0.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryContourImageFilterTest0.png
    itkBinaryContourImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryContourImageFilterTest0.png 0 200 0)
itk_add_test(NAME itkBinaryContourImageFilterTest1
      COMMAND ITKImageLabelTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/itkBinaryContourImageFilterTest1.png
              ${ITK_TEST_OUTPUT_DIR}/itkBinaryContourImageFilterTest1.png
    itkBinaryContourImageFilterTest ${ITK_DATA_ROOT}/Input/2th_cthead1.png ${ITK_TEST_OUTPUT_DIR}/itkBinaryContourImageFilterTest1.png 1 200 0)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_0
      COMMAND ITKImageLabelTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_0Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_0Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.0 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_0Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_1
      COMMAND ITKImageLabelTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_1Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_1Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.1 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_1Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_2
      COMMAND ITKImageLabelTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_2Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_2Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.2 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_2Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_3
      COMMAND ITKImageLabelTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_3Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_3Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.3 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_3Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_4
      COMMAND ITKImageLabelTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_4Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_4Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.4 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_4Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_5
      COMMAND ITKImageLabelTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_5Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_5Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.5 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_5Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_6
      COMMAND ITKImageLabelTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_6Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_6Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.6 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_6Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_7
      COMMAND ITKImageLabelTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_7Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_7Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.7 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_7Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_8
      COMMAND ITKImageLabelTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_8Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_8Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.8 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_8Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-0_9
      COMMAND ITKImageLabelTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-0_9Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-0_9Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 0.9 ${ITK_TEST_OUTPUT_DIR}/cthead1-0_9Overlay.png)
itk_add_test(NAME itkLabelOverlayImageFilterTest-Opacity-1_0
      COMMAND ITKImageLabelTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1-1_0Overlay.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1-1_0Overlay.png
    itkLabelOverlayImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png ${ITK_DATA_ROOT}/Input/cthead1Label.png 1.0 ${ITK_TEST_OUTPUT_DIR}/cthead1-1_0Overlay.png)
itk_add_test(NAME itkLabelToRGBImageFilterTest
      COMMAND ITKImageLabelTestDriver
    --compare ${ITK_DATA_ROOT}/Baseline/Review/cthead1Label-color.png
              ${ITK_TEST_OUTPUT_DIR}/cthead1Label-color.png
    itkLabelToRGBImageFilterTest ${ITK_DATA_ROOT}/Input/cthead1Label.png ${ITK_TEST_OUTPUT_DIR}/cthead1Label-color.png)
