itk_module_test()
set(ITK-ImageLabelTests
itkChangeLabelImageFilterTest.cxx
itkLabelContourImageFilterTest.cxx
itkImageLabelHeaderTest.cxx
)

CreateTestDriver(ITK-ImageLabel  "${ITK-ImageLabel-Test_LIBRARIES}" "${ITK-ImageLabelTests}")

itk_add_test(NAME itkImageLabelHeaderTest
      COMMAND ITK-ImageLabelTestDriver itkImageLabelHeaderTest)
itk_add_test(NAME itkChangeLabelImageFilterTest
      COMMAND ITK-ImageLabelTestDriver itkChangeLabelImageFilterTest)
itk_add_test(NAME itkLabelContourImageFilterTest0
      COMMAND ITK-ImageLabelTestDriver
      --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelContourImageFilterTest0.png
      ${ITK_TEST_OUTPUT_DIR}/itkLabelContourImageFilterTest0.png
      itkLabelContourImageFilterTest
      ${ITK_DATA_ROOT}/Input/2th_cthead1.png
      ${ITK_TEST_OUTPUT_DIR}/itkLabelContourImageFilterTest0.png
      0 0)
itk_add_test(NAME itkLabelContourImageFilterTest1
      COMMAND ITK-ImageLabelTestDriver
      --compare ${ITK_DATA_ROOT}/Baseline/Review/itkLabelContourImageFilterTest1.png
      ${ITK_TEST_OUTPUT_DIR}/itkLabelContourImageFilterTest1.png
      itkLabelContourImageFilterTest
      ${ITK_DATA_ROOT}/Input/2th_cthead1.png
      ${ITK_TEST_OUTPUT_DIR}/itkLabelContourImageFilterTest1.png
      1 0)
