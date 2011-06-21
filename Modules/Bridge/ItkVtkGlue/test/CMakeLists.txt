itk_module_test()
set(ITK-ItkVtkGlueTests
itkVtkMedianFilterTest.cxx
itkImageToVTKImageFilterTest.cxx
QuickViewTest.cxx
)

CreateTestDriver(ITK-ItkVtkGlue "${ITK-ItkVtkGlue-Test_LIBRARIES}" "${ITK-ItkVtkGlueTests}")

itk_add_test(
  NAME itkVtkMedianImageFilterTest
  COMMAND ITK-ItkVtkGlueTestDriver
    itkVtkMedianFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png 2)

itk_add_test(
  NAME itkImageToVTKImageFilterTest
  COMMAND ITK-ItkVtkGlueTestDriver
    itkImageToVTKImageFilterTest)

itk_add_test(
  NAME QuickViewTest
  COMMAND ITK-ItkVtkGlueTestDriver
           --compare DATA{Baseline/QuickViewTest.png} ${ITK_TEST_OUTPUT_DIR}/QuickViewTest0.png
    QuickViewTest ${ITK_TEST_OUTPUT_DIR})
