itk_module_test()
set(ITKItkVtkGlueTests
itkVtkMedianFilterTest.cxx
itkImageToVTKImageFilterTest.cxx
QuickViewTest.cxx
)

CreateTestDriver(ITKItkVtkGlue "${ITKItkVtkGlue-Test_LIBRARIES}" "${ITKItkVtkGlueTests}")

itk_add_test(
  NAME itkVtkMedianImageFilterTest
  COMMAND ITKItkVtkGlueTestDriver
    itkVtkMedianFilterTest ${ITK_DATA_ROOT}/Input/cthead1.png 2)

itk_add_test(
  NAME itkImageToVTKImageFilterTest
  COMMAND ITKItkVtkGlueTestDriver
    itkImageToVTKImageFilterTest)

itk_add_test(
  NAME QuickViewTest
  COMMAND ITKItkVtkGlueTestDriver
           --compare DATA{Baseline/QuickViewTest.png} ${ITK_TEST_OUTPUT_DIR}/QuickViewTest0.png
    QuickViewTest ${ITK_TEST_OUTPUT_DIR})
