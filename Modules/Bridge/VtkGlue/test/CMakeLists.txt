itk_module_test()
set(ITKVtkGlueTests
itkVtkMedianFilterTest.cxx
itkImageToVTKImageFilterTest.cxx
itkVTKImageToImageFilterTest.cxx
QuickViewTest.cxx
)

CreateTestDriver(ITKVtkGlue "${ITKVtkGlue-Test_LIBRARIES}" "${ITKVtkGlueTests}")

itk_add_test(
  NAME itkVtkMedianImageFilterTest
  COMMAND ITKVtkGlueTestDriver
    itkVtkMedianFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} 2)

itk_add_test(
  NAME itkImageToVTKImageFilterTest
  COMMAND ITKVtkGlueTestDriver
    itkImageToVTKImageFilterTest)

itk_add_test(
  NAME itkVTKImageToImageFilterTest
  COMMAND ITKVtkGlueTestDriver
    itkVTKImageToImageFilterTest)

itk_add_test(
  NAME QuickViewTest
  COMMAND ITKVtkGlueTestDriver
           --compare DATA{Baseline/QuickViewTest.png,:}
                     ${ITK_TEST_OUTPUT_DIR}/QuickViewTest0.png
    QuickViewTest DATA{${ITK_DATA_ROOT}/Input/peppers.png} ${ITK_TEST_OUTPUT_DIR})
