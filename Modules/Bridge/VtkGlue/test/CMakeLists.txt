itk_module_test()
set(ITKVtkGlueTests
itkVtkConnectedComponentImageFilterTest
itkVtkMedianFilterTest.cxx
itkImageToVTKImageFilterTest.cxx
itkImageToVTKImageFilterRGBTest.cxx
itkVTKImageToImageFilterTest.cxx
QuickViewTest.cxx
)

CreateTestDriver(ITKVtkGlue "${ITKVtkGlue-Test_LIBRARIES}" "${ITKVtkGlueTests}")

itk_add_test(
  NAME itkVtkMedianImageFilterTest
  COMMAND ITKVtkGlueTestDriver
    itkVtkMedianFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} 2)
set_property(TEST itkVtkMedianImageFilterTest APPEND PROPERTY LABELS REQUIRES_DISPLAY)

itk_add_test(
  NAME itkImageToVTKImageFilterTest
  COMMAND ITKVtkGlueTestDriver
    itkImageToVTKImageFilterTest)

itk_add_test(
  NAME itkImageToVTKImageFilterRGBTest
  COMMAND ITKVtkGlueTestDriver
    itkImageToVTKImageFilterRGBTest
      DATA{${ITK_DATA_ROOT}/Input/VisibleWomanEyeSlice.png})

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
set_property(TEST QuickViewTest APPEND PROPERTY LABELS REQUIRES_DISPLAY)

itk_add_test(
  NAME itkVtkConnectedComponentImageFilterTest
  COMMAND ITKVtkGlueTestDriver
    itkVtkConnectedComponentImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png})
set_property(TEST itkVtkConnectedComponentImageFilterTest APPEND PROPERTY LABELS REQUIRES_DISPLAY)
