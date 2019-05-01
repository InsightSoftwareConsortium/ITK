itk_module_test()
set(ITKVtkGlueTests
  itkImageToVTKImageFilterTest.cxx
  itkImageToVTKImageFilterRGBTest.cxx
  itkVTKImageToImageFilterTest.cxx
)
if(NOT VTK_RENDERING_BACKEND STREQUAL "None")
  list(APPEND ITKVtkGlueTests
    itkVtkMedianFilterTest.cxx
    itkVtkConnectedComponentImageFilterTest.cxx
    QuickViewTest.cxx
    # ### runViewImage ###
    # No test generated. Use it for view images (2D or 3D) with:
    # ITKVtkGlueTestDriver runViewImage image_file
    # or with a string to change title window, and change size of window
    # ITKVtkGlueTestDriver runViewImage image_file "MyImage"
    runViewImage.cxx
    )
endif()

CreateTestDriver(ITKVtkGlue "${ITKVtkGlue-Test_LIBRARIES}" "${ITKVtkGlueTests}")
if(VTK_VERSION VERSION_GREATER_EQUAL "8.90.0")
  vtk_module_autoinit(
    TARGETS ITKVtkGlueTestDriver
  MODULES ${VTK_LIBRARIES})
endif()

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

if(NOT VTK_RENDERING_BACKEND STREQUAL "None")
  itk_add_test(
    NAME itkVtkMedianImageFilterTest
    COMMAND ITKVtkGlueTestDriver
      itkVtkMedianFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png} 2)
  set_property(TEST itkVtkMedianImageFilterTest APPEND PROPERTY LABELS REQUIRES_DISPLAY)

  itk_add_test(
    NAME QuickViewTest
    COMMAND ITKVtkGlueTestDriver
             --compare DATA{Baseline/QuickViewTest.png,:}
                       ${ITK_TEST_OUTPUT_DIR}/QuickViewTest0.png
             --compareNumberOfPixelsTolerance 1500
      QuickViewTest DATA{${ITK_DATA_ROOT}/Input/peppers.png} ${ITK_TEST_OUTPUT_DIR})
  set_property(TEST QuickViewTest APPEND PROPERTY LABELS REQUIRES_DISPLAY)

  itk_add_test(
    NAME itkVtkConnectedComponentImageFilterTest
    COMMAND ITKVtkGlueTestDriver
      itkVtkConnectedComponentImageFilterTest DATA{${ITK_DATA_ROOT}/Input/cthead1.png})
  set_property(TEST itkVtkConnectedComponentImageFilterTest APPEND PROPERTY LABELS REQUIRES_DISPLAY)
endif()
