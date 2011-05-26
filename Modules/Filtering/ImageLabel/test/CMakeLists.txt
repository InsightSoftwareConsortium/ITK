itk_module_test()
set(ITK-ImageLabelTests
itkChangeLabelImageFilterTest.cxx
itkImageLabelHeaderTest.cxx
)

CreateTestDriver(ITK-ImageLabel  "${ITK-ImageLabel-Test_LIBRARIES}" "${ITK-ImageLabelTests}")

add_test(NAME itkImageLabelHeaderTest
      COMMAND ITK-ImageLabelTestDriver itkImageLabelHeaderTest)
add_test(NAME itkChangeLabelImageFilterTest
      COMMAND ITK-ImageLabelTestDriver itkChangeLabelImageFilterTest)
