itk_module_test()
set(ITK-ImageAdaptorsTests
itkImageAdaptorTest.cxx
itkImageAdaptorsHeaderTest.cxx
itkRGBToVectorImageAdaptorTest.cxx
itkVectorImageTest.cxx
)

CreateTestDriver(ITK-ImageAdaptors  "${ITK-ImageAdaptors-Test_LIBRARIES}" "${ITK-ImageAdaptorsTests}")

itk_add_test(NAME itkImageAdaptorsHeaderTest
      COMMAND ITK-ImageAdaptorsTestDriver itkImageAdaptorsHeaderTest)
itk_add_test(NAME itkImageAdaptorTest
      COMMAND ITK-ImageAdaptorsTestDriver itkImageAdaptorTest)
itk_add_test(NAME itkRGBToVectorImageAdaptorTest
      COMMAND ITK-ImageAdaptorsTestDriver itkRGBToVectorImageAdaptorTest)
itk_add_test(NAME itkVectorImageTest
      COMMAND ITK-ImageAdaptorsTestDriver itkVectorImageTest
              ${ITK_TEST_OUTPUT_DIR}/VectorImage.nrrd ${ITK_TEST_OUTPUT_DIR}/VectorImage.mhd)
