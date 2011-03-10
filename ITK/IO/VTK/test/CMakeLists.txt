itk_module_test()
set(ITK-IO-VTKTests
itkVTKImageIO2Test.cxx
itkVTKImageIOTest.cxx
itkIOVTKHeaderTest.cxx
)

CreateTestDriver(ITK-IO-VTK  "${ITK-IO-VTK-Test_LIBRARIES}" "${ITK-IO-VTKTests}")

add_test(NAME itkIOVTKHeaderTest
      COMMAND ITK-IO-VTKTestDriver itkIOVTKHeaderTest)
add_test(NAME itkVTKImageIO2Test
      COMMAND ITK-IO-VTKTestDriver itkVTKImageIO2Test
              ${ITK_TEST_OUTPUT_DIR})
add_test(NAME itkVTKImageIOTest
      COMMAND ITK-IO-VTKTestDriver itkVTKImageIOTest
              ${ITK_TEST_OUTPUT_DIR}/test1.vtk ${ITK_TEST_OUTPUT_DIR}/test2.vtk)
