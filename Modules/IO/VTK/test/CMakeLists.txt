itk_module_test()
set(ITKIOVTKTests
itkVTKImageIO2Test.cxx
itkVTKImageIOTest.cxx
itkVTKImageIO2Test2.cxx
itkVTKImageIOTest2.cxx
itkVTKImageIOStreamTest.cxx
itkVTKImageIOFileReadTest.cxx
)

CreateTestDriver(ITKIOVTK  "${ITKIOVTK-Test_LIBRARIES}" "${ITKIOVTKTests}")

itk_add_test(NAME itkVTKImageIO2Test
      COMMAND ITKIOVTKTestDriver itkVTKImageIO2Test
              ${ITK_TEST_OUTPUT_DIR})
itk_add_test(NAME itkVTKImageIOTest
      COMMAND ITKIOVTKTestDriver itkVTKImageIOTest
              ${ITK_TEST_OUTPUT_DIR}/test1.vtk ${ITK_TEST_OUTPUT_DIR}/test2.vtk)
itk_add_test(NAME itkVTKImageIO2Test2
      COMMAND ITKIOVTKTestDriver itkVTKImageIO2Test2
              ${ITK_TEST_OUTPUT_DIR}/itkVTKImageIO2Test2.vtk)
itk_add_test(NAME itkVTKImageIOTest2
      COMMAND ITKIOVTKTestDriver itkVTKImageIOTest2
              DATA{${ITK_DATA_ROOT}/Input/VHFColor.mhd,VHFColor.raw} ${ITK_TEST_OUTPUT_DIR}/testVector.vtk)
itk_add_test(NAME itkVTKImageIOStreamTest
      COMMAND ITKIOVTKTestDriver itkVTKImageIOStreamTest
              ${ITK_TEST_OUTPUT_DIR}/itkVTKImageIOStreamTest.vtk)
itk_add_test(NAME itkVTKImageIOFileReadTest
      COMMAND ITKIOVTKTestDriver itkVTKImageIOFileReadTest
              DATA{Input/matrix.vtk} DATA{Input/ironProt.vtk})
