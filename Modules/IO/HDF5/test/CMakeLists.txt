itk_module_test()
set(ITK-IO-HDF5Tests
  itkHDF5ImageIOTest.cxx
  itkHDF5ImageIOStreamingReadWriteTest.cxx
)

CreateTestDriver(ITK-IO-HDF5  "${ITK-IO-HDF5-Test_LIBRARIES}" "${ITK-IO-HDF5Tests}")

add_test(NAME itkHDF5ImageIOTest
  COMMAND ITK-IO-HDF5TestDriver itkHDF5ImageIOTest ${ITK_TEST_OUTPUT_DIR} )
add_test(NAME itkHDF5ImageIOStreamingReadWriteTest
  COMMAND ITK-IO-HDF5TestDriver itkHDF5ImageIOStreamingReadWriteTest ${ITK_TEST_OUTPUT_DIR} )
