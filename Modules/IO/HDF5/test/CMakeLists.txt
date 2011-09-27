itk_module_test()
set(ITKIOHDF5Tests
  itkHDF5ImageIOTest.cxx
  itkHDF5ImageIOStreamingReadWriteTest.cxx
)

CreateTestDriver(ITKIOHDF5  "${ITKIOHDF5-Test_LIBRARIES}" "${ITKIOHDF5Tests}")

add_test(NAME itkHDF5ImageIOTest
  COMMAND ITKIOHDF5TestDriver itkHDF5ImageIOTest ${ITK_TEST_OUTPUT_DIR} )
add_test(NAME itkHDF5ImageIOStreamingReadWriteTest
  COMMAND ITKIOHDF5TestDriver itkHDF5ImageIOStreamingReadWriteTest ${ITK_TEST_OUTPUT_DIR} )
