itk_module_test()
set(
  ITKIOHDF5Tests
  itkHDF5ImageIOStreamingReadWriteTest.cxx
  itkHDF5ImageIOTest.cxx
)

createtestdriver(ITKIOHDF5 "${ITKIOHDF5-Test_LIBRARIES}" "${ITKIOHDF5Tests}")

set(ITKIOHDF5GTests itkHDF5ImageIOGTest.cxx)
creategoogletestdriver(ITKIOHDF5 "${ITKIOHDF5-Test_LIBRARIES}" "${ITKIOHDF5GTests}")
target_compile_definitions(
  ITKIOHDF5GTestDriver
  PRIVATE
    "ITK_TEST_OUTPUT_DIR=${ITK_TEST_OUTPUT_DIR}"
)

itk_add_test(
  NAME itkHDF5ImageIOTest
  COMMAND
    ITKIOHDF5TestDriver
    itkHDF5ImageIOTest
    ${ITK_TEST_OUTPUT_DIR}
)
itk_add_test(
  NAME itkHDF5ImageIOStreamingReadWriteTest
  COMMAND
    ITKIOHDF5TestDriver
    itkHDF5ImageIOStreamingReadWriteTest
    ${ITK_TEST_OUTPUT_DIR}
)
