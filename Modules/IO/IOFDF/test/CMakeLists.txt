itk_module_test()
set(IOFDFTests itkFDFImageIOTest.cxx)

createtestdriver(IOFDF "${IOFDF-Test_LIBRARIES}" "${IOFDFTests}")

itk_add_test(
  NAME itkFDFImageIOTest
  COMMAND
    IOFDFTestDriver
    itkFDFImageIOTest
    ${ITK_TEST_OUTPUT_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/test.fdf
)
