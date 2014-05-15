itk_module_test()
set(ITKIOFDFTests itkFDFImageIOTest.cxx)

createtestdriver(ITKIOFDF "${ITKIOFDF-Test_LIBRARIES}" "${ITKIOFDFTests}")

itk_add_test(
  NAME itkFDFImageIOTest
  COMMAND
    ITKIOFDFTestDriver
    itkFDFImageIOTest
    ${ITK_TEST_OUTPUT_DIR}
    ${CMAKE_CURRENT_LIST_DIR}/test.fdf
)
