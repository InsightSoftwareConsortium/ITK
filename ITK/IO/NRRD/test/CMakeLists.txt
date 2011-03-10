itk_module_test()
set(ITK-IO-NRRDTests
itkIONRRDHeaderTest.cxx
itkNrrdImageIOTest.cxx
)

CreateTestDriver(ITK-IO-NRRD  "${ITK-IO-NRRD-Test_LIBRARIES}" "${ITK-IO-NRRDTests}")

add_test(NAME itkIONRRDHeaderTest
      COMMAND ITK-IO-NRRDTestDriver itkIONRRDHeaderTest)
add_test(NAME itkNrrdImageIOTest1
      COMMAND ITK-IO-NRRDTestDriver itkNrrdImageIOTest
              ${ITK_TEST_OUTPUT_DIR}/testNrrd.nrrd)
add_test(NAME itkNrrdImageIOTest2
      COMMAND ITK-IO-NRRDTestDriver itkNrrdImageIOTest
              ${ITK_TEST_OUTPUT_DIR}/testNrrd.nhdr)
