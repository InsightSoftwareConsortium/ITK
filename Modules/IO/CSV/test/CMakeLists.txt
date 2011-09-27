itk_module_test()
set(ITKIOCSVTests
itkCSVArray2DFileReaderTest.cxx
itkCSVArray2DFileReaderWriterTest.cxx
itkCSVNumericObjectFileWriterTest.cxx
)

set(TEMP ${ITK_TEST_OUTPUT_DIR})

CreateTestDriver(ITKIOCSV  "${ITKIOCSV-Test_LIBRARIES}" "${ITKIOCSVTests}")

itk_add_test(NAME itkCSVArray2DFileReaderTest
      COMMAND ITKIOCSVTestDriver itkCSVArray2DFileReaderTest
              DATA{Input/csvFileReaderTestInput.csv} )
itk_add_test(NAME itkCSVNumericObjectFileWriterTest
      COMMAND ITKIOCSVTestDriver itkCSVNumericObjectFileWriterTest
              ${TEMP}/NumericObjectToCSVFileWriterOutput.csv)
itk_add_test(NAME itkCSVArray2DFileReaderWriterTest
      COMMAND ITKIOCSVTestDriver itkCSVArray2DFileReaderWriterTest
              ${TEMP}/csvFileArray2DReaderWriterTestOutput.csv)
