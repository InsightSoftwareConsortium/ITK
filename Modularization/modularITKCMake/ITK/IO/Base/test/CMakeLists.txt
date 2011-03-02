itk_module_test()
set(ITK-IO-BaseTests
itkImageFileReaderTest1.cxx
itkImageFileWriterTest.cxx
itkIOBaseHeaderTest.cxx
itkIOCommonTest.cxx
itkNumericSeriesFileNamesTest.cxx
itkRegularExpressionSeriesFileNamesTest.cxx
itkArchetypeSeriesFileNamesTest.cxx
)

CreateTestDriver(ITK-IO-Base  "${ITK-IO-Base-Test_LIBRARIES}" "${ITK-IO-BaseTests}")
add_test(NAME itkArchetypeSeriesFileNamesTest
      COMMAND ITK-IO-BaseTestDriver
    itkArchetypeSeriesFileNamesTest
    ${ITK_DATA_ROOT}/Input/Archetype/a5b7.PNG
    ${ITK_DATA_ROOT}/Input/Archetype/a7b1.PNG)
add_test(NAME itkArchetypeSeriesFileNamesTest2
      COMMAND ITK-IO-BaseTestDriver
    itkArchetypeSeriesFileNamesTest
    ${ITK_DATA_ROOT}/Input/Archetype/image.001
    ${ITK_DATA_ROOT}/Input/Archetype/image.010)
add_test(NAME itkIOBaseHeaderTest
      COMMAND ITK-IO-BaseTestDriver itkIOBaseHeaderTest)
add_test(NAME itkImageFileReaderTest1
      COMMAND ITK-IO-BaseTestDriver itkImageFileReaderTest1)
add_test(NAME itkImageFileWriterTest
      COMMAND ITK-IO-BaseTestDriver itkImageFileWriterTest
              ${ITK_TEST_OUTPUT_DIR}/test.png)
add_test(NAME itkIOCommonTest
      COMMAND ITK-IO-BaseTestDriver itkIOCommonTest)
add_test(NAME itkNumericSeriesFileNamesTest
      COMMAND ITK-IO-BaseTestDriver itkNumericSeriesFileNamesTest)
add_test(NAME itkRegularExpressionSeriesFileNamesTest
      COMMAND ITK-IO-BaseTestDriver itkRegularExpressionSeriesFileNamesTest
              ${ITK_DATA_ROOT}/Input)
