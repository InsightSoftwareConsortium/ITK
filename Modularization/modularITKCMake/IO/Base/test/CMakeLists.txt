create_test_sourcelist(Tests ITK-IO-Base-tests.cxx
itkArchetypeSeriesFileNamesTest.cxx
itkImageFileReaderTest1.cxx
itkImageFileWriterTest.cxx
itkIOCommonTest.cxx
itkNumericSeriesFileNamesTest.cxx
itkRegularExpressionSeriesFileNamesTest.cxx
)

add_executable(ITK-IO-Base-tests  ${Tests} )
target_link_libraries(ITK-IO-Base-tests  ${ITK-IO-Base_LIBRARIES} )

add_test(NAME itkArchetypeSeriesFileNamesTest
      COMMAND ITK-IO-Base-tests  itkArchetypeSeriesFileNamesTest
              ${ITK_DATA_ROOT}/Input/Archetype/a5b7.PNG ${ITK_DATA_ROOT}/Input/Archetype/a7b1.PNG)
add_test(NAME itkArchetypeSeriesFileNamesTest2
      COMMAND ITK-IO-Base-tests  itkArchetypeSeriesFileNamesTest
              ${ITK_DATA_ROOT}/Input/Archetype/image.001 ${ITK_DATA_ROOT}/Input/Archetype/image.010)
add_test(NAME itkImageFileReaderTest1
      COMMAND ITK-IO-Base-tests  itkImageFileReaderTest1)
add_test(NAME itkImageFileWriterTest
      COMMAND ITK-IO-Base-tests  itkImageFileWriterTest
              ${ITK_TEST_OUTPUT_DIR}/test.png)
add_test(NAME itkIOCommonTest
      COMMAND ITK-IO-Base-tests  itkIOCommonTest)
add_test(NAME itkNumericSeriesFileNamesTest
      COMMAND ITK-IO-Base-tests  itkNumericSeriesFileNamesTest)
add_test(NAME itkRegularExpressionSeriesFileNamesTest
      COMMAND ITK-IO-Base-tests  itkRegularExpressionSeriesFileNamesTest
              ${ITK_DATA_ROOT}/Input)
