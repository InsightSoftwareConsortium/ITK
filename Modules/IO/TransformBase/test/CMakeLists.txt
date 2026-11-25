itk_module_test()
set(
  ITKIOTransformBaseTests
  itkTransformFileReaderTest.cxx
  itkTransformFileWriterTest.cxx
)

createtestdriver(ITKIOTransformBase "${ITKIOTransformBase-Test_LIBRARIES}" "${ITKIOTransformBaseTests}")

itk_add_test(
  NAME itkTransformFileReaderTest
  COMMAND
    ITKIOTransformBaseTestDriver
    itkTransformFileReaderTest
)
itk_add_test(
  NAME itkTransformFileWriterTest
  COMMAND
    ITKIOTransformBaseTestDriver
    itkTransformFileWriterTest
)

set(ITKIOTransformBaseGTests itkTransformIOBaseGTest.cxx)

creategoogletestdriver(ITKIOTransformBase "${ITKIOTransformBase-Test_LIBRARIES}" "${ITKIOTransformBaseGTests}")
