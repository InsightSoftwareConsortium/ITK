itk_module_test()
set(ITK-MarkovRandomFieldsClassifiersTests
itkMRFImageFilterTest.cxx
itkMarkovRandomFieldsClassifiersHeaderTest.cxx
)

CreateTestDriver(ITK-MarkovRandomFieldsClassifiers  "${ITK-MarkovRandomFieldsClassifiers-Test_LIBRARIES}" "${ITK-MarkovRandomFieldsClassifiersTests}")

add_test(NAME itkMarkovRandomFieldsClassifiersHeaderTest
      COMMAND ITK-MarkovRandomFieldsClassifiersTestDriver itkMarkovRandomFieldsClassifiersHeaderTest)
add_test(NAME itkMRFImageFilterTest
      COMMAND ITK-MarkovRandomFieldsClassifiersTestDriver itkMRFImageFilterTest)
