itk_module_test()
set(ITK-MarkovRandomFieldsClassifiersTests
itkMRFImageFilterTest.cxx
itkMarkovRandomFieldsClassifiersHeaderTest.cxx
itkGibbsTest.cxx
)

CreateTestDriver(ITK-MarkovRandomFieldsClassifiers  "${ITK-MarkovRandomFieldsClassifiers-Test_LIBRARIES}" "${ITK-MarkovRandomFieldsClassifiersTests}")

itk_add_test(NAME itkMarkovRandomFieldsClassifiersHeaderTest
      COMMAND ITK-MarkovRandomFieldsClassifiersTestDriver itkMarkovRandomFieldsClassifiersHeaderTest)
itk_add_test(NAME itkMRFImageFilterTest
      COMMAND ITK-MarkovRandomFieldsClassifiersTestDriver itkMRFImageFilterTest)
itk_add_test(NAME itkGibbsTest
      COMMAND ITK-MarkovRandomFieldsClassifiersTestDriver itkGibbsTest)
