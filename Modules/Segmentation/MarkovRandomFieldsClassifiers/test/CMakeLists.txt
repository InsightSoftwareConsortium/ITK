itk_module_test()
set(ITKMarkovRandomFieldsClassifiersTests
itkMRFImageFilterTest.cxx
itkMarkovRandomFieldsClassifiersHeaderTest.cxx
itkGibbsTest.cxx
)

CreateTestDriver(ITKMarkovRandomFieldsClassifiers  "${ITKMarkovRandomFieldsClassifiers-Test_LIBRARIES}" "${ITKMarkovRandomFieldsClassifiersTests}")

itk_add_test(NAME itkMarkovRandomFieldsClassifiersHeaderTest
      COMMAND ITKMarkovRandomFieldsClassifiersTestDriver itkMarkovRandomFieldsClassifiersHeaderTest)
itk_add_test(NAME itkMRFImageFilterTest
      COMMAND ITKMarkovRandomFieldsClassifiersTestDriver itkMRFImageFilterTest)
itk_add_test(NAME itkGibbsTest
      COMMAND ITKMarkovRandomFieldsClassifiersTestDriver itkGibbsTest)
