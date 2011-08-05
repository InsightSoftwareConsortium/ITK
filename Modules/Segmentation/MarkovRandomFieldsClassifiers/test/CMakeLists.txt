itk_module_test()
set(ITKMarkovRandomFieldsClassifiersTests
itkMRFImageFilterTest.cxx
itkGibbsTest.cxx
)

CreateTestDriver(ITKMarkovRandomFieldsClassifiers  "${ITKMarkovRandomFieldsClassifiers-Test_LIBRARIES}" "${ITKMarkovRandomFieldsClassifiersTests}")

itk_add_test(NAME itkMRFImageFilterTest
      COMMAND ITKMarkovRandomFieldsClassifiersTestDriver itkMRFImageFilterTest)
itk_add_test(NAME itkGibbsTest
      COMMAND ITKMarkovRandomFieldsClassifiersTestDriver itkGibbsTest)
