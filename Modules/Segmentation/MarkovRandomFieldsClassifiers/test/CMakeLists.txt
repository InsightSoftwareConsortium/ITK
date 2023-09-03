itk_module_test()
set(ITKMarkovRandomFieldsClassifiersTests
itkMRFImageFilterTest.cxx
itkRGBGibbsPriorFilterTest.cxx
)

CreateTestDriver(ITKMarkovRandomFieldsClassifiers  "${ITKMarkovRandomFieldsClassifiers-Test_LIBRARIES}" "${ITKMarkovRandomFieldsClassifiersTests}")

itk_add_test(NAME itkMRFImageFilterTest
      COMMAND ITKMarkovRandomFieldsClassifiersTestDriver itkMRFImageFilterTest)
itk_add_test(NAME itkRGBGibbsPriorFilterTest
      COMMAND ITKMarkovRandomFieldsClassifiersTestDriver itkRGBGibbsPriorFilterTest)
