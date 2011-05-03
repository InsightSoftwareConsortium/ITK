itk_module_test()
set(ITK-BiasCorrectionTests
itkBiasCorrectionHeaderTest.cxx
itkCompositeValleyFunctionTest.cxx
itkMRIBiasFieldCorrectionFilterTest.cxx
)

CreateTestDriver(ITK-BiasCorrection  "${ITK-BiasCorrection-Test_LIBRARIES}" "${ITK-BiasCorrectionTests}")

itk_add_test(NAME itkBiasCorrectionHeaderTest
      COMMAND ITK-BiasCorrectionTestDriver itkBiasCorrectionHeaderTest)
itk_add_test(NAME itkCompositeValleyFunctionTest
      COMMAND ITK-BiasCorrectionTestDriver itkCompositeValleyFunctionTest)
itk_add_test(NAME itkMRIBiasFieldCorrectionFilterTest
      COMMAND ITK-BiasCorrectionTestDriver itkMRIBiasFieldCorrectionFilterTest)
