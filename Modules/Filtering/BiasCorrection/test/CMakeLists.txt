itk_module_test()
set(ITKBiasCorrectionTests
itkCompositeValleyFunctionTest.cxx
itkMRIBiasFieldCorrectionFilterTest.cxx
)

CreateTestDriver(ITKBiasCorrection  "${ITKBiasCorrection-Test_LIBRARIES}" "${ITKBiasCorrectionTests}")

itk_add_test(NAME itkCompositeValleyFunctionTest
      COMMAND ITKBiasCorrectionTestDriver itkCompositeValleyFunctionTest)
itk_add_test(NAME itkMRIBiasFieldCorrectionFilterTest
      COMMAND ITKBiasCorrectionTestDriver itkMRIBiasFieldCorrectionFilterTest)
