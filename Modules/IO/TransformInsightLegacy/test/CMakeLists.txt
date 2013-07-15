itk_module_test()
set(ITKIOTransformInsightLegacyTests
itkIOTransformTxtTest.cxx
)

CreateTestDriver(ITKIOTransformInsightLegacy "${ITKIOTransformInsightLegacy-Test_LIBRARIES}" "${ITKIOTransformInsightLegacyTests}")

itk_add_test(NAME itkIOTransformTxtTest
      COMMAND ITKIOTransformInsightLegacyTestDriver itkIOTransformTxtTest
        ${ITK_TEST_OUTPUT_DIR} )
