itk_module_test()
set(
  ITKIOTransformInsightLegacyTests
  itkIOEuler3DTransformTxtTest.cxx
  itkIOTransformTxtTest.cxx
)

createtestdriver(ITKIOTransformInsightLegacy "${ITKIOTransformInsightLegacy-Test_LIBRARIES}"
                 "${ITKIOTransformInsightLegacyTests}"
)

itk_add_test(
  NAME itkIOTransformTxtTest
  COMMAND
    ITKIOTransformInsightLegacyTestDriver
    itkIOTransformTxtTest
    ${ITK_TEST_OUTPUT_DIR}
)

itk_add_test(
  NAME itkIOTransformEuler3DTxtTest
  COMMAND
    ITKIOTransformInsightLegacyTestDriver
    itkIOEuler3DTransformTxtTest
    DATA{Input/euler3DOldFormat.tfm}
    ${ITK_TEST_OUTPUT_DIR}/euler3DNewFormat.tfm
)
