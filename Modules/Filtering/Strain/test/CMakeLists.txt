itk_module_test()

set(StrainTests itkStrainImageFilterTest.cxx)

createtestdriver(Strain "${Strain-Test_LIBRARIES}" "${StrainTests}")

itk_add_test(
  NAME itkStrainImageFilterTest
  COMMAND
    StrainTestDriver
    itkStrainImageFilterTest
    DATA{Input/LineLoadDisplacement.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterTest
)
