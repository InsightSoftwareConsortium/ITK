itk_module_test()

set(StrainTests itkStrainImageFilterTest.cxx)

createtestdriver(Strain "${Strain-Test_LIBRARIES}" "${StrainTests}")

itk_add_test(
  NAME itkStrainImageFilterTest
  COMMAND
    StrainTestDriver
    --compare
    DATA{Baseline/LineLoadStrain.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterTestOutput.vtk
    itkStrainImageFilterTest
    DATA{Input/LineLoadDisplacement.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterTest
)
