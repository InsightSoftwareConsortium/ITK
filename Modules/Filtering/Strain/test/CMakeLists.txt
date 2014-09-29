itk_module_test()

set(
  StrainTests
  itkStrainImageFilterTest.cxx
  itkStrainImageFilterLagrangianTest.cxx
  itkStrainImageFilterEulerianTest.cxx
)

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

itk_add_test(
  NAME itkStrainImageFilterLagrangianTest
  COMMAND
    StrainTestDriver
    --compare
    DATA{Baseline/LineLoadStrain.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterLagrangianTestOutput.vtk
    itkStrainImageFilterLagrangianTest
    DATA{Input/LineLoadDisplacement.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterLagrangianTest
)

itk_add_test(
  NAME itkStrainImageFilterEulerianTest
  COMMAND
    StrainTestDriver
    --compare
    DATA{Baseline/LineLoadStrain.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterEulerianTestOutput.vtk
    itkStrainImageFilterEulerianTest
    DATA{Input/LineLoadDisplacement.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterEulerianTest
)
