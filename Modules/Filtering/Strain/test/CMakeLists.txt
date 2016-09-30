itk_module_test()

set(
  StrainTests
  itkStrainImageFilterTest.cxx
  itkStrainImageFilterLagrangianTest.cxx
  itkStrainImageFilterEulerianTest.cxx
  itkStrainImageFilterDoGTest.cxx
  itkStrainImageFilterRecursiveGaussianTest.cxx
  itkTransformToStrainFilterTest.cxx
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

itk_add_test(
  NAME itkStrainImageFilterDoGTest
  COMMAND
    StrainTestDriver
    --compare
    DATA{Baseline/LineLoadStrain.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterDoGTestOutput.vtk
    itkStrainImageFilterDoGTest
    DATA{Input/LineLoadDisplacement.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterDoGTest
)

itk_add_test(
  NAME itkStrainImageFilterRecursiveGaussianTest
  COMMAND
    StrainTestDriver
    --compare
    DATA{Baseline/LineLoadStrain.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterRecursiveGaussianTestOutput.vtk
    itkStrainImageFilterRecursiveGaussianTest
    DATA{Input/LineLoadDisplacement.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterRecursiveGaussianTest
)

# BSplineTransform has not yet implemented
# ComputeJacobianWithRespectToPosition :-(
#itk_add_test(NAME itkTransformToStrainFilterTest
#COMMAND StrainTestDriver
#itkTransformToStrainFilterTest
#"BSpline"
#${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTest.mha
#DATA{${ITK_DATA_ROOT}/Input/parametersBSpline.txt})

itk_add_test(
  NAME itkTransformToStrainFilterInfinitesimalTest
  COMMAND
    StrainTestDriver
    --compare
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestInfinitesimalAffine.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestInfinitesimalAffineToDisplacementFieldStrain.mha
    itkTransformToStrainFilterTest
    "INFINITESIMAL"
    "Affine"
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestInfinitesimalAffine.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestInfinitesimalAffineToDisplacementField.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestInfinitesimalAffineToDisplacementFieldStrain.mha
)

itk_add_test(
  NAME itkTransformToStrainFilterGreenLagrangianTest
  COMMAND
    StrainTestDriver
    --compare
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestGreenLagrangianAffine.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestGreenLagrangianAffineToDisplacementFieldStrain.mha
    itkTransformToStrainFilterTest
    "GREENLAGRANGIAN"
    "Affine"
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestGreenLagrangianAffine.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestGreenLagrangianAffineToDisplacementField.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestGreenLagrangianAffineToDisplacementFieldStrain.mha
)
