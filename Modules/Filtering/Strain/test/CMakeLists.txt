itk_module_test()

set(
  StrainTests
  itkStrainImageFilterTest.cxx
  itkStrainImageFilterDoGTest.cxx
  itkStrainImageFilterRecursiveGaussianTest.cxx
  itkTransformToStrainFilterTest.cxx
)

createtestdriver(Strain "${Strain-Test_LIBRARIES}" "${StrainTests}")

itk_add_test(
  NAME itkStrainImageFilterInfinitesimalTest
  COMMAND
    StrainTestDriver
    --compare
    DATA{Baseline/LineLoadStrain.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterTestOutput.vtk
    itkStrainImageFilterTest
    DATA{Input/LineLoadDisplacement.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterTest
    "INFINITESIMAL"
)

itk_add_test(
  NAME itkStrainImageFilterLagrangianTest
  COMMAND
    StrainTestDriver
    --compare
    DATA{Baseline/LineLoadStrain.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterLagrangianTestOutput.vtk
    itkStrainImageFilterTest
    DATA{Input/LineLoadDisplacement.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterLagrangianTest
    "GREENLAGRANGIAN"
)

itk_add_test(
  NAME itkStrainImageFilterEulerianTest
  COMMAND
    StrainTestDriver
    --compare
    DATA{Baseline/LineLoadStrain.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterEulerianTestOutput.vtk
    itkStrainImageFilterTest
    DATA{Input/LineLoadDisplacement.mha}
    ${ITK_TEST_OUTPUT_DIR}/itkStrainImageFilterEulerianTest
    "EULERIANALMANSI"
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
# ComputeJacobianWithRespectToPosition
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

itk_add_test(
  NAME itkTransformToStrainFilterEulerianAlmansiTest
  COMMAND
    StrainTestDriver
    --compare
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestEulerianAlmansiAffine.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestEulerianAlmansiAffineToDisplacementFieldStrain.mha
    itkTransformToStrainFilterTest
    "EULERIANALMANSI"
    "Affine"
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestEulerianAlmansiAffine.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestEulerianAlmansiAffineToDisplacementField.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestEulerianAlmansiAffineToDisplacementFieldStrain.mha
)

itk_add_test(
  NAME itkTransformToStrainFilterInfinitesimalSimilarityTest
  COMMAND
    StrainTestDriver
    --compare
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestInfinitesimalSimilarity.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestInfinitesimalSimilarityToDisplacementFieldStrain.mha
    itkTransformToStrainFilterTest
    "INFINITESIMAL"
    "Similarity"
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestInfinitesimalSimilarity.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestInfinitesimalSimilarityToDisplacementField.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestInfinitesimalSimilarityToDisplacementFieldStrain.mha
)

itk_add_test(
  NAME itkTransformToStrainFilterSimilarityGreenLagrangianSimilarityTest
  COMMAND
    StrainTestDriver
    --compare
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestGreenLagrangianSimilarity.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestGreenLagrangianSimilarityToDisplacementFieldStrain.mha
    itkTransformToStrainFilterTest
    "GREENLAGRANGIAN"
    "Similarity"
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestGreenLagrangianSimilarity.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestGreenLagrangianSimilarityToDisplacementField.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestGreenLagrangianSimilarityToDisplacementFieldStrain.mha
)

itk_add_test(
  NAME itkTransformToStrainFilterSimilarityEulerianAlmansiSimilarityTest
  COMMAND
    StrainTestDriver
    --compare
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestEulerianAlmansiSimilarity.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestEulerianAlmansiSimilarityToDisplacementFieldStrain.mha
    itkTransformToStrainFilterTest
    "EULERIANALMANSI"
    "Similarity"
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestEulerianAlmansiSimilarity.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestEulerianAlmansiSimilarityToDisplacementField.mha
    ${ITK_TEST_OUTPUT_DIR}/itkTransformToStrainFilterTestEulerianAlmansiSimilarityToDisplacementFieldStrain.mha
)
