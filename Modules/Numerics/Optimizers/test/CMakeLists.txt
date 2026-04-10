itk_module_test()
set(
  ITKOptimizersTests
  itkAmoebaOptimizerTest.cxx
  itkConjugateGradientOptimizerTest.cxx
  itkCumulativeGaussianOptimizerTest.cxx
  itkExhaustiveOptimizerTest.cxx
  itkFRPROptimizerTest.cxx
  itkGradientDescentOptimizerTest.cxx
  itkInitializationBiasedParticleSwarmOptimizerTest.cxx
  itkLBFGSBOptimizerTest.cxx
  itkLBFGSOptimizerTest.cxx
  itkLevenbergMarquardtOptimizerTest.cxx
  itkOnePlusOneEvolutionaryOptimizerTest.cxx
  itkOptimizersHierarchyTest.cxx
  itkParticleSwarmOptimizerTest.cxx
  itkPowellOptimizerTest.cxx
  itkRegularStepGradientDescentOptimizerTest.cxx
  itkSPSAOptimizerTest.cxx
  itkVersorRigid3DTransformOptimizerTest.cxx
  itkVersorTransformOptimizerTest.cxx
)

createtestdriver(ITKOptimizers "${ITKOptimizers-Test_LIBRARIES}" "${ITKOptimizersTests}")

itk_add_test(
  NAME itkOptimizersHierarchyTest
  COMMAND
    ITKOptimizersTestDriver
    itkOptimizersHierarchyTest
)
itk_add_test(
  NAME itkFRPROptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkFRPROptimizerTest
)
itk_add_test(
  NAME itkPowellOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkPowellOptimizerTest
    0
    10
    0.01
    0.1
    100
    100
    0
    0.0
)
itk_add_test(
  NAME itkGradientDescentOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkGradientDescentOptimizerTest
)
itk_add_test(
  NAME itkVersorTransformOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkVersorTransformOptimizerTest
)
itk_add_test(
  NAME itkSPSAOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkSPSAOptimizerTest
)
itk_add_test(
  NAME itkExhaustiveOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkExhaustiveOptimizerTest
)
itk_add_test(
  NAME itkLBFGSBOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkLBFGSBOptimizerTest
)
itk_add_test(
  NAME itkConjugateGradientOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkConjugateGradientOptimizerTest
)
itk_add_test(
  NAME itkLBFGSOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkLBFGSOptimizerTest
)
itk_add_test(
  NAME itkCumulativeGaussianOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkCumulativeGaussianOptimizerTest
)
itk_add_test(
  NAME itkRegularStepGradientDescentOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkRegularStepGradientDescentOptimizerTest
)
itk_add_test(
  NAME itkLevenbergMarquardtOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkLevenbergMarquardtOptimizerTest
)
itk_add_test(
  NAME itkVersorRigid3DTransformOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkVersorRigid3DTransformOptimizerTest
)
itk_add_test(
  NAME itkAmoebaOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkAmoebaOptimizerTest
)
itk_add_test(
  NAME itkParticleSwarmOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkParticleSwarmOptimizerTest
)
itk_add_test(
  NAME itkInitializationBiasedParticleSwarmOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkInitializationBiasedParticleSwarmOptimizerTest
    0.7298
    1.49609
    1.49609
    1.49609
)
itk_add_test(
  NAME itkOnePlusOneEvolutionaryOptimizerTest
  COMMAND
    ITKOptimizersTestDriver
    itkOnePlusOneEvolutionaryOptimizerTest
)
