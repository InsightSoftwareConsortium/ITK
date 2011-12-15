itk_module_test()
set(ITKOptimizersTests
itkFRPROptimizerTest.cxx
itkPowellOptimizerTest.cxx
itkGradientDescentOptimizerTest.cxx
itkVersorTransformOptimizerTest.cxx
itkSPSAOptimizerTest.cxx
itkOptimizersHierarchyTest.cxx
itkExhaustiveOptimizerTest.cxx
itkLBFGSBOptimizerTest.cxx
itkConjugateGradientOptimizerTest.cxx
itkLBFGSOptimizerTest.cxx
itkCumulativeGaussianOptimizerTest.cxx
itkRegularStepGradientDescentOptimizerTest.cxx
itkLevenbergMarquardtOptimizerTest.cxx
itkVersorRigid3DTransformOptimizerTest.cxx
itkAmoebaOptimizerTest.cxx
itkOnePlusOneEvolutionaryOptimizerTest.cxx
itkParticleSwarmOptimizerTest.cxx
itkInitializationBiasedParticleSwarmOptimizerTest.cxx
)

CreateTestDriver(ITKOptimizers  "${ITKOptimizers-Test_LIBRARIES}" "${ITKOptimizersTests}")

itk_add_test(NAME itkOptimizersHierarchyTest
      COMMAND ITKOptimizersTestDriver itkOptimizersHierarchyTest)
itk_add_test(NAME itkFRPROptimizerTest
      COMMAND ITKOptimizersTestDriver itkFRPROptimizerTest)
itk_add_test(NAME itkPowellOptimizerTest
      COMMAND ITKOptimizersTestDriver itkPowellOptimizerTest)
itk_add_test(NAME itkGradientDescentOptimizerTest
      COMMAND ITKOptimizersTestDriver itkGradientDescentOptimizerTest)
itk_add_test(NAME itkVersorTransformOptimizerTest
      COMMAND ITKOptimizersTestDriver itkVersorTransformOptimizerTest)
itk_add_test(NAME itkSPSAOptimizerTest
      COMMAND ITKOptimizersTestDriver itkSPSAOptimizerTest)
itk_add_test(NAME itkExhaustiveOptimizerTest
      COMMAND ITKOptimizersTestDriver itkExhaustiveOptimizerTest)
itk_add_test(NAME itkLBFGSBOptimizerTest
      COMMAND ITKOptimizersTestDriver itkLBFGSBOptimizerTest)
itk_add_test(NAME itkConjugateGradientOptimizerTest
      COMMAND ITKOptimizersTestDriver itkConjugateGradientOptimizerTest)
itk_add_test(NAME itkLBFGSOptimizerTest
      COMMAND ITKOptimizersTestDriver itkLBFGSOptimizerTest)
itk_add_test(NAME itkCumulativeGaussianOptimizerTest
      COMMAND ITKOptimizersTestDriver itkCumulativeGaussianOptimizerTest)
itk_add_test(NAME itkRegularStepGradientDescentOptimizerTest
      COMMAND ITKOptimizersTestDriver itkRegularStepGradientDescentOptimizerTest)
itk_add_test(NAME itkLevenbergMarquardtOptimizerTest
      COMMAND ITKOptimizersTestDriver itkLevenbergMarquardtOptimizerTest)
itk_add_test(NAME itkVersorRigid3DTransformOptimizerTest
      COMMAND ITKOptimizersTestDriver itkVersorRigid3DTransformOptimizerTest)
itk_add_test(NAME itkAmoebaOptimizerTest
      COMMAND ITKOptimizersTestDriver itkAmoebaOptimizerTest)
itk_add_test(NAME itkParticleSwarmOptimizerTest
      COMMAND ITKOptimizersTestDriver itkParticleSwarmOptimizerTest)
itk_add_test(NAME itkInitializationBiasedParticleSwarmOptimizerTest
      COMMAND ITKOptimizersTestDriver itkInitializationBiasedParticleSwarmOptimizerTest)
itk_add_test(NAME itkOnePlusOneEvolutionaryOptimizerTest
      COMMAND ITKOptimizersTestDriver itkOnePlusOneEvolutionaryOptimizerTest)
