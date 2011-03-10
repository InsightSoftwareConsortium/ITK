itk_module_test()
set(ITK-OptimizersTests
itkFRPROptimizerTest.cxx
itkPowellOptimizerTest.cxx
itkGradientDescentOptimizerTest.cxx
itkVersorTransformOptimizerTest.cxx
itkSPSAOptimizerTest.cxx
itkOptimizersHeaderTest.cxx
itkExhaustiveOptimizerTest.cxx
itkLBFGSBOptimizerTest.cxx
itkConjugateGradientOptimizerTest.cxx
itkLBFGSOptimizerTest.cxx
itkCumulativeGaussianOptimizerTest.cxx
itkRegularStepGradientDescentOptimizerTest.cxx
itkLevenbergMarquardtOptimizerTest.cxx
itkVersorRigid3DTransformOptimizerTest.cxx
itkAmoebaOptimizerTest.cxx
)

CreateTestDriver(ITK-Optimizers  "${ITK-Optimizers-Test_LIBRARIES}" "${ITK-OptimizersTests}")

add_test(NAME itkOptimizersHeaderTest
      COMMAND ITK-OptimizersTestDriver itkOptimizersHeaderTest)
add_test(NAME itkFRPROptimizerTest
      COMMAND ITK-OptimizersTestDriver itkFRPROptimizerTest)
add_test(NAME itkPowellOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkPowellOptimizerTest)
add_test(NAME itkGradientDescentOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkGradientDescentOptimizerTest)
add_test(NAME itkVersorTransformOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkVersorTransformOptimizerTest)
add_test(NAME itkSPSAOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkSPSAOptimizerTest)
add_test(NAME itkExhaustiveOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkExhaustiveOptimizerTest)
add_test(NAME itkLBFGSBOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkLBFGSBOptimizerTest)
add_test(NAME itkConjugateGradientOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkConjugateGradientOptimizerTest)
add_test(NAME itkLBFGSOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkLBFGSOptimizerTest)
add_test(NAME itkCumulativeGaussianOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkCumulativeGaussianOptimizerTest)
add_test(NAME itkRegularStepGradientDescentOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkRegularStepGradientDescentOptimizerTest)
add_test(NAME itkLevenbergMarquardtOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkLevenbergMarquardtOptimizerTest)
add_test(NAME itkVersorRigid3DTransformOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkVersorRigid3DTransformOptimizerTest)
add_test(NAME itkAmoebaOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkAmoebaOptimizerTest)
