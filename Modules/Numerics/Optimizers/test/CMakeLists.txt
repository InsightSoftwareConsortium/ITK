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

itk_add_test(NAME itkOptimizersHeaderTest
      COMMAND ITK-OptimizersTestDriver itkOptimizersHeaderTest)
itk_add_test(NAME itkFRPROptimizerTest
      COMMAND ITK-OptimizersTestDriver itkFRPROptimizerTest)
itk_add_test(NAME itkPowellOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkPowellOptimizerTest)
itk_add_test(NAME itkGradientDescentOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkGradientDescentOptimizerTest)
itk_add_test(NAME itkVersorTransformOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkVersorTransformOptimizerTest)
itk_add_test(NAME itkSPSAOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkSPSAOptimizerTest)
itk_add_test(NAME itkExhaustiveOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkExhaustiveOptimizerTest)
itk_add_test(NAME itkLBFGSBOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkLBFGSBOptimizerTest)
itk_add_test(NAME itkConjugateGradientOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkConjugateGradientOptimizerTest)
itk_add_test(NAME itkLBFGSOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkLBFGSOptimizerTest)
itk_add_test(NAME itkCumulativeGaussianOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkCumulativeGaussianOptimizerTest)
itk_add_test(NAME itkRegularStepGradientDescentOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkRegularStepGradientDescentOptimizerTest)
itk_add_test(NAME itkLevenbergMarquardtOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkLevenbergMarquardtOptimizerTest)
itk_add_test(NAME itkVersorRigid3DTransformOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkVersorRigid3DTransformOptimizerTest)
itk_add_test(NAME itkAmoebaOptimizerTest
      COMMAND ITK-OptimizersTestDriver itkAmoebaOptimizerTest)
