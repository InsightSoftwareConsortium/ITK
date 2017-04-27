itk_module_test()
set(ITKOptimizersv4Tests
  itkObjectToObjectOptimizerBaseTest.cxx
  itkGradientDescentOptimizerBasev4Test.cxx
  itkGradientDescentOptimizerv4Test.cxx
  itkGradientDescentOptimizerv4Test2.cxx
  itkGradientDescentLineSearchOptimizerv4Test.cxx
  itkConjugateGradientLineSearchOptimizerv4Test.cxx
  itkMultiStartOptimizerv4Test.cxx
  itkMultiGradientOptimizerv4Test.cxx
  itkOptimizerParameterScalesEstimatorTest.cxx
  itkRegistrationParameterScalesEstimatorTest.cxx
  itkRegistrationParameterScalesFromPhysicalShiftTest.cxx
  itkRegistrationParameterScalesFromPhysicalShiftPointSetTest.cxx
  itkRegistrationParameterScalesFromIndexShiftTest.cxx
  itkRegistrationParameterScalesFromJacobianTest.cxx
  itkAutoScaledGradientDescentRegistrationTest.cxx
  itkAutoScaledGradientDescentRegistrationOnVectorTest.cxx
  itkWindowConvergenceMonitoringFunctionTest.cxx
  itkQuasiNewtonOptimizerv4Test.cxx
  itkObjectToObjectMetricBaseTest.cxx
  itkLBFGSOptimizerv4Test.cxx
  itkLBFGS2Optimizerv4Test.cxx
  itkLBFGSBOptimizerv4Test.cxx
  itkRegularStepGradientDescentOptimizerv4Test.cxx
  itkAmoebaOptimizerv4Test.cxx
  itkExhaustiveOptimizerv4Test.cxx
  itkPowellOptimizerv4Test.cxx
  itkOnePlusOneEvolutionaryOptimizerv4Test.cxx
 )

set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(BASELINE_ROOT ${ITK_DATA_ROOT}/Baseline)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

CreateTestDriver(ITKOptimizersv4  "${ITKOptimizersv4-Test_LIBRARIES}" "${ITKOptimizersv4Tests}")

itk_add_test(NAME itkWindowConvergenceMonitoringFunctionTest
         COMMAND ITKOptimizersv4TestDriver itkWindowConvergenceMonitoringFunctionTest)

itk_add_test(NAME itkObjectToObjectOptimizerBaseTest
         COMMAND ITKOptimizersv4TestDriver
         itkObjectToObjectOptimizerBaseTest)

itk_add_test(NAME itkGradientDescentOptimizerBasev4Test
      COMMAND ITKOptimizersv4TestDriver
      itkGradientDescentOptimizerBasev4Test)

itk_add_test(NAME itkGradientDescentOptimizerv4Test
      COMMAND ITKOptimizersv4TestDriver
      itkGradientDescentOptimizerv4Test)

itk_add_test(NAME itkGradientDescentLineSearchOptimizerv4Test
      COMMAND ITKOptimizersv4TestDriver
      itkGradientDescentLineSearchOptimizerv4Test)

itk_add_test(NAME itkConjugateGradientLineSearchOptimizerv4Test
      COMMAND ITKOptimizersv4TestDriver
      itkConjugateGradientLineSearchOptimizerv4Test)

itk_add_test(NAME itkMultiStartOptimizerv4Test
      COMMAND ITKOptimizersv4TestDriver
     itkMultiStartOptimizerv4Test)

itk_add_test(NAME itkMultiGradientOptimizerv4Test
      COMMAND ITKOptimizersv4TestDriver
     itkMultiGradientOptimizerv4Test)

itk_add_test(NAME itkOptimizerParameterScalesEstimatorTest
      COMMAND ITKOptimizersv4TestDriver
      itkOptimizerParameterScalesEstimatorTest)

itk_add_test(NAME itkRegistrationParameterScalesEstimatorTest
      COMMAND ITKOptimizersv4TestDriver
      itkRegistrationParameterScalesEstimatorTest)

itk_add_test(NAME itkRegistrationParameterScalesFromPhysicalShiftTest
      COMMAND ITKOptimizersv4TestDriver
      itkRegistrationParameterScalesFromPhysicalShiftTest)

itk_add_test(NAME itkRegistrationParameterScalesFromJacobianTest
      COMMAND ITKOptimizersv4TestDriver
      itkRegistrationParameterScalesFromJacobianTest)

itk_add_test(NAME itkGradientDescentOptimizerv4Test2
      COMMAND ITKOptimizersv4TestDriver
      itkGradientDescentOptimizerv4Test2)

itk_add_test(NAME itkAutoScaledGradientDescentRegistrationTest
      COMMAND ITKOptimizersv4TestDriver
      itkAutoScaledGradientDescentRegistrationTest 30 1.0 1 0 1)

itk_add_test(NAME itkAutoScaledGradientDescentRegistrationTest2
      COMMAND ITKOptimizersv4TestDriver
      itkAutoScaledGradientDescentRegistrationTest 30 1.0 0 1 1)

itk_add_test(NAME itkAutoScaledGradientDescentRegistrationTest3
      COMMAND ITKOptimizersv4TestDriver
      itkAutoScaledGradientDescentRegistrationTest 30 1.0 0 0 1)

itk_add_test(NAME itkAutoScaledGradientDescentRegistrationTest4
      COMMAND ITKOptimizersv4TestDriver
      itkAutoScaledGradientDescentRegistrationTest 30 1.0 0 1 0)

itk_add_test(NAME itkQuasiNewtonOptimizerv4Test
      COMMAND ITKOptimizersv4TestDriver
      itkQuasiNewtonOptimizerv4Test)

itk_add_test(NAME itkRegistrationParameterScalesFromIndexShiftTest
      COMMAND ITKOptimizersv4TestDriver
      itkRegistrationParameterScalesFromIndexShiftTest)

itk_add_test(NAME itkRegistrationParameterScalesFromPhysicalShiftPointSetTest
      COMMAND ITKOptimizersv4TestDriver
      itkRegistrationParameterScalesFromPhysicalShiftPointSetTest)

itk_add_test(NAME itkObjectToObjectMetricBaseTest
      COMMAND ITKOptimizersv4TestDriver
      itkObjectToObjectMetricBaseTest)

itk_add_test(NAME itkLBFGSOptimizerv4Test
  COMMAND ITKOptimizersv4TestDriver
  itkLBFGSOptimizerv4Test)

itk_add_test(NAME itkLBFGS2Optimizerv4Test
  COMMAND ITKOptimizersv4TestDriver
  itkLBFGS2Optimizerv4Test)

itk_add_test(NAME itkLBFGSBOptimizerv4Test
  COMMAND ITKOptimizersv4TestDriver
  itkLBFGSBOptimizerv4Test)

itk_add_test(NAME itkAmoebaOptimizerv4Test
  COMMAND ITKOptimizersv4TestDriver
  itkAmoebaOptimizerv4Test)

itk_add_test(NAME itkExhaustiveOptimizerv4Test
  COMMAND ITKOptimizersv4TestDriver
  itkExhaustiveOptimizerv4Test)

itk_add_test(NAME itkPowellOptimizerv4Test
  COMMAND ITKOptimizersv4TestDriver
  itkPowellOptimizerv4Test)

itk_add_test(NAME itkOnePlusOneEvolutionaryOptimizerv4Test
  COMMAND ITKOptimizersv4TestDriver
  itkOnePlusOneEvolutionaryOptimizerv4Test)

itk_add_test(NAME itkRegularStepGradientDescentOptimizerv4Test
  COMMAND ITKOptimizersv4TestDriver
  itkRegularStepGradientDescentOptimizerv4Test)
