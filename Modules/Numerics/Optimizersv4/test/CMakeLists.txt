itk_module_test()
set(ITKOptimizersv4Tests
  itkObjectToObjectOptimizerBaseTest.cxx
  itkGradientDescentOptimizerBasev4Test.cxx
  itkGradientDescentOptimizerv4Test.cxx
  itkGradientDescentOptimizerv4Test2.cxx
  itkMultiStartOptimizerv4Test.cxx
  itkMultiGradientOptimizerv4Test.cxx
  itkOptimizerParameterScalesEstimatorTest.cxx
  itkRegistrationParameterScalesEstimatorTest.cxx
  itkRegistrationParameterScalesFromShiftTest.cxx
  itkRegistrationParameterScalesFromJacobianTest.cxx
  itkAutoScaledGradientDescentRegistrationTest.cxx
  itkWindowConvergenceMonitoringFunctionTest.cxx
  itkQuasiNewtonOptimizerv4Test.cxx
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

itk_add_test(NAME itkRegistrationParameterScalesFromShiftTest
      COMMAND ITKOptimizersv4TestDriver
      itkRegistrationParameterScalesFromShiftTest)

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
