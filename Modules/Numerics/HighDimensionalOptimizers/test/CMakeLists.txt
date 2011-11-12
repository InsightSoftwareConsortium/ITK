itk_module_test()
set(ITKHighDimensionalOptimizersTests
  itkArray1DToDataTest.cxx
  itkObjectToObjectMetricTest.cxx
  itkObjectToObjectOptimizerBaseTest.cxx
  itkGradientDescentObjectOptimizerBaseTest.cxx
  itkGradientDescentObjectOptimizerTest.cxx
  itkGradientDescentObjectOptimizerTest2.cxx
  itkImageToDataTest.cxx
  itkOptimizerParameterScalesEstimatorTest.cxx
  itkRegistrationParameterScalesEstimatorTest.cxx
  itkRegistrationParameterScalesFromShiftTest.cxx
  itkRegistrationParameterScalesFromJacobianTest.cxx
  itkAutoScaledGradientDescentRegistrationTest.cxx
  itkWindowConvergenceMonitoringFunctionTest.cxx
)

set(INPUTDATA ${ITK_DATA_ROOT}/Input)
set(BASELINE_ROOT ${ITK_DATA_ROOT}/Baseline)
set(TEMP ${ITK_TEST_OUTPUT_DIR})

CreateTestDriver(ITKHighDimensionalOptimizers  "${ITKHighDimensionalOptimizers-Test_LIBRARIES}" "${ITKHighDimensionalOptimizersTests}")

itk_add_test(NAME itkArray1DToDataTest
         COMMAND ITKHighDimensionalOptimizersTestDriver itkArray1DToDataTest)

itk_add_test(NAME itkWindowConvergenceMonitoringFunctionTest
         COMMAND ITKHighDimensionalOptimizersTestDriver itkWindowConvergenceMonitoringFunctionTest)

itk_add_test(NAME itkObjectToObjectMetricTest
      COMMAND ITKHighDimensionalOptimizersTestDriver itkObjectToObjectMetricTest)

itk_add_test(NAME itkObjectToObjectOptimizerBaseTest
         COMMAND ITKHighDimensionalOptimizersTestDriver
         itkObjectToObjectOptimizerBaseTest)

itk_add_test(NAME itkGradientDescentObjectOptimizerBaseTest
      COMMAND ITKHighDimensionalOptimizersTestDriver
      itkGradientDescentObjectOptimizerBaseTest)

itk_add_test(NAME itkGradientDescentObjectOptimizerTest
      COMMAND ITKHighDimensionalOptimizersTestDriver
      itkGradientDescentObjectOptimizerTest)

itk_add_test(NAME itkImageToDataTest
         COMMAND ITKHighDimensionalOptimizersTestDriver itkImageToDataTest)

itk_add_test(NAME itkOptimizerParameterScalesEstimatorTest
      COMMAND ITKHighDimensionalOptimizersTestDriver
      itkOptimizerParameterScalesEstimatorTest)

itk_add_test(NAME itkRegistrationParameterScalesEstimatorTest
      COMMAND ITKHighDimensionalOptimizersTestDriver
      itkRegistrationParameterScalesEstimatorTest)

itk_add_test(NAME itkRegistrationParameterScalesFromShiftTest
      COMMAND ITKHighDimensionalOptimizersTestDriver
      itkRegistrationParameterScalesFromShiftTest)

itk_add_test(NAME itkRegistrationParameterScalesFromJacobianTest
      COMMAND ITKHighDimensionalOptimizersTestDriver
      itkRegistrationParameterScalesFromJacobianTest)

itk_add_test(NAME itkGradientDescentObjectOptimizerTest2
      COMMAND ITKHighDimensionalOptimizersTestDriver
      itkGradientDescentObjectOptimizerTest2)

itk_add_test(NAME itkAutoScaledGradientDescentRegistrationTest
      COMMAND ITKHighDimensionalOptimizersTestDriver
      itkAutoScaledGradientDescentRegistrationTest)
