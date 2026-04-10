set(
  ITKOptimizers_SRCS
  itkAmoebaOptimizer.cxx
  itkConjugateGradientOptimizer.cxx
  itkCumulativeGaussianCostFunction.cxx
  itkCumulativeGaussianOptimizer.cxx
  itkExhaustiveOptimizer.cxx
  itkFRPROptimizer.cxx
  itkGradientDescentOptimizer.cxx
  itkInitializationBiasedParticleSwarmOptimizer.cxx
  itkLBFGSBOptimizer.cxx
  itkLBFGSOptimizer.cxx
  itkLevenbergMarquardtOptimizer.cxx
  itkMultipleValuedCostFunction.cxx
  itkMultipleValuedNonLinearOptimizer.cxx
  itkMultipleValuedNonLinearVnlOptimizer.cxx
  itkMultipleValuedVnlCostFunctionAdaptor.cxx
  itkNonLinearOptimizer.cxx
  itkOnePlusOneEvolutionaryOptimizer.cxx
  itkOptimizer.cxx
  itkParticleSwarmOptimizer.cxx
  itkParticleSwarmOptimizerBase.cxx
  itkPowellOptimizer.cxx
  itkQuaternionRigidTransformGradientDescentOptimizer.cxx
  itkRegularStepGradientDescentBaseOptimizer.cxx
  itkRegularStepGradientDescentOptimizer.cxx
  itkSingleValuedCostFunction.cxx
  itkSingleValuedNonLinearOptimizer.cxx
  itkSingleValuedNonLinearVnlOptimizer.cxx
  itkSingleValuedVnlCostFunctionAdaptor.cxx
  itkSPSAOptimizer.cxx
  itkVersorTransformOptimizer.cxx
)

itk_module_add_library(ITKOptimizers ${ITKOptimizers_SRCS})
target_link_libraries(ITKOptimizers PUBLIC ${ITKTransform_LIBRARIES})
