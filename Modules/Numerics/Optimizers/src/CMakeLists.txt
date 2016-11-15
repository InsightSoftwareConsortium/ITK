set(ITKOptimizers_SRCS
  itkNonLinearOptimizer.cxx
  itkMultipleValuedCostFunction.cxx
  itkSingleValuedCostFunction.cxx
  itkConjugateGradientOptimizer.cxx
  itkRegularStepGradientDescentOptimizer.cxx
  itkLBFGSOptimizer.cxx
  itkExhaustiveOptimizer.cxx
  itkLevenbergMarquardtOptimizer.cxx
  itkSingleValuedNonLinearVnlOptimizer.cxx
  itkQuaternionRigidTransformGradientDescentOptimizer.cxx
  itkSPSAOptimizer.cxx
  itkSingleValuedVnlCostFunctionAdaptor.cxx
  itkMultipleValuedNonLinearVnlOptimizer.cxx
  itkPowellOptimizer.cxx
  itkVersorRigid3DTransformOptimizer.cxx
  itkVersorTransformOptimizer.cxx
  itkOnePlusOneEvolutionaryOptimizer.cxx
  itkMultipleValuedNonLinearOptimizer.cxx
  itkLBFGSBOptimizer.cxx
  itkCumulativeGaussianOptimizer.cxx
  itkAmoebaOptimizer.cxx
  itkCumulativeGaussianCostFunction.cxx
  itkOptimizer.cxx
  itkFRPROptimizer.cxx
  itkGradientDescentOptimizer.cxx
  itkSingleValuedNonLinearOptimizer.cxx
  itkRegularStepGradientDescentBaseOptimizer.cxx
  itkMultipleValuedVnlCostFunctionAdaptor.cxx
  itkParticleSwarmOptimizerBase.cxx
  itkParticleSwarmOptimizer.cxx
  itkInitializationBiasedParticleSwarmOptimizer.cxx
  )

itk_module_add_library(ITKOptimizers ${ITKOptimizers_SRCS})
target_link_libraries(ITKOptimizers LINK_PUBLIC ${ITKTransform_LIBRARIES})
