SUBDIRS(Statistics FEM)

ADD_LIBRARY(ITKNumerics
  itkAmoebaOptimizer.cxx
  itkCacheableScalarFunction.cxx
  itkCompositeValleyFunction.cxx
  itkConjugateGradientOptimizer.cxx
  itkCostFunction.cxx
  itkCumulativeGaussianCostFunction.cxx
  itkCumulativeGaussianOptimizer.cxx
  itkExhaustiveOptimizer.cxx
  itkFRPROptimizer.cxx
  itkGradientDescentOptimizer.cxx
  itkLBFGSBOptimizer.cxx
  itkLBFGSOptimizer.cxx
  itkLevenbergMarquardtOptimizer.cxx
  itkMultipleValuedNonLinearOptimizer.cxx
  itkMultipleValuedNonLinearVnlOptimizer.cxx
  itkMultipleValuedVnlCostFunctionAdaptor.cxx
  itkMultivariateLegendrePolynomial.cxx
  itkOnePlusOneEvolutionaryOptimizer.cxx
  itkOptimizer.cxx
  itkPowellOptimizer.cxx
  itkQuaternionRigidTransformGradientDescentOptimizer.cxx
  itkRegularStepGradientDescentBaseOptimizer.cxx
  itkRegularStepGradientDescentOptimizer.cxx
  itkSPSAOptimizer.cxx
  itkSingleValuedNonLinearOptimizer.cxx
  itkSingleValuedNonLinearVnlOptimizer.cxx
  itkSingleValuedVnlCostFunctionAdaptor.cxx
  itkVersorRigid3DTransformOptimizer.cxx
  itkVersorTransformOptimizer.cxx
  lbfgsb.c
)

TARGET_LINK_LIBRARIES (ITKNumerics ITKCommon)

INSTALL_TARGETS(/lib/InsightToolkit ITKNumerics)
INSTALL_FILES(/include/InsightToolkit/Numerics "(\\.h|\\.txx)$")
