SUBDIRS(Statistics FEM)

ADD_LIBRARY(ITKNumerics
  itkAmoebaOptimizer.cxx
  itkCacheableScalarFunction.cxx
  itkCompositeValleyFunction.cxx
  itkConjugateGradientOptimizer.cxx
  itkCostFunction.cxx
  itkGradientDescentOptimizer.cxx
  itkLBFGSOptimizer.cxx
  itkLevenbergMarquardtOptimizer.cxx
  itkMultipleValuedNonLinearOptimizer.cxx
  itkMultipleValuedNonLinearVnlOptimizer.cxx
  itkMultipleValuedVnlCostFunctionAdaptor.cxx
  itkMultivariateLegendrePolynomial.cxx
  itkOnePlusOneEvolutionaryOptimizer.cxx
  itkOptimizer.cxx
  itkQuaternionRigidTransformGradientDescentOptimizer.cxx
  itkRegularStepGradientDescentBaseOptimizer.cxx
  itkRegularStepGradientDescentOptimizer.cxx
  itkSingleValuedNonLinearOptimizer.cxx
  itkSingleValuedNonLinearVnlOptimizer.cxx
  itkSingleValuedVnlCostFunctionAdaptor.cxx
  itkVersorTransformOptimizer.cxx
  itkCumulativeGaussianOptimizer.cxx
  itkCumulativeGaussianCostFunction.cxx
)

TARGET_LINK_LIBRARIES (ITKNumerics ITKCommon)

INSTALL_TARGETS(/lib/InsightToolkit ITKNumerics)
INSTALL_FILES(/include/InsightToolkit/Numerics "(\\.h|\\.txx)$")
