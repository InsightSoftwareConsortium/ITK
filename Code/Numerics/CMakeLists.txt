IF(NOT VXL_FROM_UTILITIES)
  SUBDIRS(vxl)
ENDIF(NOT VXL_FROM_UTILITIES)
SUBDIRS(Statistics)
# Sources of non-templated classes.
SET(ITKNumerics_SRCS
itkCacheableScalarFunction.cxx
itkCostFunction.cxx
itkCompositeValleyFunction.cxx
itkMultipleValuedVnlCostFunctionAdaptor.cxx
itkMultivariateLegendrePolynomial.cxx
itkSingleValuedVnlCostFunctionAdaptor.cxx
itkAmoebaOptimizer.cxx
itkConjugateGradientOptimizer.cxx
itkGradientDescentOptimizer.cxx
itkLBFGSOptimizer.cxx
itkLevenbergMarquardtOptimizer.cxx
itkMultipleValuedNonLinearOptimizer.cxx
itkMultipleValuedNonLinearVnlOptimizer.cxx
itkOptimizer.cxx
itkQuaternionRigidTransformGradientDescentOptimizer.cxx
itkRegularStepGradientDescentBaseOptimizer.cxx
itkRegularStepGradientDescentOptimizer.cxx
itkSingleValuedNonLinearOptimizer.cxx
itkSingleValuedNonLinearVnlOptimizer.cxx
itkVersorTransformOptimizer.cxx
itkOnePlusOneEvolutionaryOptimizer.cxx
)

SUBDIRS( FEM )

ADD_LIBRARY(ITKNumerics ${ITKNumerics_SRCS})

TARGET_LINK_LIBRARIES (ITKNumerics ITKCommon VXLNumerics)

INSTALL_TARGETS(/lib/InsightToolkit ITKNumerics)
INSTALL_FILES(/include/InsightToolkit/Numerics "(\\.h|\\.txx)$")
