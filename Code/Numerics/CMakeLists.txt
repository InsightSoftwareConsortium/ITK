SUBDIRS(vxl Statistics)
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

SET(HAS_FEM 0 CACHE INTERNAL "Support for FEM")
OPTION(BUILD_FEM_CODE "Build the FEM Code" ON)
IF(BUILD_FEM_CODE)
  SUBDIRS( FEM )
  SET(HAS_FEM 1 CACHE INTERNAL "Support for FEM")
ENDIF(BUILD_FEM_CODE)

ADD_LIBRARY(ITKNumerics ${ITKNumerics_SRCS})

TARGET_LINK_LIBRARIES (ITKNumerics ITKCommon VXLNumerics)

INSTALL_TARGETS(/lib/InsightToolkit ITKNumerics)
INSTALL_FILES(/include/InsightToolkit/Numerics "(\\.h|\\.txx)$")
