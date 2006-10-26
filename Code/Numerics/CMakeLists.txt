SUBDIRS(Statistics FEM NeuralNetworks)

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

TARGET_LINK_LIBRARIES(ITKNumerics ITKCommon)

IF(ITK_LIBRARY_PROPERTIES)
  SET_TARGET_PROPERTIES(ITKNumerics PROPERTIES ${ITK_LIBRARY_PROPERTIES})
ENDIF(ITK_LIBRARY_PROPERTIES)

IF(NOT ITK_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS ITKNumerics
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_LIBRARIES)

IF(NOT ITK_INSTALL_NO_DEVELOPMENT)
  FILE(GLOB __files1 "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  FILE(GLOB __files2 "${CMAKE_CURRENT_SOURCE_DIR}/*.txx")
  INSTALL(FILES ${__files1} ${__files2}
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24}/Numerics
    COMPONENT Development)
ENDIF(NOT ITK_INSTALL_NO_DEVELOPMENT)
