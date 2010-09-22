add_subdirectory(Statistics)
add_subdirectory(FEM)
add_subdirectory(NeuralNetworks)

add_library(ITKNumerics
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
)

target_link_libraries(ITKNumerics ITKCommon)

if(ITK_LIBRARY_PROPERTIES)
  set_target_properties(ITKNumerics PROPERTIES ${ITK_LIBRARY_PROPERTIES})
endif(ITK_LIBRARY_PROPERTIES)

if(NOT ITK_INSTALL_NO_LIBRARIES)
  install(TARGETS ITKNumerics
    RUNTIME DESTINATION ${ITK_INSTALL_BIN_DIR_CM24} COMPONENT RuntimeLibraries
    LIBRARY DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT RuntimeLibraries
    ARCHIVE DESTINATION ${ITK_INSTALL_LIB_DIR_CM24} COMPONENT Development)
endif(NOT ITK_INSTALL_NO_LIBRARIES)

if(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_NUMERICS ${ITK_INSTALL_INCLUDE_DIR_CM24})
else(ITK_USE_FLAT_DIRECTORY_INSTALL)
  set( ITK_INSTALL_INCLUDE_DIR_CM24_NUMERICS ${ITK_INSTALL_INCLUDE_DIR_CM24}/Numerics)
endif(ITK_USE_FLAT_DIRECTORY_INSTALL)


if(NOT ITK_INSTALL_NO_DEVELOPMENT)
  file(GLOB __files1 "${CMAKE_CURRENT_SOURCE_DIR}/*.h")
  file(GLOB __files2 "${CMAKE_CURRENT_SOURCE_DIR}/*.txx")
  install(FILES ${__files1} ${__files2}
    DESTINATION ${ITK_INSTALL_INCLUDE_DIR_CM24_NUMERICS}
    COMPONENT Development)
endif(NOT ITK_INSTALL_NO_DEVELOPMENT)
