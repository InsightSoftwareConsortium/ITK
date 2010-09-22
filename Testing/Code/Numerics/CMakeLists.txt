add_subdirectory(Statistics)

add_subdirectory(FEM)
add_subdirectory(NeuralNetworks)

if( NOT ITK_DISABLE_CXX_TESTING )
if(ITK_BINARY_DIR)
  if(TCL_TCLSH)
    add_test(PrintSelf-Numerics ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl
             ${ITK_SOURCE_DIR}/Code/Numerics)
  endif(TCL_TCLSH)
endif(ITK_BINARY_DIR)


set(NUMERICS_TESTS ${CXX_TEST_PATH}/itkNumericsTests)
set(NUMERICS_HEADER_TEST ${CXX_TEST_PATH}/itkNumericsHeaderTest)

add_test(itkAmoebaOptimizerTest ${NUMERICS_TESTS} itkAmoebaOptimizerTest)
add_test(itkCompositeValleyFunctionTest ${NUMERICS_TESTS} itkCompositeValleyFunctionTest)
add_test(itkConjugateGradientOptimizerTest ${NUMERICS_TESTS} itkConjugateGradientOptimizerTest)
add_test(itkCumulativeGaussianOptimizerTest ${NUMERICS_TESTS} itkCumulativeGaussianOptimizerTest)
add_test(itkExhaustiveOptimizerTest ${NUMERICS_TESTS} itkExhaustiveOptimizerTest)
add_test(itkFRPROptimizerTest ${NUMERICS_TESTS} itkFRPROptimizerTest)
add_test(itkGradientDescentOptimizerTest ${NUMERICS_TESTS} itkGradientDescentOptimizerTest)
add_test(itkLBFGSBOptimizerTest ${NUMERICS_TESTS} itkLBFGSBOptimizerTest)
add_test(itkLBFGSOptimizerTest ${NUMERICS_TESTS} itkLBFGSOptimizerTest)
add_test(itkLevenbergMarquardtOptimizerTest ${NUMERICS_TESTS} itkLevenbergMarquardtOptimizerTest)
add_test(itkMultivariateLegendrePolynomialTest ${NUMERICS_TESTS} itkMultivariateLegendrePolynomialTest)
add_test(itkNumericsHeaderTest ${NUMERICS_HEADER_TEST})
add_test(itkNumericsPrintTest ${NUMERICS_TESTS} itkNumericsPrintTest)
add_test(itkNumericsTest ${NUMERICS_TESTS} itkNumericsTest)
add_test(itkOptimizersHierarchyTest ${NUMERICS_TESTS} itkOptimizersHierarchyTest)
add_test(itkPowellOptimizerTest ${NUMERICS_TESTS} itkPowellOptimizerTest)
add_test(itkRegularStepGradientDescentOptimizerTest ${NUMERICS_TESTS} itkRegularStepGradientDescentOptimizerTest)
add_test(itkSymmetricEigenSystemTest ${NUMERICS_TESTS} itkSymmetricEigenSystemTest)
add_test(itkSPSAOptimizerTest ${NUMERICS_TESTS} itkSPSAOptimizerTest)
add_test(itkVersorRigid3DTransformOptimizerTest ${NUMERICS_TESTS} itkVersorRigid3DTransformOptimizerTest)
add_test(itkVersorTransformOptimizerTest ${NUMERICS_TESTS} itkVersorTransformOptimizerTest)

set(Numerics_SRCS
itkAmoebaOptimizerTest.cxx
itkCompositeValleyFunctionTest.cxx
itkConjugateGradientOptimizerTest.cxx
itkCumulativeGaussianOptimizerTest.cxx
itkExhaustiveOptimizerTest.cxx
itkFRPROptimizerTest.cxx
itkGradientDescentOptimizerTest.cxx
itkLBFGSBOptimizerTest.cxx
itkLBFGSOptimizerTest.cxx
itkLevenbergMarquardtOptimizerTest.cxx
itkMultivariateLegendrePolynomialTest.cxx
itkNumericsPrintTest.cxx
itkNumericsTest.cxx
itkOptimizersHierarchyTest.cxx
itkPowellOptimizerTest.cxx
itkRegularStepGradientDescentOptimizerTest.cxx
itkSPSAOptimizerTest.cxx
itkSymmetricEigenSystemTest.cxx
itkVersorRigid3DTransformOptimizerTest.cxx
itkVersorTransformOptimizerTest.cxx
)

if(ITK_OBJCXX_COMPILER_WORKS)
  set(NUMERICS_OBJCXX_TEST ${CXX_TEST_PATH}/itkNumericsHeaderObjCxxTest)
  add_test(itkNumericsHeaderObjCxxTest ${NUMERICS_OBJCXX_TEST})
  add_executable(itkNumericsHeaderObjCxxTest itkNumericsHeaderObjCxxTest.mm)
  target_link_libraries(itkNumericsHeaderObjCxxTest ITKNumerics ITKStatistics)
endif(ITK_OBJCXX_COMPILER_WORKS)

add_executable(itkNumericsHeaderTest itkNumericsHeaderTest.cxx)
target_link_libraries(itkNumericsHeaderTest ITKNumerics ITKStatistics)

add_executable(itkNumericsTests itkNumericsTests.cxx ${Numerics_SRCS})
target_link_libraries(itkNumericsTests ITKNumerics ITKIO ITKStatistics)

endif( NOT ITK_DISABLE_CXX_TESTING )


