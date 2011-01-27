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

set(KIT Numerics)
set(KIT_LIBS ITKNumerics ITKIO ITKStatistics)
set(KitTests
itkAmoebaOptimizerTest
itkCompositeValleyFunctionTest
itkConjugateGradientOptimizerTest
itkCumulativeGaussianOptimizerTest
itkExhaustiveOptimizerTest
itkFRPROptimizerTest
itkGradientDescentOptimizerTest
itkLBFGSBOptimizerTest
itkLBFGSOptimizerTest
itkLevenbergMarquardtOptimizerTest
itkMultivariateLegendrePolynomialTest
itkNumericsPrintTest
itkNumericsTest
itkOptimizersHierarchyTest
itkPowellOptimizerTest
itkRegularStepGradientDescentOptimizerTest
itkSPSAOptimizerTest
itkSymmetricEigenSystemTest
itkVersorRigid3DTransformOptimizerTest
itkVersorTransformOptimizerTest
)

# Given KIT, KIT_LIBS and KitTests, build TestDriver
CreateTestDriver(${KIT} "${KIT_LIBS}" "${KitTests}" TestDriver)

set(NUMERICS_HEADER_TEST ${CXX_TEST_PATH}/itkNumericsHeaderTest)
add_executable(itkNumericsHeaderTest itkNumericsHeaderTest.cxx)
target_link_libraries(itkNumericsHeaderTest ITKNumerics ITKStatistics)
add_test(itkNumericsHeaderTest ${NUMERICS_HEADER_TEST})

add_test(itkAmoebaOptimizerTest ${TestDriver} itkAmoebaOptimizerTest)
add_test(itkCompositeValleyFunctionTest ${TestDriver} itkCompositeValleyFunctionTest)
add_test(itkConjugateGradientOptimizerTest ${TestDriver} itkConjugateGradientOptimizerTest)
add_test(itkCumulativeGaussianOptimizerTest ${TestDriver} itkCumulativeGaussianOptimizerTest)
add_test(itkExhaustiveOptimizerTest ${TestDriver} itkExhaustiveOptimizerTest)
add_test(itkFRPROptimizerTest ${TestDriver} itkFRPROptimizerTest)
add_test(itkGradientDescentOptimizerTest ${TestDriver} itkGradientDescentOptimizerTest)
add_test(itkLBFGSBOptimizerTest ${TestDriver} itkLBFGSBOptimizerTest)
add_test(itkLBFGSOptimizerTest ${TestDriver} itkLBFGSOptimizerTest)
add_test(itkLevenbergMarquardtOptimizerTest ${TestDriver} itkLevenbergMarquardtOptimizerTest)
add_test(itkMultivariateLegendrePolynomialTest ${TestDriver} itkMultivariateLegendrePolynomialTest)
add_test(itkNumericsPrintTest ${TestDriver} itkNumericsPrintTest)
add_test(itkNumericsTest ${TestDriver} itkNumericsTest)
add_test(itkOptimizersHierarchyTest ${TestDriver} itkOptimizersHierarchyTest)
add_test(itkPowellOptimizerTest ${TestDriver} itkPowellOptimizerTest)
add_test(itkRegularStepGradientDescentOptimizerTest ${TestDriver} itkRegularStepGradientDescentOptimizerTest)
add_test(itkSymmetricEigenSystemTest ${TestDriver} itkSymmetricEigenSystemTest)
add_test(itkSPSAOptimizerTest ${TestDriver} itkSPSAOptimizerTest)
add_test(itkVersorRigid3DTransformOptimizerTest ${TestDriver} itkVersorRigid3DTransformOptimizerTest)
add_test(itkVersorTransformOptimizerTest ${TestDriver} itkVersorTransformOptimizerTest)

if(ITK_OBJCXX_COMPILER_WORKS)
  set(NUMERICS_OBJCXX_TEST ${CXX_TEST_PATH}/itkNumericsHeaderObjCxxTest)
  add_test(itkNumericsHeaderObjCxxTest ${NUMERICS_OBJCXX_TEST})
  add_executable(itkNumericsHeaderObjCxxTest itkNumericsHeaderObjCxxTest.mm)
  target_link_libraries(itkNumericsHeaderObjCxxTest ITKNumerics ITKStatistics)
endif(ITK_OBJCXX_COMPILER_WORKS)

endif( NOT ITK_DISABLE_CXX_TESTING )


