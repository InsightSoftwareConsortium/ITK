SUBDIRS(Statistics)
SUBDIRS(FEM)

IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-Numerics ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/Numerics)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)

SET(NUMERICS_TESTS ${CXX_TEST_PATH}/itkNumericsTests)
SET(NUMERICS_HEADER_TEST ${CXX_TEST_PATH}/itkNumericsHeaderTest)

ADD_TEST(itkAmoebaOptimizerTest ${NUMERICS_TESTS} itkAmoebaOptimizerTest)
ADD_TEST(itkConjugateGradientOptimizerTest ${NUMERICS_TESTS} itkConjugateGradientOptimizerTest)
ADD_TEST(itkGradientDescentOptimizerTest ${NUMERICS_TESTS} itkGradientDescentOptimizerTest)
ADD_TEST(itkLevenbergMarquardtOptimizerTest ${NUMERICS_TESTS} itkLevenbergMarquardtOptimizerTest)
ADD_TEST(itkLBFGSOptimizerTest ${NUMERICS_TESTS} itkLBFGSOptimizerTest)
ADD_TEST(itkMultivariateLegendrePolynomialTest ${NUMERICS_TESTS} itkMultivariateLegendrePolynomialTest)
ADD_TEST(itkNumericsTest ${NUMERICS_TESTS} itkNumericsTest)
ADD_TEST(itkOptimizersHierarchyTest ${NUMERICS_TESTS} itkOptimizersHierarchyTest)
ADD_TEST(itkRegularStepGradientDescentOptimizerTest ${NUMERICS_TESTS} itkRegularStepGradientDescentOptimizerTest)
ADD_TEST(itkVersorTransformOptimizerTest ${NUMERICS_TESTS} itkVersorTransformOptimizerTest)
ADD_TEST(itkNumericsHeaderTest ${NUMERICS_HEADER_TEST})

SET(Numerics_SRCS
itkAmoebaOptimizerTest.cxx
itkConjugateGradientOptimizerTest.cxx
itkGradientDescentOptimizerTest.cxx
itkLevenbergMarquardtOptimizerTest.cxx
itkLBFGSOptimizerTest.cxx
itkMultivariateLegendrePolynomialTest.cxx
itkNumericsTest.cxx
itkOptimizersHierarchyTest.cxx
itkRegularStepGradientDescentOptimizerTest.cxx
itkVersorTransformOptimizerTest.cxx
)

ADD_EXECUTABLE(itkNumericsHeaderTest itkNumericsHeaderTest.cxx)
TARGET_LINK_LIBRARIES(itkNumericsHeaderTest ITKCommon ITKNumerics)
ADD_EXECUTABLE(itkNumericsTests itkNumericsTests.cxx ${Numerics_SRCS})
TARGET_LINK_LIBRARIES(itkNumericsTests ITKCommon ITKNumerics ITKIO)

