SET(KIT Numerics)
CREATE_TEST_SOURCELIST(Tests itk${KIT}Tests.cxx 
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

ADD_EXECUTABLE(itk${KIT}Tests ${Tests})
TARGET_LINK_LIBRARIES(itk${KIT}Tests ITKNumerics)

SET (TestsToRun ${Tests})
REMOVE (TestsToRun itk${KIT}Tests.cxx)

FOREACH (test ${TestsToRun})
  GET_FILENAME_COMPONENT(TName ${test} NAME_WE)
  ADD_TEST(${TName} ${CXX_TEST_PATH}/itk${KIT}Tests ${TName}) 
ENDFOREACH (test)

# Add PrintSelf test.
IF (TCL_TCLSH)
  ADD_TEST(PrintSelf-${KIT} ${TCL_TCLSH}
           ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
           ${ITK_SOURCE_DIR}/Code/${KIT})
ENDIF (TCL_TCLSH)

# Add header test.
ADD_EXECUTABLE(itk${KIT}HeaderTest itk${KIT}HeaderTest.cxx)
TARGET_LINK_LIBRARIES(itk${KIT}HeaderTest ITKNumerics)
ADD_TEST(itk${KIT}HeaderTest ${CXX_TEST_PATH}/itk${KIT}HeaderTest)

SUBDIRS(vxl Statistics FEM)
