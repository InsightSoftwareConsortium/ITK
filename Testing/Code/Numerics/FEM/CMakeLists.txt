IF(ITK_BINARY_DIR)
  IF(TCL_TCLSH)
    ADD_TEST(PrintSelf-Statistics ${TCL_TCLSH}
             ${ITK_SOURCE_DIR}/Utilities/Dart/PrintSelfCheck.tcl  
             ${ITK_SOURCE_DIR}/Code/Numerics/Statistics)
  ENDIF(TCL_TCLSH)
ENDIF(ITK_BINARY_DIR)

SET(FEM_TESTS ${CXX_TEST_PATH}/itkFEMTests)

SET(FEM_SRCS
itkFEMElementTest.cxx
itkFEMLinearSystemWrapperItpackTest.cxx
itkFEMLinearSystemWrapperVNLTest.cxx
itkFEMLinearSystemWrapperDenseVNLTest.cxx
)

#ADD_TEST(itkFEMElementTestMenu ${FEM_TESTS} itkFEMElementTestMenu)

ADD_TEST(itkFEMC0HexahedralElement-NodalLoads-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/hexa2.fem)
ADD_TEST(itkFEMC0HexahedralElement-NoLoads ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/hexa3.fem)
ADD_TEST(itkFEMC0HexahedralElement-GravityLoad-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/hexa4-grav.fem)
ADD_TEST(itkFEMC0QuadElement-NodalLoads-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad2-small.fem)
ADD_TEST(itkFEMC0QuadElement-NoLoads ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad4.fem)
ADD_TEST(itkFEMC0QuadElement-GravityLoad-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad6-grav.fem)

# Edge loads are not re-implemented yet
#ADD_TEST(itkFEMC0QuadElement-EdgeLoad-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/quad_1_edge.fem)

ADD_TEST(itkFEMC0TetrahedralElement-NodalLoads-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tetra2.fem)
ADD_TEST(itkFEMC0TetrahedralElement-NoLoads ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tetra3.fem)
ADD_TEST(itkFEMC0TetrahedralElement-GravityLoad-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tetra4-grav.fem)
ADD_TEST(itkFEMC0QuadElementTrapezoidalGeometry-NoLoads ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/trapezoid.fem)
ADD_TEST(itkFEMC0TriangularElement-NodalLoads-BCs ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tri2.fem)
ADD_TEST(itkFEMC0TriangularElement-NoLoads ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/tri3.fem)
ADD_TEST(itkFEMTruss ${FEM_TESTS} itkFEMElementTest ${ITK_DATA_ROOT}/Input/FEM/truss.fem)
ADD_TEST(itkFEMLinearSystemWrapperItpackTest  ${FEM_TESTS} itkFEMLinearSystemWrapperItpackTest)
ADD_TEST(itkFEMLinearSystemWrapperVNLTest  ${FEM_TESTS} itkFEMLinearSystemWrapperVNLTest)
ADD_TEST(itkFEMLinearSystemWrapperDenseVNLTest  ${FEM_TESTS} itkFEMLinearSystemWrapperDenseVNLTest)

#ADD_TEST(itkFEMBar2DTest ${FEM_TESTS} itkFEMBar2DTest)

# NOTE: the two commands below are mutually exclusive!  Do not
# uncomment both at the same time!

# To run the FEM input menu-based test locally, uncomment the line below
# ADD_EXECUTABLE(itkFEMElementTestMenu itkFEMElementTestMenu)

# To create the standard ITK test program, uncomment the line below
ADD_EXECUTABLE(itkFEMTests itkFEMTests.cxx ${FEM_SRCS})
IF(UNIX)
  TARGET_LINK_LIBRARIES (itkFEMTests -lm )
ENDIF(UNIX)
TARGET_LINK_LIBRARIES(itkFEMTests VXLNumerics ITKCommon ITKNumerics ITKFEM)
