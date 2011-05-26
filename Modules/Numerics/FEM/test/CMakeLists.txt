itk_module_test()
set(ITK-FEMTests
itkFEMHeaderTest.cxx
itkFEMElement2DMembraneTest.cxx
itkFEMElement2DQuadraticTriangularTest.cxx
itkFEMElement2DStrainTest.cxx
itkFEMElement3DMembraneTest.cxx
itkFEMElementTest.cxx
itkFEMExceptionTest.cxx
itkFEMGenerateMeshTest.cxx
itkFEMLinearSystemWrapperDenseVNLTest.cxx
itkFEMLinearSystemWrapperItpackTest.cxx
itkFEMLinearSystemWrapperItpackTest2.cxx
itkFEMLinearSystemWrapperVNLTest.cxx
itkFEMPArrayTest.cxx
)

CreateTestDriver(ITK-FEM  "${ITK-FEM-Test_LIBRARIES}" "${ITK-FEMTests}")

add_test(NAME itkFEMHeaderTest
      COMMAND ITK-FEMTestDriver itkFEMHeaderTest)
add_test(NAME itkFEMElement2DMembraneTest
      COMMAND ITK-FEMTestDriver itkFEMElement2DMembraneTest)
add_test(NAME itkFEMElement2DQuadraticTriangularTest
      COMMAND ITK-FEMTestDriver itkFEMElement2DQuadraticTriangularTest)
add_test(NAME itkFEMElement2DStrainTest
      COMMAND ITK-FEMTestDriver itkFEMElement2DStrainTest)
add_test(NAME itkFEMElement3DMembraneTest
      COMMAND ITK-FEMTestDriver itkFEMElement3DMembraneTest)
add_test(NAME itkFEMC0HexahedralElement-NodalLoads-BCs
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/hexa2.fem)
add_test(NAME itkFEMC0HexahedralElement-NoLoads
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/hexa3.fem)
add_test(NAME itkFEMC0HexahedralElement-GravityLoad-BCs
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/hexa4-grav.fem)
add_test(NAME itkFEMC0QuadElement-NodalLoads-BCs
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/quad2-small.fem)
add_test(NAME itkFEMC0QuadElement-Strain
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/quad2-strain.fem)
add_test(NAME itkFEMC0QuadElement-NoLoads
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/quad4.fem)
add_test(NAME itkFEMC0QuadElement-GravityLoad-BCs
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/quad6-grav.fem)
add_test(NAME itkFEMLoadLandmarkImplementation
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/quad-lm.fem)
add_test(NAME itkFEMC0TetrahedralElement-NodalLoads-BCs
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tetra2.fem)
add_test(NAME itkFEMC0TetrahedralElement-NoLoads
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tetra3.fem)
add_test(NAME itkFEMC0TetrahedralElement-GravityLoad-BCs
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tetra4-grav.fem)
add_test(NAME itkFEMC0QuadElementTrapezoidalGeometry-NoLoads
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/trapezoid.fem)
add_test(NAME itkFEMC0TriangularElement-NodalLoads-BCs
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tri2.fem)
add_test(NAME itkFEMC0TriangularElement-NoLoads
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tri3.fem)
add_test(NAME itkFEMC0TriangularElement-Strain
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tri3-e.fem)
add_test(NAME itkFEMC0TriangularElement-Quadratic
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tri3-q.fem)
add_test(NAME itkFEMTruss
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/truss.fem)
add_test(NAME itkFEMExceptionTest
      COMMAND ITK-FEMTestDriver itkFEMExceptionTest)
add_test(NAME itkFEMGenerateMeshTest
      COMMAND ITK-FEMTestDriver itkFEMGenerateMeshTest)
add_test(NAME itkFEMLinearSystemWrapperDenseVNLTest
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperDenseVNLTest)
add_test(NAME itkFEMLinearSystemWrapperItpackTest
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest)
add_test(NAME itkFEMLinearSystemWrapperItpackTest1
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              0)
add_test(NAME itkFEMLinearSystemWrapperItpackTest2
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              1)
add_test(NAME itkFEMLinearSystemWrapperItpackTest3
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              2)
add_test(NAME itkFEMLinearSystemWrapperItpackTest4
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              3)
add_test(NAME itkFEMLinearSystemWrapperItpackTest5
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              4)
add_test(NAME itkFEMLinearSystemWrapperItpackTest6
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              5)
add_test(NAME itkFEMLinearSystemWrapperItpackTest7
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              6)
add_test(NAME itkFEMLinearSystemWrapperItpackTest8
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              0)
add_test(NAME itkFEMLinearSystemWrapperItpackTest9
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              1)
add_test(NAME itkFEMLinearSystemWrapperItpackTest10
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              2)
add_test(NAME itkFEMLinearSystemWrapperItpackTest11
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              3)
add_test(NAME itkFEMLinearSystemWrapperItpackTest12
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              4)
add_test(NAME itkFEMLinearSystemWrapperItpackTest13
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              5)
add_test(NAME itkFEMLinearSystemWrapperItpackTest14
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              6)
add_test(NAME itkFEMLinearSystemWrapperVNLTest
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperVNLTest)
add_test(NAME itkFEMPArrayTest
      COMMAND ITK-FEMTestDriver itkFEMPArrayTest)
