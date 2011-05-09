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

itk_add_test(NAME itkFEMHeaderTest
      COMMAND ITK-FEMTestDriver itkFEMHeaderTest)
itk_add_test(NAME itkFEMElement2DMembraneTest
      COMMAND ITK-FEMTestDriver itkFEMElement2DMembraneTest)
itk_add_test(NAME itkFEMElement2DQuadraticTriangularTest
      COMMAND ITK-FEMTestDriver itkFEMElement2DQuadraticTriangularTest)
itk_add_test(NAME itkFEMElement2DStrainTest
      COMMAND ITK-FEMTestDriver itkFEMElement2DStrainTest)
itk_add_test(NAME itkFEMElement3DMembraneTest
      COMMAND ITK-FEMTestDriver itkFEMElement3DMembraneTest)
itk_add_test(NAME itkFEMC0HexahedralElement-NodalLoads-BCs
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/hexa2.fem)
itk_add_test(NAME itkFEMC0HexahedralElement-NoLoads
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/hexa3.fem)
itk_add_test(NAME itkFEMC0HexahedralElement-GravityLoad-BCs
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/hexa4-grav.fem)
itk_add_test(NAME itkFEMC0QuadElement-NodalLoads-BCs
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/quad2-small.fem)
itk_add_test(NAME itkFEMC0QuadElement-Strain
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/quad2-strain.fem)
itk_add_test(NAME itkFEMC0QuadElement-NoLoads
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/quad4.fem)
itk_add_test(NAME itkFEMC0QuadElement-GravityLoad-BCs
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/quad6-grav.fem)
itk_add_test(NAME itkFEMLoadLandmarkImplementation
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/quad-lm.fem)
itk_add_test(NAME itkFEMC0TetrahedralElement-NodalLoads-BCs
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tetra2.fem)
itk_add_test(NAME itkFEMC0TetrahedralElement-NoLoads
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tetra3.fem)
itk_add_test(NAME itkFEMC0TetrahedralElement-GravityLoad-BCs
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tetra4-grav.fem)
itk_add_test(NAME itkFEMC0QuadElementTrapezoidalGeometry-NoLoads
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/trapezoid.fem)
itk_add_test(NAME itkFEMC0TriangularElement-NodalLoads-BCs
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tri2.fem)
itk_add_test(NAME itkFEMC0TriangularElement-NoLoads
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tri3.fem)
itk_add_test(NAME itkFEMC0TriangularElement-Strain
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tri3-e.fem)
itk_add_test(NAME itkFEMC0TriangularElement-Quadratic
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/tri3-q.fem)
itk_add_test(NAME itkFEMTruss
      COMMAND ITK-FEMTestDriver itkFEMElementTest
              ${ITK_DATA_ROOT}/Input/FEM/truss.fem)
itk_add_test(NAME itkFEMExceptionTest
      COMMAND ITK-FEMTestDriver itkFEMExceptionTest)
itk_add_test(NAME itkFEMGenerateMeshTest
      COMMAND ITK-FEMTestDriver itkFEMGenerateMeshTest)
itk_add_test(NAME itkFEMLinearSystemWrapperDenseVNLTest
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperDenseVNLTest)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest1
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              0)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest2
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              1)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest3
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              2)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest4
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              3)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest5
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              4)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest6
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              5)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest7
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest
              6)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest8
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              0)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest9
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              1)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest10
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              2)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest11
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              3)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest12
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              4)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest13
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              5)
itk_add_test(NAME itkFEMLinearSystemWrapperItpackTest14
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperItpackTest2
              6)
itk_add_test(NAME itkFEMLinearSystemWrapperVNLTest
      COMMAND ITK-FEMTestDriver itkFEMLinearSystemWrapperVNLTest)
itk_add_test(NAME itkFEMPArrayTest
      COMMAND ITK-FEMTestDriver itkFEMPArrayTest)
