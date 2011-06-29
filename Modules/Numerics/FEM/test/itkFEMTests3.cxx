// this file defines the itkFEMTests for the test driver
// and all it expects is that you have a function called RegisterTests

#include <iostream>
#include "itkTestMain.h"

// Register all *.cxx files that do the testing with the REGISTER_TEST
// macro.
void RegisterTests()
{
  REGISTER_TEST(itkFEMElement2DC1BeamTest);
  REGISTER_TEST(itkFEMElement2DC0LinearLineStressTest);
  REGISTER_TEST(itkFEMElement2DC0LinearQuadrilateralStrainItpackTest);
  REGISTER_TEST(itkFEMElement2DC0LinearTriangleStressTest);
  REGISTER_TEST(itkFEMElement2DC0LinearTriangleStrainTest);
  REGISTER_TEST(itkFEMElement2DC0LinearTriangleMembraneTest);
  REGISTER_TEST(itkFEMElement2DC0LinearQuadrilateralStressTest);
  REGISTER_TEST(itkFEMElement2DC0LinearQuadrilateralStrainTest);
  REGISTER_TEST(itkFEMElement2DC0LinearQuadrilateralMembraneTest);
  REGISTER_TEST(itkFEMElement2DC0QuadraticTriangleStrainTest);
  REGISTER_TEST(itkFEMElement2DC0QuadraticTriangleStressTest);
  REGISTER_TEST(itkFEMElement3DC0LinearHexahedronStrainTest);
  REGISTER_TEST(itkFEMElement3DC0LinearHexahedronMembraneTest);
  REGISTER_TEST(itkFEMElement3DC0LinearTetrahedronStrainTest);
  REGISTER_TEST(itkFEMElement3DC0LinearTetrahedronMembraneTest);
  REGISTER_TEST(itkFEMLoadBCMFCTest);
  REGISTER_TEST(itkFEMLoadBCMFCTestUser);
  REGISTER_TEST(itkFEMLoadEdgeTest);
  REGISTER_TEST(itkFEMLoadGravConstTest);
  REGISTER_TEST(itkFEMLandmarkLoadImplementationTest);
  REGISTER_TEST(itkFEMRegistrationFilterTest);
//  REGISTER_TEST(itkFEMSolverTest2D);
  REGISTER_TEST(itkFEMSolverTest3D);
  REGISTER_TEST(itkImageToRectilinearFEMObjectFilter2DTest);
  REGISTER_TEST(itkImageToRectilinearFEMObjectFilter3DTest);
  REGISTER_TEST(itkFEMElement2DTest);
  REGISTER_TEST(itkFEMElement3DTest);
}
