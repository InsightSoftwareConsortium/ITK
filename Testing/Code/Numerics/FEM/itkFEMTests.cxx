// this file defines the itkFEMTests for the test driver
// and all it expects is that you have a function called RegisterTests

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include <iostream>
#include "itkTestMain.h"


// Register all *.cxx files that do the testing with the REGISTER_TEST
// macro.
void RegisterTests()
{
  //REGISTER_TEST(itkFEMElementTestMenu);
  REGISTER_TEST(itkFEMElementTest);
  REGISTER_TEST(itkFEMExceptionTest);
  REGISTER_TEST(itkFEMGenerateMeshTest);
  REGISTER_TEST(itkFEMElement2DMembraneTest);
  REGISTER_TEST(itkFEMElement3DMembraneTest);
  REGISTER_TEST(itkFEMElement2DStrainTest);
  REGISTER_TEST(itkFEMElement2DQuadraticTriangularTest);
  REGISTER_TEST(itkFEMLinearSystemWrapperItpackTest);
  REGISTER_TEST(itkFEMLinearSystemWrapperVNLTest);
  REGISTER_TEST(itkFEMLinearSystemWrapperDenseVNLTest);
  REGISTER_TEST(itkFEMPArrayTest);
  //REGISTER_TEST( itkFEMBar2DTest );
}
