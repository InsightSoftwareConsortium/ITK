// this file defines the itkFEMTests for the test driver
// and all it expects is that you have a function called RegisterTests

#include <iostream>
#include "itkTestMain.h"

// Register all *.cxx files that do the testing with the REGISTER_TEST
// macro.
void RegisterTests()
{
  // REGISTER_TEST(itkFEMElementTest);
  REGISTER_TEST(itkFEMExceptionTest);
  // REGISTER_TEST(itkFEMGenerateMeshTest);
  REGISTER_TEST(itkFEMElement2DMembraneTest);
  REGISTER_TEST(itkFEMElement3DMembraneTest);
  REGISTER_TEST(itkFEMElement2DStrainTest);
}
