// this file defines the itkFEMTests for the test driver
// and all it expects is that you have a function called RegisterTests
#include "itkTestMain.h"


// Register all *.cxx files that do the testing with the REGISTER_TEST
// macro.
void RegisterTests()
{
REGISTER_TEST( itkFEMBar2DTest );
}
