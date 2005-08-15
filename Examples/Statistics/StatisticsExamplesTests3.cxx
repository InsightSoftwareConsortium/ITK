// this file defines the Statistics examples tested for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST( ScalarImageKmeansClassifierTest );
REGISTER_TEST( ScalarImageMarkovRandomField1Test );
}

#undef main
#define main ScalarImageKmeansClassifierTest 
#include "ScalarImageKmeansClassifier.cxx"

#undef main
#define main ScalarImageMarkovRandomField1Test
#include "ScalarImageMarkovRandomField1.cxx"
