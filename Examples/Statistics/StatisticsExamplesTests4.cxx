// this file defines the Statistics examples tested for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST( BayesianClassifierInitializerTest );
REGISTER_TEST( BayesianClassifierTest );
}

#undef main
#define main BayesianClassifierInitializerTest
#include "BayesianClassifierInitializer.cxx"

#undef main
#define main BayesianClassifierTest
#include "BayesianClassifier.cxx"
