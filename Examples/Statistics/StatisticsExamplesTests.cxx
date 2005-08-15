// this file defines the Statistics examples tested for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST( BayesianPluginClassifierTest );
REGISTER_TEST( EuclideanDistanceTest );
REGISTER_TEST( GaussianDensityFunctionTest );
}

#undef main
#define main BayesianPluginClassifierTest
#include "BayesianPluginClassifier.cxx"

#undef main
#define main EuclideanDistanceTest
#include "EuclideanDistance.cxx"

#undef main
#define main GaussianDensityFunctionTest
#include "GaussianDensityFunction.cxx"
