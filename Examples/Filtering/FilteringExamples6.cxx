// this file defines the FilterExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(DigitallyReconstructedRadiograph1Test);
REGISTER_TEST(FFTDirectInverseTest);
}

#undef main
#define main DigitallyReconstructedRadiograph1Test
#include "DigitallyReconstructedRadiograph1.cxx"


#undef main
#define main FFTDirectInverseTest
#include "FFTDirectInverse.cxx"

