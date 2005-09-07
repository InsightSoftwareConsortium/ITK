// this file defines the FilterExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(FFTDirectInverseTest);
#if defined(USE_FFTWF)
REGISTER_TEST(FFTDirectInverse2Test);
#endif
}
#undef main
#define main FFTDirectInverseTest
#include "FFTDirectInverse.cxx"

#if defined(USE_FFTWF)
#undef main
#define main FFTDirectInverse2Test
#include "FFTDirectInverse2.cxx"
#endif

