// this file defines the ImageExamples for the test driver
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(ImageAdaptor1Test);
}

#undef main
#define main ImageAdaptor1Test
#include "ImageAdaptor1.cxx"


