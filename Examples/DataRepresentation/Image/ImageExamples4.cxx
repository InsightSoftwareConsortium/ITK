// this file defines the ImageExamples for the test driver
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(RGBImageTest);
  REGISTER_TEST(VectorImageTest);
}
#undef main
#define main RGBImageTest
#include "RGBImage.cxx"

#undef main
#define main VectorImageTest
#include "VectorImage.cxx"

