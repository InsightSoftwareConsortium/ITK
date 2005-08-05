// this file defines the ImageExamples for the test driver
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(ImageAdaptor2Test);
  REGISTER_TEST(ImageAdaptor3Test);
  REGISTER_TEST(ImageAdaptor4Test);
}


#undef main
#define main ImageAdaptor2Test
#include "ImageAdaptor2.cxx"

#undef main
#define main ImageAdaptor3Test
#include "ImageAdaptor3.cxx"

#undef main
#define main ImageAdaptor4Test
#include "ImageAdaptor4.cxx"

