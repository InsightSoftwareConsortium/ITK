// this file defines the RegistrationExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "vnl/vnl_sample.h"
#include "itkTestMain.h" 


void RegisterTests()
{
  vnl_sample_reseed(8775070);
  REGISTER_TEST(ImageRegistration14Test);
  REGISTER_TEST(ImageRegistration15Test);
}

#undef main
#define main  ImageRegistration14Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate6
#include "ImageRegistration14.cxx"

#undef main
#define main  ImageRegistration15Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate7
#include "ImageRegistration15.cxx"

