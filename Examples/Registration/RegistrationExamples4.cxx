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
  REGISTER_TEST(ImageRegistration12Test);
}

#undef main
#define main  ImageRegistration12Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate17
#include "ImageRegistration12.cxx"


