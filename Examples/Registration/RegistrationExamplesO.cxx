// this file defines the RegistrationExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

#include <iostream>
#include "vnl/vnl_sample.h"
#include "itkTestMain.h" 


void RegisterTests()
{
  vnl_sample_reseed(8775070);
  REGISTER_TEST(ImageRegistration1oTest);
  REGISTER_TEST(ImageRegistration2oTest);
  REGISTER_TEST(ImageRegistration3oTest);
  REGISTER_TEST(ImageRegistration4oTest);
}


#undef main
#define main  ImageRegistration1oTest
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate3
#include "ImageRegistration1o.cxx"

#undef main
#define main  ImageRegistration2oTest
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate4
#include "ImageRegistration2o.cxx"

#undef main
#define main  ImageRegistration3oTest
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate5
#include "ImageRegistration3o.cxx"

#undef main
#define main  ImageRegistration4oTest
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate6
#include "ImageRegistration4o.cxx"
