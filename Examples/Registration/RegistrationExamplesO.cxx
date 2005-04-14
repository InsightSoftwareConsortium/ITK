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
  REGISTER_TEST(ImageRegistration1oTest);
  REGISTER_TEST(ImageRegistration2oTest);
  REGISTER_TEST(ImageRegistration3oTest);
  REGISTER_TEST(ImageRegistration4oTest);
  REGISTER_TEST(ImageRegistration5oTest);
  REGISTER_TEST(ImageRegistration6oTest);
  REGISTER_TEST(ImageRegistration7oTest);
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

#undef main
#define main  ImageRegistration5oTest
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate7
#include "ImageRegistration5o.cxx"

#undef main
#define main  ImageRegistration6oTest
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate8
#include "ImageRegistration6o.cxx"

#undef main
#define main  ImageRegistration7oTest
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate9
#include "ImageRegistration7o.cxx"
