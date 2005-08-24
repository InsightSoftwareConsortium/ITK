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
  REGISTER_TEST(ImageRegistration10Test);
  REGISTER_TEST(ImageRegistration14Test);
  REGISTER_TEST(ImageRegistration15Test);
  REGISTER_TEST(ImageRegistration16Test);
  REGISTER_TEST(ImageRegistration17Test);
}

#undef main
#define main  ImageRegistration10Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate6
#include "ImageRegistration10.cxx"

#undef main
#define main  ImageRegistration14Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate7
#include "ImageRegistration14.cxx"

#undef main
#define main  ImageRegistration15Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate8
#include "ImageRegistration15.cxx"

#undef main
#define main  ImageRegistration16Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate9
#include "ImageRegistration16.cxx"

#undef main
#define main  ImageRegistration17Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate10
#include "ImageRegistration17.cxx"

