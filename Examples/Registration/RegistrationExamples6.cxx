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
  REGISTER_TEST(DeformableRegistration4Test);
  REGISTER_TEST(BSplineWarping1Test);
  vnl_sample_reseed(8775070);
  REGISTER_TEST(DeformableRegistration6Test);
}

#undef main
#define main  DeformableRegistration4Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate15
#include "DeformableRegistration4.cxx"

#undef main
#define main  BSplineWarping1Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate16
#include "BSplineWarping1.cxx"

#undef main
#define main  DeformableRegistration6Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate17
#include "DeformableRegistration6.cxx"


