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
  REGISTER_TEST(DeformableRegistration1Test);
  REGISTER_TEST(DeformableRegistration2Test);
  REGISTER_TEST(DeformableRegistration3Test);
  REGISTER_TEST(DeformableRegistration5Test);
}

#undef main
#define main  DeformableRegistration1Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate1
#include "DeformableRegistration1.cxx"

#undef main
#define main  DeformableRegistration2Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate2
#include "DeformableRegistration2.cxx"

#undef main
#define main  DeformableRegistration3Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate3
#include "DeformableRegistration3.cxx"

#undef main
#define main  DeformableRegistration5Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate4
#include "DeformableRegistration5.cxx"

