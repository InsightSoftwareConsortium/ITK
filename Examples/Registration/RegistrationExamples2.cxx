// this file defines the RegistrationExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
  REGISTER_TEST(DeformableRegistration1Test);
  REGISTER_TEST(DeformableRegistration2Test);
  REGISTER_TEST(ModelToImageRegistration1Test);
  REGISTER_TEST(MultiResImageRegistration1Test);
  REGISTER_TEST(MultiResImageRegistration2Test);
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
#define main  ModelToImageRegistration1Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate12
#include "ModelToImageRegistration1.cxx"

#undef main
#define main  MultiResImageRegistration1Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate13
#undef RegistrationInterfaceCommand
#define RegistrationInterfaceCommand RegistrationInterfaceCommand1
#include "MultiResImageRegistration1.cxx"

#undef main
#define main  MultiResImageRegistration2Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate14
#undef RegistrationInterfaceCommand
#define RegistrationInterfaceCommand RegistrationInterfaceCommand2
#include "MultiResImageRegistration2.cxx"
