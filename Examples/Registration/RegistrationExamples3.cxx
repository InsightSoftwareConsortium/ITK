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
  REGISTER_TEST(ImageRegistration8Test);
  REGISTER_TEST(ImageRegistration9Test);
  REGISTER_TEST(IterativeClosestPoint1Test);
  REGISTER_TEST(IterativeClosestPoint2Test);
  REGISTER_TEST(IterativeClosestPoint3Test);
}

#undef main
#define main  ImageRegistration8Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate10
#include "ImageRegistration8.cxx"

#undef main
#define main  ImageRegistration9Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate11
#include "ImageRegistration9.cxx"

#undef main
#define main  IterativeClosestPoint1Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate12
#include "IterativeClosestPoint1.cxx"

#undef main
#define main  IterativeClosestPoint2Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate13
#include "IterativeClosestPoint2.cxx"

#undef main
#define main  IterativeClosestPoint3Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate14
#include "IterativeClosestPoint3.cxx"





