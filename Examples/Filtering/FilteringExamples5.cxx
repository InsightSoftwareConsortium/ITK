// this file defines the FilterExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(OtsuThresholdImageFilterTest);
REGISTER_TEST(OtsuMultipleThresholdImageFilterTest);
REGISTER_TEST(DiffusionTensor3DReconstructionImageFilterTest1);
REGISTER_TEST(DiffusionTensor3DReconstructionImageFilterTest2);
}

#undef main
#define main OtsuThresholdImageFilterTest
#include "OtsuThresholdImageFilter.cxx"

#undef main
#define main OtsuMultipleThresholdImageFilterTest
#include "OtsuMultipleThresholdImageFilter.cxx"

#undef main
#define main DiffusionTensor3DReconstructionImageFilterTest1 
#include "DiffusionTensor3DReconstructionImageFilter.cxx"

#undef main
#define main DiffusionTensor3DReconstructionImageFilterTest2
#include "DiffusionTensor3DReconstructionImageFilter.cxx"
