// this file defines the FilterExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h" 


void RegisterTests()
{
REGISTER_TEST(ResampleVolumesToBeIsotropicTest);
REGISTER_TEST(ScaleSpaceGenerator2DTest);
REGISTER_TEST(SubsampleVolumeTest);
}

#undef main
#define main ResampleVolumesToBeIsotropicTest
#include "ResampleVolumesToBeIsotropic.cxx"

#undef main
#define main ScaleSpaceGenerator2DTest
#include "ScaleSpaceGenerator2D.cxx"

#undef main
#define main SubsampleVolumeTest
#include "SubsampleVolume.cxx"

