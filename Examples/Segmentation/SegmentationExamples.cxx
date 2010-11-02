/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
// this file defines the SegmentationExamples for the test driver
// and all it expects is that you have a function called RegisterTests
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include <iostream>
#include "itkTestMain.h"


void RegisterTests()
{
  REGISTER_TEST(CannySegmentationLevelSetImageFilterTest);
  REGISTER_TEST(ConfidenceConnectedTest);
  REGISTER_TEST(ConnectedThresholdImageFilterTest);
  REGISTER_TEST(FastMarchingImageFilterTest);
  REGISTER_TEST(GeodesicActiveContourImageFilterTest);
  REGISTER_TEST(GibbsPriorImageFilter1Test);
}

#undef main
#define main CannySegmentationLevelSetImageFilterTest
#include "CannySegmentationLevelSetImageFilter.cxx"

#undef main
#define main ConfidenceConnectedTest
#include "ConfidenceConnected.cxx"

#undef main
#define main ConnectedThresholdImageFilterTest
#include "ConnectedThresholdImageFilter.cxx"

#undef main
#define main FastMarchingImageFilterTest
#include "FastMarchingImageFilter.cxx"

#undef main
#define main GeodesicActiveContourImageFilterTest
#include "GeodesicActiveContourImageFilter.cxx"

#undef main
#define main GibbsPriorImageFilter1Test
#include "GibbsPriorImageFilter1.cxx"
