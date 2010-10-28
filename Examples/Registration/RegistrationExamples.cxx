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
  REGISTER_TEST(ImageRegistration1Test);
  REGISTER_TEST(ImageRegistration2Test);
  REGISTER_TEST(ImageRegistration3Test);
  REGISTER_TEST(ImageRegistration4Test);
}

#undef main
#define main  ImageRegistration1Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate3
#include "ImageRegistration1.cxx"

#undef main
#define main  ImageRegistration2Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate4
#include "ImageRegistration2.cxx"

#undef main
#define main  ImageRegistration3Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate5
#include "ImageRegistration3.cxx"

#undef main
#define main  ImageRegistration4Test
#undef CommandIterationUpdate
#define CommandIterationUpdate CommandIterationUpdate6
#include "ImageRegistration4.cxx"
