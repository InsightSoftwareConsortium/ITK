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
// this file defines the ImageExamples for the test driver
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h"


void RegisterTests()
{
  REGISTER_TEST(Image1Test);
  REGISTER_TEST(Image2Test);
  REGISTER_TEST(Image3Test);
  REGISTER_TEST(Image4Test);
  REGISTER_TEST(Image5Test);
}

#undef main
#define main Image1Test
#include "Image1.cxx"

#undef main
#define main Image2Test
#include "Image2.cxx"

#undef main
#define main Image3Test
#include "Image3.cxx"

#undef main
#define main Image4Test
#include "Image4.cxx"

#undef main
#define main Image5Test
#include "Image5.cxx"
