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
// this file defines the Iterator Examples for the test driver
// and all it expects is that you have a function called RegisterTests
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif
#include <iostream>
#include "itkTestMain.h"


void RegisterTests()
{
  REGISTER_TEST(ImageLinearIteratorWithIndexTest);
  REGISTER_TEST(ImageRandomConstIteratorWithIndexTest);
  REGISTER_TEST(ImageRegionIteratorTest);
  REGISTER_TEST(ImageRegionIteratorWithIndexTest);
  REGISTER_TEST(ImageSliceIteratorWithIndexTest);
}

#undef main
#define main ImageLinearIteratorWithIndexTest
#include "ImageLinearIteratorWithIndex.cxx"

#undef main
#define main ImageRandomConstIteratorWithIndexTest
#include "ImageRandomConstIteratorWithIndex.cxx"

#undef main
#define main ImageRegionIteratorTest
#include "ImageRegionIterator.cxx"

#undef main
#define main ImageRegionIteratorWithIndexTest
#include "ImageRegionIteratorWithIndex.cxx"

#undef main
#define main ImageSliceIteratorWithIndexTest
#include "ImageSliceIteratorWithIndex.cxx"
