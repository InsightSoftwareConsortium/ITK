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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


// this file defines the itkBasicFiltersTest for the test driver
// and all it expects is that you have a function called RegisterTests
#include <iostream>
#include "itkTestMain.h"


void RegisterTests()
{
  REGISTER_TEST(itkAnalyzeImageIOTest);
  REGISTER_TEST(itkAnalyzeImageIOTest2);
  REGISTER_TEST(itkAnalyzeImageIOBadHeader);
  REGISTER_TEST(itkAnalyzeImageIODirectionsTest);
  REGISTER_TEST(itkAnalyzeImageIORGBImageTest);
  REGISTER_TEST(itkPNGImageIOTest);
  REGISTER_TEST(itkPNGRGBAIOTest);
  REGISTER_TEST(itkGEImageIOTest);
  REGISTER_TEST(itkGEImageIOFactoryTest);
  REGISTER_TEST(itkNiftiImageIOTest);
  REGISTER_TEST(itkNiftiImageIOTest2);
  REGISTER_TEST(itkNiftiImageIOTest3);
  REGISTER_TEST(itkNiftiImageIOTest4);
  REGISTER_TEST(itkNiftiImageIOTest5);
  REGISTER_TEST(itkNiftiImageIOTest6);
  REGISTER_TEST(itkNiftiImageIOTest7);
  REGISTER_TEST(itkNiftiImageIOTest8);
  REGISTER_TEST(itkNiftiImageIOTest9);
  REGISTER_TEST(itkNiftiImageIOTest10);
  REGISTER_TEST(itkNiftiImageIOTest11);
}
