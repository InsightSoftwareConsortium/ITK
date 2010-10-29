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
// this file defines the itkFEMTests for the test driver
// and all it expects is that you have a function called RegisterTests

// disable debug warnings in MS compiler
#ifdef _MSC_VER
#pragma warning(disable: 4786)
#endif

#include <iostream>
#include "itkTestMain.h"


// Register all *.cxx files that do the testing with the REGISTER_TEST
// macro.
void RegisterTests()
{
  REGISTER_TEST(itkFEMElementTest);
  REGISTER_TEST(itkFEMExceptionTest);
  REGISTER_TEST(itkFEMGenerateMeshTest);
  REGISTER_TEST(itkFEMElement2DMembraneTest);
  REGISTER_TEST(itkFEMElement3DMembraneTest);
  REGISTER_TEST(itkFEMElement2DStrainTest);
}
