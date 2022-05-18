/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkESMDemonsRegistrationFunction.h"

int
itkESMDemonsRegistrationFunctionTest(int, char *[])
{
  // Test streaming enumeration for ESMDemonsRegistrationFunctionEnums::Gradient elements
  const std::set<itk::ESMDemonsRegistrationFunctionEnums::Gradient> allGradient{
    itk::ESMDemonsRegistrationFunctionEnums::Gradient::Symmetric,
    itk::ESMDemonsRegistrationFunctionEnums::Gradient::Fixed,
    itk::ESMDemonsRegistrationFunctionEnums::Gradient::WarpedMoving,
    itk::ESMDemonsRegistrationFunctionEnums::Gradient::MappedMoving
  };
  for (const auto & ee : allGradient)
  {
    std::cout << "STREAMED ENUM VALUE ESMDemonsRegistrationFunctionEnums::Gradient: " << ee << std::endl;
  }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
}
