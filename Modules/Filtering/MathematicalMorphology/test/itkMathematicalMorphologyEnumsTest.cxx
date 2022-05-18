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

#include <set>
#include "itkMathematicalMorphologyEnums.h"


int
itkMathematicalMorphologyEnumsTest(int, char *[])
{

  // Test streaming enumeration for MathematicalMorphologyEnums::Algorithm elements
  const std::set<itk::MathematicalMorphologyEnums::Algorithm> allAlgorithm{
    itk::MathematicalMorphologyEnums::Algorithm::BASIC,
    itk::MathematicalMorphologyEnums::Algorithm::HISTO,
    itk::MathematicalMorphologyEnums::Algorithm::ANCHOR,
    itk::MathematicalMorphologyEnums::Algorithm::VHGW
  };
  for (const auto & ee : allAlgorithm)
  {
    std::cout << "STREAMED ENUM VALUE MathematicalMorphologyEnums::Algorithm: " << ee << std::endl;
  }


  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
