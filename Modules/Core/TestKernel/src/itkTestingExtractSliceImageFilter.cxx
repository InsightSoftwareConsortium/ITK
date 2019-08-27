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
#include "itkTestingExtractSliceImageFilter.h"

namespace itk
{
namespace Testing
{
/** Define how to print enumerations */
std::ostream &
operator<<(std::ostream & out, const TestExtractSliceImageFilterCollapseStrategy value)
{
  const char * s = nullptr;
  switch (value)
  {
    case TestExtractSliceImageFilterCollapseStrategy::DIRECTIONCOLLAPSETOUNKOWN:
      s = "TestExtractSliceImageFilterCollapseStrategy::DIRECTIONCOLLAPSETOUNKOWN";
      break;
    case TestExtractSliceImageFilterCollapseStrategy::DIRECTIONCOLLAPSETOIDENTITY:
      s = "TestExtractSliceImageFilterCollapseStrategy::DIRECTIONCOLLAPSETOIDENTITY";
      break;
    case TestExtractSliceImageFilterCollapseStrategy::DIRECTIONCOLLAPSETOSUBMATRIX:
      s = "TestExtractSliceImageFilterCollapseStrategy::DIRECTIONCOLLAPSETOSUBMATRIX";
      break;
    case TestExtractSliceImageFilterCollapseStrategy::DIRECTIONCOLLAPSETOGUESS:
      s = "TestExtractSliceImageFilterCollapseStrategy::DIRECTIONCOLLAPSETOGUESS";
      break;
    default:
      s = "INVALID VALUE FOR TestExtractSliceImageFilterCollapseStrategy";
  }
  return out << s;
}
} // end namespace Testing
} // end namespace itk
