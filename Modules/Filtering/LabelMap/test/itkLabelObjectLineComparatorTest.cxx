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

#include <iostream>
#include "itkLabelObjectLine.h"
#include "itkLabelObjectLineComparator.h"

int
itkLabelObjectLineComparatorTest(int, char *[])
{
  using LabelObjectLineType = itk::LabelObjectLine<2>;
  using IndexType = itk::LabelObjectLine<2>::IndexType;
  using ComparatorType = itk::Functor::LabelObjectLineComparator<LabelObjectLineType>;

  constexpr ComparatorType lessThan;

  IndexType lowIndex;
  lowIndex[0] = 3;
  lowIndex[1] = 7;

  IndexType highIndex;
  highIndex[0] = 14;
  highIndex[1] = 7;

  const LabelObjectLineType low(lowIndex, 11);
  const LabelObjectLineType high(highIndex, 11);
  const LabelObjectLineType lowlong(lowIndex, 15);

  if (lessThan(high, low))
  {
    std::cerr << "Failed, high<low returned true." << std::endl;
    return (EXIT_FAILURE);
  }

  if (!lessThan(low, high))
  {
    std::cerr << "Failed, low<high returned false." << std::endl;
    return (EXIT_FAILURE);
  }

  if (lessThan(low, low))
  {
    std::cerr << "Failed, low<low returned true." << std::endl;
    return (EXIT_FAILURE);
  }

  if (!lessThan(low, lowlong))
  {
    std::cerr << "Failed, low<lowlong returned false." << std::endl;
    return (EXIT_FAILURE);
  }

  return (EXIT_SUCCESS);
}
