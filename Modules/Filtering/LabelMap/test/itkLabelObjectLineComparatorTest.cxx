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

#include <iostream>
#include "itkLabelObjectLine.h"
#include "itkLabelObjectLineComparator.h"

int itkLabelObjectLineComparatorTest(int, char* [] )
{
  typedef itk::LabelObjectLine<2>                                      LabelObjectLineType;
  typedef itk::LabelObjectLine<2>::IndexType                           IndexType;
  typedef itk::Functor::LabelObjectLineComparator<LabelObjectLineType> ComparatorType;

  ComparatorType lessThan;

  IndexType lowIndex;
  lowIndex[0] = 3;
  lowIndex[1] = 7;

  IndexType highIndex;
  highIndex[0] = 14;
  highIndex[1] = 7;

  LabelObjectLineType *low = new LabelObjectLineType(lowIndex, 11);
  LabelObjectLineType *high = new LabelObjectLineType(highIndex, 11);
  LabelObjectLineType *lowlong = new LabelObjectLineType(lowIndex, 15);

  if (lessThan(*high, *low))
    {
    std::cerr << "Failed, high<low returned true." << std::endl;
    delete low;
    delete high;
    delete lowlong;
    return (EXIT_FAILURE);
    }

  if (!lessThan(*low, *high))
    {
    std::cerr << "Failed, low<high returned false." << std::endl;
    delete low;
    delete high;
    delete lowlong;
    return (EXIT_FAILURE);
    }

  if (lessThan(*low, *low))
    {
    std::cerr << "Failed, low<low returned true." << std::endl;
    delete low;
    delete high;
    delete lowlong;
    return (EXIT_FAILURE);
    }

  if (!lessThan(*low, *lowlong))
    {
    std::cerr << "Failed, low<lowlong returned false." << std::endl;
    delete low;
    delete high;
    delete lowlong;
    return (EXIT_FAILURE);
    }

  delete low;
  delete high;
  delete lowlong;

  return (EXIT_SUCCESS);
}
