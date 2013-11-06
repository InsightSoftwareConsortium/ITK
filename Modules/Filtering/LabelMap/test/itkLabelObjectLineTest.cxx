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

int itkLabelObjectLineTest(int, char* [] )
{

  typedef itk::LabelObjectLine<2>            LabelObjectLineType;
  typedef itk::LabelObjectLine<2>::IndexType IndexType;
  IndexType currentIndex;
  currentIndex[0] = 3;
  currentIndex[1] = 7;

  IndexType nextIndex;
  nextIndex[0] = 14;
  nextIndex[1] = 7;

  LabelObjectLineType *labelLine = new LabelObjectLineType;
  labelLine->SetIndex(currentIndex);
  labelLine->SetLength(11);

  IndexType indexBack;
  indexBack = labelLine->GetIndex();

  if ((indexBack[0] != 3) || (indexBack[1] != 7))
    {
    std::cerr << "Set/Get Index failed on null constructor. " << indexBack << std::endl;
    delete labelLine;
    return (EXIT_FAILURE);
    }

  LabelObjectLineType::LengthType length;
  length = labelLine->GetLength();
  if (length != 11)
    {
    std::cerr << "Set/Get length failed on null constructor." << length << std::endl;
    delete labelLine;
    return (EXIT_FAILURE);
    }
  delete labelLine;

  labelLine = new LabelObjectLineType(currentIndex, 11);
  indexBack = labelLine->GetIndex();

  if ((indexBack[0] != 3) || (indexBack[1] != 7))
    {
    std::cerr << "Set/Get Index failed on arg constructor. " << indexBack << std::endl;
    delete labelLine;
    return (EXIT_FAILURE);
    }

  if (labelLine->GetLength() != 11)
    {
    std::cerr << "Set/Get length failed on arg constructor." << length << std::endl;
    delete labelLine;
    return (EXIT_FAILURE);
    }

  if (!labelLine->HasIndex(currentIndex))
    {
    std::cerr << "Has Index failed." << std::endl;
    delete labelLine;
    return (EXIT_FAILURE);
    }

  if (labelLine->HasIndex(nextIndex))
    {
    std::cerr << "Has Index failed." << std::endl;
    delete labelLine;
    return (EXIT_FAILURE);
    }

  if (labelLine->IsNextIndex(currentIndex))
    {
    std::cerr << "Is Next Index failed." << std::endl;
    delete labelLine;
    return (EXIT_FAILURE);
    }

  if (!labelLine->IsNextIndex(nextIndex))
    {
    std::cerr << "Is Next Index failed." << std::endl;
    delete labelLine;
    return (EXIT_FAILURE);
    }

  labelLine->Print(std::cout);
  delete labelLine;

  return (EXIT_SUCCESS);
}
