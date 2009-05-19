/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelObjectLineTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

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
    return (EXIT_FAILURE);
    }

  LabelObjectLineType::LengthType length;
  length = labelLine->GetLength();
  if (length != 11)
    {
    std::cerr << "Set/Get length failed on null constructor." << length << std::endl;
    return (EXIT_FAILURE);
    }
  delete labelLine;

  labelLine = new LabelObjectLineType(currentIndex, 11);
  indexBack = labelLine->GetIndex();

  if ((indexBack[0] != 3) || (indexBack[1] != 7))
    {
    std::cerr << "Set/Get Index failed on arg constructor. " << indexBack << std::endl;
    return (EXIT_FAILURE);
    }

  if (labelLine->GetLength() != 11)
    {
    std::cerr << "Set/Get length failed on arg constructor." << length << std::endl;
    return (EXIT_FAILURE);
    }

  if (!labelLine->HasIndex(currentIndex))
    {
    std::cerr << "Has Index failed." << std::endl;
    return (EXIT_FAILURE);
    }

  if (labelLine->HasIndex(nextIndex))
    {
    std::cerr << "Has Index failed." << std::endl;
    return (EXIT_FAILURE);
    }

  if (labelLine->IsNextIndex(currentIndex))
    {
    std::cerr << "Is Next Index failed." << std::endl;
    return (EXIT_FAILURE);
    }

  if (!labelLine->IsNextIndex(nextIndex))
    {
    std::cerr << "Is Next Index failed." << std::endl;
    return (EXIT_FAILURE);
    }

  labelLine->Print(std::cout);
  delete labelLine;

  return (EXIT_SUCCESS);
}
