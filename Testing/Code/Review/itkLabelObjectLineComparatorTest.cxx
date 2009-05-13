/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelObjectLineComparatorTest.cxx
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
    return (EXIT_FAILURE);
    }

  if (!lessThan(*low, *high))
    {
    std::cerr << "Failed, low<high returned false." << std::endl;
    return (EXIT_FAILURE);
    }

  if (lessThan(*low, *low))
    {
    std::cerr << "Failed, low<low returned true." << std::endl;
    return (EXIT_FAILURE);
    }

  if (!lessThan(*low, *lowlong))
    {
    std::cerr << "Failed, low<lowlong returned false." << std::endl;
    return (EXIT_FAILURE);
    }

  delete low;
  delete high;
  delete lowlong;

  return (EXIT_SUCCESS);
}
