/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDataTypeTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>
#include "itkVector.h"


// using namespace itk;

int main()
{
  itk::Vector<int, 4> v;

  v = 1,2,3,4;
  std::cout << "Vector value = ";
  for (unsigned int i=0; i < v.GetVectorDimension(); i++)
    {
    std::cout << v[i];
    if (i < v.GetVectorDimension() - 1)
      {
      std::cout << ", ";
      }
    }
  std::cout << std::endl;
  
  return 0;
}
