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

int itkDataTypeTest(int, char* [] )
{
  int status = 0;
  itk::Vector<int, 4> v;

  v[0] = 1; v[1] = 2; v[2] = 3; v[3] = 4;
  std::cout << "Vector value = ";
  for (unsigned int i=0; i < v.GetVectorDimension(); i++)
    {
    if (v[i] != static_cast<int>(i + 1))
      {
      std::cout << std::endl << "ERROR: v[" << i << "] = " << v[i] << " but should = " << i + 1 << std::endl;
      status++;
      }
    std::cout << v[i];
    if (i < v.GetVectorDimension() - 1)
      {
      std::cout << ", ";
      }
    }
  std::cout << std::endl;
  
  return status;
}
