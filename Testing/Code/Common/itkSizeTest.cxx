/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSizeTest.cxx
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

#include "itkSize.h"

int itkSizeTest(int, char* [] )
{

  typedef itk::Size<4> SizeType;

  SizeType size1 = {{10, 20, 30, 40}};
  std::cout << "  SizeType size1 = {10, 20, 30, 40}; ";
  std::cout << size1 << std::endl;

  SizeType size2 = size1;
  std::cout << "  SizeType size2 = size1; ";
  std::cout << size2 << std::endl;

  SizeType size3 = size1 + size2;
  std::cout << "  SizeType size3 = size1 + size2; ";
  std::cout << size3 << std::endl;

  size2 += size1;
  std::cout << "  size2 += size1; ";
  std::cout << size2 << std::endl;

  size3 = size2 - size1;
  std::cout << "  size3 = size2 - size1; ";
  std::cout << size3 << std::endl;

  size2 -= size1;
  std::cout << "  size2 -= size1; ";
  std::cout << size2 << std::endl;

  size1.Fill(2);  size2.Fill(4); size3 = size1 * size2;
  std::cout << "  size1.Fill(2);  size2.Fill(4); size3 = size1 * size2; ";
  std::cout << size3 << std::endl;

  size3 *= size1;
  std::cout << "  size3 *= size1; ";
  std::cout << size3 << std::endl;

  size2 = size1;
  if (size1 == size2)
    {
    std::cout << "size1 == size2" << std::endl;
    }

  size1 *= size2;
  if (size1 != size2)
    {
    std::cout << "size1 != size2" << std::endl;
    }

  std::cout << "size1.GetElement(2):" << size1.GetElement(2) << std::endl;
  return EXIT_SUCCESS;

}
