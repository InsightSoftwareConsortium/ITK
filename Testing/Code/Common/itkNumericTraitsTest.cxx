/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsTest.cxx
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
#include "itkNumericTraits.h"


// using namespace itk;
template<class T> CheckTraits(char *name, T var)
{
  std::cout << "itk::NumericTraits<" << name << ">" << std::endl;
  std::cout << "\tmin(): " << static_cast<itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::min()) << std::endl;
  std::cout << "\tmax(): " << static_cast<itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::max()) << std::endl;
  std::cout << "\tNonpositiveMin(): " << static_cast<itk::NumericTraits<T>::PrintType>(itk::NumericTraits<T>::NonpositiveMin()) << std::endl;
}

int itkNumericTraitsTest(int, char* [] )
{
  CheckTraits("char", char (0));
  CheckTraits("unsigned char", unsigned char(0));
  CheckTraits("short", short (0));
  CheckTraits("unsigned short", unsigned short(0));
  CheckTraits("int", int(0));
  CheckTraits("unsigned int", unsigned int(0));
  CheckTraits("long", long(0));
  CheckTraits("unsigned long", unsigned long(0));
  CheckTraits("float", float(0));
  CheckTraits("double", double(0));

  return 0;
}
