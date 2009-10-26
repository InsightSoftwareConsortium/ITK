/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMathTest.cxx
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

#include "itkMath.h"
#include <iostream>
#include <cstdlib>

int main( int, char *[] )
{
  bool ok = true;
  
  std::cout << "e: " << itk::Math::e << std::endl;
  std::cout << "log2e: " << itk::Math::log2e << std::endl;
  std::cout << "log10e: " << itk::Math::log10e << std::endl;
  std::cout << "ln2: " << itk::Math::ln2 << std::endl;
  std::cout << "pi: " << itk::Math::pi << std::endl;
  std::cout << "pi_over_2: " << itk::Math::pi_over_2 << std::endl;
  std::cout << "two_over_pi: " << itk::Math::two_over_pi << std::endl;
  std::cout << "two_over_sqrtpi: " << itk::Math::two_over_sqrtpi << std::endl;
  std::cout << "one_over_sqrt2pi: " << itk::Math::one_over_sqrt2pi << std::endl;
  std::cout << "sqrt2: " << itk::Math::sqrt2 << std::endl;
  std::cout << "sqrt1_2: " << itk::Math::sqrt1_2 << std::endl;

  
  std::cout << itk::Math::e*itk::Math::log2e*
    itk::Math::log10e*itk::Math::ln2*
    itk::Math::pi*itk::Math::pi_over_2*
    itk::Math::pi_over_4*itk::Math::one_over_pi*
    itk::Math::two_over_pi*itk::Math::two_over_sqrtpi*
    itk::Math::one_over_sqrt2pi*itk::Math::sqrt2*
    itk::Math::sqrt1_2 << std::endl;

  if (!ok)
    {
    return EXIT_FAILURE;
    }
  else
    {
    std::cout<<"Test passed"<<std::endl;
    return EXIT_SUCCESS;
    }
  
}
