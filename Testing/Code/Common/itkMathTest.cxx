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


// we are temporarily not going to use a ctest driver so just this
//compilation will fail when there is a problem with the Math header
//int itkMathTest( int, char *[] )
int main( int, char *[] )
{
  bool ok = true;


#ifndef ITK_LEGACY_REMOVE

  std::cout << itk::Math::RoundHalfIntegerToEven( 1.9f ) << std::endl;
  std::cout << itk::Math::RoundHalfIntegerToEven( 1.9 ) << std::endl;
  std::cout << itk::Math::RoundHalfIntegerUp( 1.9f ) << std::endl;
  std::cout << itk::Math::RoundHalfIntegerUp( 1.9 ) << std::endl;
  std::cout << itk::Math::Round( 1.9f ) << std::endl;
  std::cout << itk::Math::Round( 1.9 ) << std::endl;
  std::cout << itk::Math::Floor( 1.9f ) << std::endl;
  std::cout << itk::Math::Floor( 1.9 ) << std::endl;
  std::cout << itk::Math::Ceil( 1.9f ) << std::endl;
  std::cout << itk::Math::Ceil( 1.9 ) << std::endl;
#endif

  std::cout << itk::Math::RoundHalfIntegerToEven<int>( 1.9f ) << std::endl;
  std::cout << itk::Math::RoundHalfIntegerToEven<int>( 1.9 ) << std::endl;
  std::cout << itk::Math::RoundHalfIntegerUp<int>( 1.9f ) << std::endl;
  std::cout << itk::Math::RoundHalfIntegerUp<int>( 1.9 ) << std::endl;
  std::cout << itk::Math::Round<int>( 1.9f ) << std::endl;
  std::cout << itk::Math::Round<int>( 1.9 ) << std::endl;
  std::cout << itk::Math::Floor<int>( 1.9f ) << std::endl;
  std::cout << itk::Math::Floor<int>( 1.9 ) << std::endl;
  std::cout << itk::Math::Ceil<int>( 1.9f ) << std::endl;
  std::cout << itk::Math::Ceil<int>( 1.9 ) << std::endl;


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
