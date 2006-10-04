/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFlatStructuringElementTest.cxx
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

#include "itkFlatStructuringElement.h"

int itkFlatStructuringElementTest(int, char *[])
{
  const unsigned int Dimension = 3;
  
  typedef itk::FlatStructuringElement< Dimension > StructuringElementType;

  StructuringElementType::RadiusType Rad;

  Rad.Fill( 3 );
  
  StructuringElementType K;
  K = StructuringElementType::Box( Rad );
  K.Print(std::cout);
  K = StructuringElementType::Ball( Rad );
  K.Print(std::cout);
  
  return EXIT_SUCCESS;
}

