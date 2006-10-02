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

int itkFlatStructuringElementTest(int argn, char * argv[])
{

  if( argn < 4 )
    {
    std::cerr << "usage: kernelShape fileName radius type [lines|img]" << std::endl;
    std::cerr << "  type: 0 -> Box" << std::endl;
    std::cerr << "        1 -> Ball" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;
  
  typedef itk::FlatStructuringElement< Dimension > StructuringElementType;

  StructuringElementType::RadiusType Rad;

  Rad.Fill( atoi( argv[2] ) );
  
  int type = atoi( argv[3] );
  
  StructuringElementType K;
  
  if( type == 0 )
    {
    K = StructuringElementType::Box( Rad );
    }
  else if( type == 1 )
    {
    K = StructuringElementType::Ball( Rad );
    }
  else
    {
    return EXIT_FAILURE;
    }

  
  return EXIT_SUCCESS;
}

