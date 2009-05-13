/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLabelObjectTest.cxx
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
#include "itkLabelObject.h"

int itkLabelObjectTest(int argc, char * argv[])
{

  if( argc != 1 )
    {
    std::cerr << "usage: " << argv[0] << "" << std::endl;
    // std::cerr << "  : " << std::endl;
    exit(1);
    }

  const int dim = 3;
  
  typedef itk::LabelObject< unsigned long, dim > LabelObjectType;
  typedef LabelObjectType::IndexType             IndexType;
  
  LabelObjectType::Pointer lo = LabelObjectType::New();
  
  IndexType idx;
  idx[0] = 1;
  idx[1] = 20;
  idx[2] = 30;
  lo->AddLine( idx, 10 );
  
  idx[0] = 5;
  lo->AddLine( idx, 10 );

  idx[0] = 5;
  idx[1] = 0;
  idx[2] = 0;
  lo->AddLine( idx, 10 );

  idx[0] = 5;
  idx[1] = 0;
  idx[2] = 1;
  lo->AddLine( idx, 10 );

  idx[0] = 5;
  idx[1] = 1;
  idx[2] = 0;
  lo->AddLine( idx, 10 );
  
  idx[0] = 0;
  idx[1] = 1;
  idx[2] = 1;
  lo->AddLine( idx, 10 );
  
  idx[0] = 10;
  idx[1] = 1;
  idx[2] = 1;
  lo->AddLine( idx, 1 );
  
  lo->Optimize();
  
  // the expected result after Optimize()
  LabelObjectType::Pointer ref = LabelObjectType::New();
  
  idx[0] = 5;
  idx[1] = 0;
  idx[2] = 0;
  ref->AddLine( idx, 10 );

  idx[0] = 5;
  idx[1] = 1;
  idx[2] = 0;
  ref->AddLine( idx, 10 );
  
  idx[0] = 5;
  idx[1] = 0;
  idx[2] = 1;
  ref->AddLine( idx, 10 );

  idx[0] = 0;
  idx[1] = 1;
  idx[2] = 1;
  ref->AddLine( idx, 11 );
  
  idx[0] = 1;
  idx[1] = 20;
  idx[2] = 30;
  ref->AddLine( idx, 14 );
  
  // compare the result
  if( lo->GetNumberOfLines() != ref->GetNumberOfLines() )
    {
    std::cerr << "number of lines is different!" << std::endl;
    return EXIT_FAILURE;
    }
    
  LabelObjectType::LineContainerType::const_iterator it2=lo->GetLineContainer().begin();
  for( LabelObjectType::LineContainerType::const_iterator it=ref->GetLineContainer().begin(); 
     it != ref->GetLineContainer().end();
     it++, it2++ )
    {
    std::cout << it->GetIndex() << "-" << it->GetLength() << "    ";
    std::cout << it2->GetIndex() << "-" << it2->GetLength();
    std::cout << std::endl;
    if( it->GetIndex() != it2->GetIndex() || it->GetLength() != it2->GetLength() )
      {
      std::cerr << "Line mismatch." << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}
