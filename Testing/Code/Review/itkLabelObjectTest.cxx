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
    return EXIT_FAILURE;
    }

  const int dim = 3;
  
  typedef itk::LabelObject< unsigned long, dim > LabelObjectType;
  typedef LabelObjectType::IndexType             IndexType;
  
  // testing AddLine(), GetNumberOfLines(), GetLineContainer() const and Optimize()
  
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
  // add this line with an itk::LabelLineObject to test the AddLine(LabelObjectLine) method
  lo->AddLine( LabelObjectType::LineType(idx, 1) );
  
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
    
  typedef LabelObjectType::LineContainerType::const_iterator IteratorType;

  IteratorType it2 = lo->GetLineContainer().begin();
  IteratorType it1 = ref->GetLineContainer().begin();
  IteratorType end = ref->GetLineContainer().end();

  lo->Print(std::cerr);

  while( it1 != end )
    {
    std::cout << it1->GetIndex() << "-" << it1->GetLength() << "    ";
    std::cout << it2->GetIndex() << "-" << it2->GetLength();
    std::cout << std::endl;
    if( it1->GetIndex() != it2->GetIndex() || it1->GetLength() != it2->GetLength() )
      {
      std::cerr << "Line mismatch." << std::endl;
      return EXIT_FAILURE;
      }
    it1++;
    it2++;
    }
    

  // testing AddIndex(), GetIndex() and HasIndex()
  
  lo = LabelObjectType::New();
  std::vector< IndexType > idxs;
  
  // one isolated pixel
  idx[0] = 0;
  idx[1] = 0;
  idx[2] = 0;
  lo->AddIndex( idx );
  idxs.push_back( idx );

  // and two consecutive ones
  idx[0] = 1;
  idx[1] = 2;
  idx[2] = 3;
  lo->AddIndex( idx );
  idxs.push_back( idx );

  idx[0] = 2;
  idx[1] = 2;
  idx[2] = 3;
  lo->AddIndex( idx );
  idxs.push_back( idx );
  
  // should produce 2 lines
  if( lo->GetNumberOfLines() != 2 )
    {
    std::cerr << "number of lines should be 2!" << std::endl;
    return EXIT_FAILURE;
    }
  
  // should produce 3 pixels
  if( lo->Size() != 3 )
    {
    std::cerr << "size should be 3!" << std::endl;
    return EXIT_FAILURE;
    }
  
  for( unsigned int i=0; i<lo->Size(); i++ )
    {
    if( lo->GetIndex( i ) != idxs[i] )
      {
      std::cerr << "Wrong index returned by GetIndex(" << i << "): " << lo->GetIndex( i ) << ". " << idxs[i] << " was expected." << std::endl;
      return EXIT_FAILURE;
      }
    if( !lo->HasIndex( idxs[i] ) )
      {
      std::cerr << "label object should have the index " << idxs[i] << "!" << std::endl;
      return EXIT_FAILURE;
      }
    }
  
  // test with an index not there
  idx[0] = 10;
  idx[1] = 10;
  idx[2] = 10;
  if( lo->HasIndex( idx ) )
    {
    std::cerr << "label object shouldn't have the index " << idx << "!" << std::endl;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;
}
