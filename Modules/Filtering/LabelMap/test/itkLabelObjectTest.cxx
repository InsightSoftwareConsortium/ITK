/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

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

  typedef LabelObjectType::ConstLineIterator IteratorType;

  IteratorType it1;
  it1 = IteratorType( ref );
  IteratorType it2( lo );

  lo->Print(std::cerr);

  while( ! it1.IsAtEnd() )
    {
    std::cout << it1.GetLine().GetIndex() << "-" << it1.GetLine().GetLength() << "    ";
    std::cout << it2.GetLine().GetIndex() << "-" << it2.GetLine().GetLength();
    std::cout << std::endl;
    if( it1.GetLine().GetIndex() != it2.GetLine().GetIndex() || it1.GetLine().GetLength() != it2.GetLine().GetLength() )
      {
      std::cerr << "Line mismatch." << std::endl;
      return EXIT_FAILURE;
      }
    // test both versions of the iterator
    it1++;
    ++it2;
    }

  if( ! it2.IsAtEnd() )
    {
    std::cerr << "it2 not at end" << std::endl;
    return EXIT_FAILURE;
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
