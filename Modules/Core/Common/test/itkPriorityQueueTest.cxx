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

#include "itkMath.h"
#include "itkPriorityQueueContainer.h"

int itkPriorityQueueTest( int, char * [] )
{
  typedef itk::IdentifierType    ElementIdentifier;

  typedef itk::MinPriorityQueueElementWrapper< size_t, double, ElementIdentifier > MinPQElementType;
  typedef itk::MaxPriorityQueueElementWrapper< size_t, double, ElementIdentifier > MaxPQElementType;

  typedef itk::PriorityQueueContainer<
    MinPQElementType, MinPQElementType, double, ElementIdentifier > MinPQType;
  MinPQType::Pointer min_priority_queue = MinPQType::New( );

  std::cout << min_priority_queue->GetNameOfClass() << std::endl;

  typedef itk::PriorityQueueContainer<
    MaxPQElementType, MaxPQElementType, double, ElementIdentifier > MaxPQType;
  MaxPQType::Pointer max_priority_queue = MaxPQType::New( );

  std::list< double > sequence;
  sequence.push_back( -0.1 );
  sequence.push_back( 0.1 );
  sequence.push_back( 0.4 );
  sequence.push_back( -0.2 );
  sequence.push_back( -0.3 );
  sequence.push_back( 0.3 );
  sequence.push_back( 0.2 );
  sequence.push_back( 0.5 );
  sequence.push_back( -0.6 );
  sequence.push_back( -0.5 );
  sequence.push_back( 0.6 );
  sequence.push_back( 1. );
  sequence.push_back( -1. );

  std::list< double >::const_iterator it = sequence.begin();
  size_t i = 0;
  for(; it != sequence.end(); ++it, i++ )
    {
    min_priority_queue->Push( MinPQElementType( i, *it ) );
    max_priority_queue->Push( MaxPQElementType( i, *it ) );
    }

  sequence.sort();
  it = sequence.begin();
  i  = sequence.size();

  std::cout <<"Min Priority Queue   ";
  while( !min_priority_queue->Empty() )
    {
    if( itk::Math::NotAlmostEquals( min_priority_queue->Peek().m_Priority, *it ) )
      {
      std::cout <<min_priority_queue->Peek().m_Priority <<" " <<*it <<std::endl;
      return EXIT_FAILURE;
      }
    if( min_priority_queue->Size() != i )
      {
      std::cout <<"Size " <<min_priority_queue->Size() <<" " <<i <<std::endl;
      return EXIT_FAILURE;
      }
    min_priority_queue->Pop();
    ++it;
    --i;
    }
  std::cout <<"OK" <<std::endl;

  std::cout <<"Max Priority Queue   ";
  while( !max_priority_queue->Empty() )
    {
    if( itk::Math::NotAlmostEquals( max_priority_queue->Peek().m_Priority, sequence.back() ) )
      {
      std::cout <<max_priority_queue->Peek().m_Priority <<" " <<sequence.back() <<std::endl;
      return EXIT_FAILURE;
      }
    if( max_priority_queue->Size() != sequence.size() )
      {
      std::cout <<"Size " <<max_priority_queue->Size() <<" " <<sequence.size() <<std::endl;
      return EXIT_FAILURE;
      }
    max_priority_queue->Pop();
    if (max_priority_queue->Empty())
      {
      break;
      }
    sequence.pop_back();
    }
  std::cout <<"OK" <<std::endl;

  return EXIT_SUCCESS;
}
