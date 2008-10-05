/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPriorityQueueTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <vnl/vnl_random.h>
#include "itkPriorityQueueContainer.h"

using namespace itk;

int itkPriorityQueueTest( int argc, char* argv[] )
{
  (void) argc;
  (void) argv;
  typedef MinPriorityQueueElementWrapper< int, double, int > PQElementType;
  typedef PriorityQueueContainer< PQElementType, PQElementType, double, int >
    PQType;
  PQType::Pointer priority_queue = PQType::New( );

  vnl_random random( 12 );
  int i( 0 ), element;
  double value = random.drand32( -1000., 1000. );

  std::cout <<"{" <<i <<", " <<value <<"}" <<std::endl;
  PQElementType to_be_erased( i, value );
  priority_queue->Push( to_be_erased );

  for( i = 1; i < 10; i++ )
  {
    value = random.drand32( -1000., 1000. );
    std::cout <<"{" <<i <<", " <<value <<"}" <<std::endl;
    priority_queue->Push( PQElementType( i, value ) );
  }

  i = 0;

  while( !priority_queue->Empty() )
    {
    element = priority_queue->Peek( ).m_Element;
    value = priority_queue->Peek( ).m_Priority;
    std::cout <<i++ <<" ** element: " <<element <<" priority: " <<value;
    std::cout <<" ** size: " <<priority_queue->Size( )<<std::endl;
    priority_queue->Pop( );
    }

  return EXIT_SUCCESS;
}
