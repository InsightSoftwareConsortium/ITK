#include <vnl/vnl_random.h>
#include "itkPriorityQueueContainer.h"

using namespace itk;

int itkPriorityQueueTest( int argc, char** argv )
{
  typedef MinPriorityQueueElementWrapper< int, double, int > PQElementType;
  typedef PriorityQueueContainer< PQElementType, PQElementType, double, int >
    PQType;
  PQType::Pointer priority_queue = PQType::New( );

  vnl_random random( 12 );
  int i( 0 ), element( 0 );
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
