#ifndef __itkQuadEdgeMeshEdgeMergeDecimationFilter_txx
#define __itkQuadEdgeMeshEdgeMergeDecimationFilter_txx

#include "itkQuadEdgeMeshEdgeMergeDecimationFilter.h"

namespace itk
{

template< class TInput, class TOutput, class TCriterion >
QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
QuadEdgeMeshEdgeMergeDecimationFilter() : Superclass(),
  m_Verbose( false ), m_Relocate( true ), m_CheckOrientation( false )
{
  m_JoinVertexFunction = OperatorType::New();
  m_PriorityQueue = PriorityQueueType::New();
}

template< class TInput, class TOutput, class TCriterion >
QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
~QuadEdgeMeshEdgeMergeDecimationFilter()
{
}

template< class TInput, class TOutput, class TCriterion >
void QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
FillPriorityQueue()
{
  OutputMeshPointer output = this->GetOutput();
  m_JoinVertexFunction->SetInput( output );

  OutputCellsContainerIterator it = output->GetEdgeCells()->Begin();
  OutputCellsContainerIterator end = output->GetEdgeCells()->End();

  OutputEdgeCellType* edge;

  for ( ; it != end; ++it )
    {
    if ( ( edge = dynamic_cast< OutputEdgeCellType* >( it.Value( ) ) ) )
      {
      PushElement( edge->GetQEGeom( ) );
      }
    }
}

template< class TInput, class TOutput, class TCriterion >
void QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
PushElement( OutputQEType* iEdge )
{
  OutputPointIdentifier id_org = iEdge->GetOrigin();
  OutputPointIdentifier id_dest = iEdge->GetDestination();

  OutputQEType* temp = ( id_org < id_dest ) ? iEdge : iEdge->GetSym();
  MeasureType measure = MeasureEdge( temp );

  PriorityQueueItemType* qi = new PriorityQueueItemType( temp,
    PriorityType( false, measure ) );

  m_QueueMapper[ temp ] = qi;
  m_PriorityQueue->Push( qi );
}

template< class TInput, class TOutput, class TCriterion >
bool QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
IsEdgeOKForPopping( OutputQEType* iEdge )
{
  if ( iEdge == 0 )
    {
    if( m_Verbose )
      std::cout <<"** iEdge == 0 ** " <<this->m_Iteration <<std::endl;
    return false;
    }

  OutputPointIdentifier id_org = iEdge->GetOrigin();
  if ( id_org == iEdge->m_NoPoint )
    {
    if( m_Verbose )
      std::cout <<"** id_org == iEdge->m_NoPoint ** "
        <<this->m_Iteration <<std::endl;
    return false;
    }

  OutputMeshPointer output = this->GetOutput();
  if ( output->FindEdge( id_org ) == 0 )
    {
    if( m_Verbose )
      std::cout <<"** output->FindEdge( id_org ) == 0 ** "
        <<this->m_Iteration <<std::endl;
    return false;
    }
  if ( iEdge->GetSym() == 0 )
    {
    if( m_Verbose )
      std::cout <<"** iEdge->GetSym() == 0 ** "
        <<this->m_Iteration <<std::endl;
    return false;
    }

  OutputPointIdentifier id_dest = iEdge->GetDestination();
  if ( id_dest == iEdge->m_NoPoint )
    {
    if( m_Verbose )
      std::cout <<"** id_dest == iEdge->m_NoPoint ** "
        <<this->m_Iteration <<std::endl;
    return false;
    }
  if ( output->FindEdge( id_dest ) == 0 )
    {
    if( m_Verbose )
      std::cout <<"** output->FindEdge( id_dest ) == 0 ** "
        <<this->m_Iteration <<std::endl;
    return false;
    }
  if ( output->FindEdge( id_org, id_dest ) == 0 )
    {
    if( m_Verbose )
      std::cout <<"** output->FindEdge( id_org, id_dest ) == 0 ** "
        <<this->m_Iteration <<std::endl;
    return false;
    }

  return true;
}

template< class TInput, class TOutput, class TCriterion >
void QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
Extract()
{
  OutputMeshPointer output = this->GetOutput();

  do
    {
    m_Element = m_PriorityQueue->Peek( )->m_Element;
    m_Priority = m_PriorityQueue->Peek( )->m_Priority;

    m_PriorityQueue->Pop();
    m_QueueMapper.erase( m_Element );
    } while ( !IsEdgeOKForPopping( m_Element ) );
}

template< class TInput, class TOutput, class TCriterion >
void QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
DeleteElement( OutputQEType* iEdge )
{
  if ( iEdge ) // this test can be removed
    {
    OutputQEType* temp = ( iEdge->GetOrigin() < iEdge->GetDestination() ) ?
      iEdge : iEdge->GetSym();

    QueueMapIterator map_it = m_QueueMapper.find( temp );
    if ( map_it != m_QueueMapper.end() )
      {
      if( !map_it->second->m_Priority.first )
        {
        m_PriorityQueue->Delete( map_it->second );
        m_QueueMapper.erase( map_it );
        }
      }
    }
}

template< class TInput, class TOutput, class TCriterion >
void QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
PushOrUpdateElement( OutputQEType* iEdge )
{
  OutputQEType* temp = iEdge;

  if ( temp->GetOrigin() > temp->GetDestination() )
    temp = temp->GetSym();

  QueueMapIterator map_it = m_QueueMapper.find( temp );

  MeasureType measure = MeasureEdge( temp );
  if ( map_it != m_QueueMapper.end() )
    {
    if( !map_it->second->m_Priority.first )
      {
      map_it->second->m_Priority.second = measure;
      m_PriorityQueue->Update( map_it->second );
      }
    }
  else
    {
    PriorityQueueItemType* qi = new PriorityQueueItemType( temp,
      PriorityType( false, measure ) );
    m_QueueMapper[ temp ] = qi;
    m_PriorityQueue->Push( qi );
    }
}


template< class TInput, class TOutput, class TCriterion >
void QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
JoinVertexFailed( )
{
  typename OperatorType::EdgeStatusType
    status = m_JoinVertexFunction->GetEdgeStatus();
  switch( status )
  {
  default:
  case OperatorType::EDGE_NULL:
  case OperatorType::MESH_NULL:
  case OperatorType::FACE_ISOLATED:
    break;
  case OperatorType::EDGE_ISOLATED:
    if( m_Verbose )
      std::cout <<"** EDGE_ISOLATED: " <<this->m_Iteration <<std::endl;
    TagElementOut( m_Element );
    break;
    // more than 2 common vertices in 0-ring of org and dest respectively
    case OperatorType::TOO_MANY_COMMON_VERTICES:
      if( m_Verbose )
        {
        std::cout <<"** TOO_MANY_COMMON_VERTICES: " <<this->m_Iteration;
        std::cout <<" ** " <<m_Element->GetOrigin() <<" -> "
          <<m_Element->GetDestination() <<std::endl;
        }
      TagElementOut( m_Element );
      break;
    // ******************************************************************
    // Tetraedron case
    case OperatorType::TETRAEDRON_CONFIG:
      if( m_Verbose )
        std::cout <<"** TETRAEDRON_CONFIG: " <<this->m_Iteration <<std::endl;
        TagElementOut( m_Element );
        TagElementOut( m_Element->GetOnext() );
      TagElementOut( m_Element->GetOprev() );
      TagElementOut( m_Element->GetSym() );
        TagElementOut( m_Element->GetSym()->GetOnext() );
      TagElementOut( m_Element->GetSym()->GetOprev() );
      TagElementOut( m_Element->GetOnext()->GetLnext() );
      break;
    // ******************************************************************
    // Samosa case
    case OperatorType::SAMOSA_CONFIG:
      if( m_Verbose )
        std::cout <<"** SAMOSA_CONFIG: " <<this->m_Iteration <<std::endl;
      RemoveSamosa();
      break;
    // ******************************************************************
    // Eye case
    case OperatorType::EYE_CONFIG:
      if( m_Verbose )
        std::cout <<"** EYE_CONFIG: " <<this->m_Iteration <<std::endl;    
      RemoveEye();
      break;
    case OperatorType::EDGE_JOINING_DIFFERENT_BORDERS:
      if( m_Verbose )
        std::cout <<"** EDGE_JOINING_DIFFERENT_BORDERS: "
          <<this->m_Iteration <<std::endl;
      TagElementOut( m_Element );
      break;
  }
}

template< class TInput, class TOutput, class TCriterion >
void QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
DeletePoint( const OutputPointIdentifier& iIdToBeDeleted, 
  const OutputPointIdentifier& iRemaining )
{
  (void)iRemaining;
  this->GetOutput()->DeletePoint( iIdToBeDeleted );
}

template< class TInput, class TOutput, class TCriterion >
bool QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
ProcessWithoutAnyTopologicalGuarantee()
{
  OutputMeshPointer output = this->GetOutput();
  OutputPointType pt;

  OutputPointIdentifier id_org = m_Element->GetOrigin();
  OutputPointIdentifier id_dest = m_Element->GetDestination();
  OutputPointIdentifier idx = ( id_org < id_dest ) ? id_org : id_dest;

  bool to_be_processed( true );

  if ( m_Relocate )
    pt = Relocate( m_Element );
  else
    pt = output->GetPoint( idx );

///TODO use CheckOrientation!!!
//   if( m_CheckOrientation )
//     to_be_processed = CheckOrientation( m_Element, idx, pt );

  if( !to_be_processed )
    return false;

  std::list< OutputQEType* > list_qe_to_be_deleted;
  OutputQEType* temp = m_Element->GetOnext();
  
  while( temp != m_Element )
    {
    list_qe_to_be_deleted.push_back( temp );
    temp = temp->GetOnext();
    }
  
  temp = m_Element->GetSym()->GetOnext();
  while( temp != m_Element->GetSym() )
    {
    list_qe_to_be_deleted.push_back( temp );
    temp = temp->GetOnext();
    }
  
   for( typename std::list< OutputQEType* >::iterator 
         it = list_qe_to_be_deleted.begin();
         it != list_qe_to_be_deleted.end();
         ++it )
      DeleteElement( *it );
    
  if ( !m_JoinVertexFunction->Evaluate( m_Element ) )
    {
    for( typename std::list< OutputQEType* >::iterator 
         it = list_qe_to_be_deleted.begin();
         it != list_qe_to_be_deleted.end();
         ++it )
      PushOrUpdateElement( *it );
      
    JoinVertexFailed();
    }
  else
    {
    OutputPointIdentifier old_id = m_JoinVertexFunction->GetOldPointID();

    OutputPointIdentifier new_id = ( old_id == id_dest ) ? id_org : id_dest;
    DeletePoint( old_id, new_id );

    OutputQEType* edge = output->FindEdge( new_id );
    if ( edge == 0 )
      {
      if( m_Verbose )
        std::cout <<"** edge == 0 ** " <<this->m_Iteration <<std::endl;

      return false;
      }

    if ( m_Relocate )
      {
      pt.SetEdge( edge );
      output->SetPoint( new_id, pt );
      }

    temp = edge;

    do
      {
      PushOrUpdateElement( temp );
      temp = temp->GetOnext();
      }
    while ( temp != edge );
    }
  return false;
}


template< class TInput, class TOutput, class TCriterion >
unsigned int QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion
>::
CheckQEProcessingStatus( )
{
  OutputQEType* qe = m_Element;
  OutputQEType* qe_sym = qe->GetSym( );

  bool LeftIsTriangle = qe->IsLnextOfTriangle( );
  bool RightIsTriangle = qe->GetSym( )->IsLnextOfTriangle( );

  if( LeftIsTriangle || RightIsTriangle )
    {
    if( LeftIsTriangle && RightIsTriangle )
      {
      // two triangles
      bool OriginOrderIsTwo = ( qe->GetOrder( ) == 2 );
      bool DestinationOrderIsTwo = ( qe_sym->GetOrder( ) == 2 );

      if( OriginOrderIsTwo || DestinationOrderIsTwo )
        {
        if( OriginOrderIsTwo && DestinationOrderIsTwo )
          {
          // isolated component made of two triangles
          // sharing same points but with opposite orientation
          // looks like a samosa
          if( m_Verbose )
            std::cout <<"RemoveSamosa" <<std::endl;
          return 1;
          } // end if( OriginOrderIsTwo && DestinationOrderIsTwo )
        else
          {
          // two triangles share three points and two edges
          // the last edge is duplicated = two edge cells
          // having the same points. It is a valid manifold case
          // but you have to decimate it the right way.
          // from the top the drawing of that case looks like an Eye
          if( m_Verbose )
            std::cout <<"RemoveEye" <<std::endl;
          return 2;
          } // end else if( OriginOrderIsTwo && DestinationOrderIsTwo )
       } // end if( OriginOrderIsTwo || DestinationOrderIsTwo )
      else // if( OriginOrderIsTwo || DestinationOrderIsTwo )
        {
        if( NumberOfCommonVerticesIn0Ring( ) > 2 )
          {
          // both points have more than 2 edges on their O-ring
          if( m_Verbose )
            std::cout <<"NumberOfCommonVerticesIn0Ring( ) > 2" <<std::endl;
          return 3;
          } //end if( NumberOfCommonVerticesIn0Ring( ) > 2 )
        else
          return 0;
        } // end else if( OriginOrderIsTwo || DestinationOrderIsTwo )
      } // end if( LeftIsTriangle && RightIsTriangle )
    else // if( LeftIsTriangle && RightIsTriangle )
      {
      if( NumberOfCommonVerticesIn0Ring( ) > 1 )
        {
        if( m_Verbose )
          std::cout <<"NumberOfCommonVerticesIn0Ring( ) > 1" <<std::endl;
        return 4;
        } // end if( NumberOfCommonVerticesIn0Ring( ) > 1 )
      else // if( NumberOfCommonVerticesIn0Ring( ) > 1 )
        {
        if( RightIsTriangle )
          return 5;
        else
          return 6;
        } // end else if( NumberOfCommonVerticesIn0Ring( ) > 1 )
      } // end else if( LeftIsTriangle && RightIsTriangle )
    } // end if( LeftIsTriangle || RightIsTriangle )
  else // if( LeftIsTriangle || RightIsTriangle )
    {
    if( NumberOfCommonVerticesIn0Ring( ) > 0 )
      return 7;
    else
      return 0;
    } // end if( LeftIsTriangle || RightIsTriangle )

//   return 0;
}

template< class TInput, class TOutput, class TCriterion >
bool QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
ProcessWithTopologicalGuarantee()
{
  if( m_Priority.first )
    return true;

  ProcessWithoutAnyTopologicalGuarantee();
  return false;
}

template< class TInput, class TOutput, class TCriterion >
size_t QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
NumberOfCommonVerticesIn0Ring( )
{
  OutputQEType* qe = m_Element;
  OutputQEType* e_it  = qe->GetOnext( );

  std::list< OutputPointIdentifier > dir_list, sym_list, intersection_list;
  do
    {
    dir_list.push_back( e_it->GetDestination() );
    e_it = e_it->GetOnext();
    } while( e_it != qe );

  qe = qe->GetSym();
  e_it = qe;

  do
    {
    sym_list.push_back( e_it->GetDestination() );
    e_it = e_it->GetOnext();
    } while( e_it != qe );

  dir_list.sort();
  sym_list.sort();

  std::set_intersection( dir_list.begin(), dir_list.end(),
    sym_list.begin(), sym_list.end(),
    std::back_inserter( intersection_list ) );

  return intersection_list.size();
}


template< class TInput, class TOutput, class TCriterion >
void QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
RemoveSamosa()
{
  DeleteElement( m_Element->GetLnext( ) );
  DeleteElement( m_Element->GetLprev( ) );
  DeleteElement( m_Element->GetRnext( ) );
  DeleteElement( m_Element->GetRprev( ) );
}

template< class TInput, class TOutput, class TCriterion >
void QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
TagElementOut( OutputQEType* iEdge )
{
  QueueMapIterator map_it = m_QueueMapper.find( iEdge );

  if( map_it != m_QueueMapper.end() )
    {
    map_it->second->m_Priority.first = true;
    map_it->second->m_Priority.second = static_cast< MeasureType >( 0. );
    m_PriorityQueue->Update( map_it->second );
    }
  else
    {
    PriorityQueueItemType* qi = new PriorityQueueItemType( iEdge,
      PriorityType( true, static_cast< MeasureType >( 0. ) ) );

    m_QueueMapper[ iEdge ] = qi;
    m_PriorityQueue->Push( qi );
    }
}

template< class TInput, class TOutput, class TCriterion >
void QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
RemoveEye()
{
  OutputQEType* qe = m_Element;
  OutputQEType* qe_sym = m_Element->GetSym( );

  if( qe->GetSym( )->GetOrder( ) == 2 )
    qe = qe_sym;

  TagElementOut( qe );
  TagElementOut( qe->GetOnext( ) );
  TagElementOut( qe->GetSym( )->GetOnext( ) );
  TagElementOut( qe->GetSym( )->GetOprev( ) );
}

template< class TInput, class TOutput, class TCriterion >
bool QuadEdgeMeshEdgeMergeDecimationFilter< TInput, TOutput, TCriterion >::
IsCriterionSatisfied()
{
  if ( m_PriorityQueue->Empty() )
    return true;
  else
    return this->m_Criterion->is_satisfied( this->GetOutput(), 0,
      m_Priority.second );
}

}
#endif
