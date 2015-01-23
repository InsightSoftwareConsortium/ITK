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
#ifndef itkEdgeDecimationQuadEdgeMeshFilter_hxx
#define itkEdgeDecimationQuadEdgeMeshFilter_hxx

#include "itkEdgeDecimationQuadEdgeMeshFilter.h"

namespace itk
{
template< typename TInput, typename TOutput, typename TCriterion >
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput,TCriterion >::
EdgeDecimationQuadEdgeMeshFilter() :
  Superclass(),
  m_Relocate(true),
  m_CheckOrientation(false),
  m_Element(ITK_NULLPTR)

{
  m_JoinVertexFunction = OperatorType::New();
  m_PriorityQueue = PriorityQueueType::New();
}

template< typename TInput, typename TOutput, typename TCriterion >
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::
~EdgeDecimationQuadEdgeMeshFilter()
{
  OutputQEType *edge;

  while ( !m_PriorityQueue->Empty() )
    {
    edge = m_PriorityQueue->Peek()->m_Element;
    m_PriorityQueue->Pop();

    QueueMapIterator it = m_QueueMapper.find(edge);
    delete it->second;
    m_QueueMapper.erase(it);
    }
}

template< typename TInput, typename TOutput, typename TCriterion >
void
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::FillPriorityQueue()
{
  OutputMeshPointer output = this->GetOutput();

  m_JoinVertexFunction->SetInput(output);

  OutputCellsContainerIterator it = output->GetEdgeCells()->Begin();
  OutputCellsContainerIterator end = output->GetEdgeCells()->End();

  OutputEdgeCellType *edge;

  // cache for use in MeasureEdge
  this->m_OutputMesh = this->GetOutput();

  while ( it != end )
    {
    edge = dynamic_cast< OutputEdgeCellType * >( it.Value() );

    if ( edge )
      {
      PushElement( edge->GetQEGeom() );
      }
    ++it;
    }
}

template< typename TInput, typename TOutput, typename TCriterion >
void
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::PushElement(OutputQEType *iEdge)
{
  OutputPointIdentifier id_org = iEdge->GetOrigin();
  OutputPointIdentifier id_dest = iEdge->GetDestination();

  OutputQEType *temp = ( id_org < id_dest ) ? iEdge : iEdge->GetSym();
  MeasureType   measure = MeasureEdge(temp);

  PriorityQueueItemType *qi = new PriorityQueueItemType( temp,
                                                         PriorityType(false, measure) );

  m_QueueMapper[temp] = qi;
  m_PriorityQueue->Push(qi);
}

template< typename TInput, typename TOutput, typename TCriterion >
bool
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::
#ifdef NDEBUG
IsEdgeOKToBeProcessed(OutputQEType *iEdge)
#else
IsEdgeOKToBeProcessed(OutputQEType *)
#endif
  {
#ifdef NDEBUG
  if ( iEdge == ITK_NULLPTR )
    {
    itkDebugMacro("iEdge == 0, at iteration: " << this->m_Iteration);
    return false;
    }

  OutputPointIdentifier id_org = iEdge->GetOrigin();
  if ( id_org == iEdge->m_NoPoint )
    {
    itkDebugMacro("id_org == iEdge->m_NoPoint, at iteration: "
                  << this->m_Iteration);
    return false;
    }

  OutputMeshPointer output = this->GetOutput();
  if ( output->FindEdge(id_org) == ITK_NULLPTR )
    {
    itkDebugMacro("output->FindEdge( id_org ) == 0, at iteration: "
                  << this->m_Iteration);
    return false;
    }
  if ( iEdge->GetSym() == ITK_NULLPTR )
    {
    itkDebugMacro("iEdge->GetSym() == 0, at iteration: "
                  << this->m_Iteration);
    return false;
    }

  OutputPointIdentifier id_dest = iEdge->GetDestination();
  if ( id_dest == iEdge->m_NoPoint )
    {
    itkDebugMacro("id_dest == iEdge->m_NoPoint, at iteration: "
                  << this->m_Iteration);
    return false;
    }
  if ( output->FindEdge(id_dest) == ITK_NULLPTR )
    {
    itkDebugMacro("output->FindEdge( id_dest ) == 0, at iteration: "
                  << this->m_Iteration);
    return false;
    }
  if ( output->FindEdge(id_org, id_dest) == ITK_NULLPTR )
    {
    itkDebugMacro("output->FindEdge( id_org, id_dest ) == 0, at iteration: "
                  << this->m_Iteration);
    return false;
    }
#endif

  return true;
  }

template< typename TInput, typename TOutput, typename TCriterion >
void
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::Extract()
{
  OutputMeshPointer output = this->GetOutput();

  do
    {
    m_Element = m_PriorityQueue->Peek()->m_Element;
    m_Priority = m_PriorityQueue->Peek()->m_Priority;

    m_PriorityQueue->Pop();
    QueueMapIterator it = m_QueueMapper.find(m_Element);
    delete it->second;
    m_QueueMapper.erase(it);
    }
  while ( !IsEdgeOKToBeProcessed(m_Element) );
}

template< typename TInput, typename TOutput, typename TCriterion >
void
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::DeleteElement(OutputQEType *iEdge)
{
  if ( iEdge ) // this test can be removed
    {
    OutputQEType *temp = ( iEdge->GetOrigin() < iEdge->GetDestination() ) ?
                         iEdge : iEdge->GetSym();

    QueueMapIterator map_it = m_QueueMapper.find(temp);
    if ( map_it != m_QueueMapper.end() )
      {
      if ( !map_it->second->m_Priority.first )
        {
        PriorityQueueItemType* e( map_it->second );
        m_PriorityQueue->DeleteElement(e);
        delete map_it->second;
        m_QueueMapper.erase(map_it);
        }
      }
    }
}

template< typename TInput, typename TOutput, typename TCriterion >
void
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::PushOrUpdateElement(OutputQEType *iEdge)
{
  OutputQEType *temp = iEdge;

  if ( temp->GetOrigin() > temp->GetDestination() )
    {
    temp = temp->GetSym();
    }

  QueueMapIterator map_it = m_QueueMapper.find(temp);

  MeasureType measure = MeasureEdge(temp);
  if ( map_it != m_QueueMapper.end() )
    {
    if ( !map_it->second->m_Priority.first )
      {
      map_it->second->m_Priority.second = measure;
      m_PriorityQueue->Update(map_it->second);
      }
    }
  else
    {
    PriorityQueueItemType *qi = new PriorityQueueItemType( temp,
                                                           PriorityType(false, measure) );
    m_QueueMapper[temp] = qi;
    m_PriorityQueue->Push(qi);
    }
}

template< typename TInput, typename TOutput, typename TCriterion >
void
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::JoinVertexFailed()
{
  typename OperatorType::EdgeStatusType
  status = m_JoinVertexFunction->GetEdgeStatus();
  switch ( status )
    {
    default:
    case OperatorType::EDGE_NULL:
    case OperatorType::MESH_NULL:
    case OperatorType::FACE_ISOLATED:
      break;
    case OperatorType::EDGE_ISOLATED:
      itkDebugMacro("EDGE_ISOLATED, at iteration: " << this->m_Iteration);
      TagElementOut(m_Element);
      break;
    // more than 2 common vertices in 0-ring of org and dest respectively
    case OperatorType::TOO_MANY_COMMON_VERTICES:
      itkDebugMacro("TOO_MANY_COMMON_VERTICES, at iteration "
                    << this->m_Iteration);
      itkDebugMacro( << m_Element->GetOrigin() << " -> "
                     << m_Element->GetDestination() );
      this->TagElementOut(m_Element);
      break;
    // ******************************************************************
    // Tetrahedron case
    case OperatorType::TETRAHEDRON_CONFIG:
      itkDebugMacro("TETRAHEDRON_CONFIG, at iteration " << this->m_Iteration);

      this->TagElementOut(m_Element);
      this->TagElementOut( m_Element->GetOnext() );
      this->TagElementOut( m_Element->GetOprev() );
      this->TagElementOut( m_Element->GetSym() );
      this->TagElementOut( m_Element->GetSym()->GetOnext() );
      this->TagElementOut( m_Element->GetSym()->GetOprev() );
      this->TagElementOut( m_Element->GetOnext()->GetLnext() );
      break;
    // ******************************************************************
    // Samosa case
    case OperatorType::SAMOSA_CONFIG:
      itkDebugMacro("SAMOSA_CONFIG, at iteration " << this->m_Iteration);
      this->RemoveSamosa();
      break;
    // ******************************************************************
    // Eye case
    case OperatorType::EYE_CONFIG:
      itkDebugMacro("EYE_CONFIG, at iteration " << this->m_Iteration);
      this->RemoveEye();
      break;
    case OperatorType::EDGE_JOINING_DIFFERENT_BORDERS:
      itkDebugMacro("EDGE_JOINING_DIFFERENT_BORDERS, at iteration "
                    << this->m_Iteration);
      this->TagElementOut(m_Element);
      break;
    }
}

template< typename TInput, typename TOutput, typename TCriterion >
void
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::DeletePoint(
  const OutputPointIdentifier & iIdToBeDeleted,
  const OutputPointIdentifier &
  iRemaining)
{
  (void)iRemaining;
  this->GetOutput()->DeletePoint(iIdToBeDeleted);
}

template< typename TInput, typename TOutput, typename TCriterion >
bool
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::ProcessWithoutAnyTopologicalGuarantee()
{
  OutputPointType   pt;

  OutputPointIdentifier id_org = m_Element->GetOrigin();
  OutputPointIdentifier id_dest = m_Element->GetDestination();
  OutputPointIdentifier idx = ( id_org < id_dest ) ? id_org : id_dest;

  bool to_be_processed(true);

  if ( m_Relocate )
    {
    pt = Relocate(m_Element);
    }
  else
    {
    pt = this->m_OutputMesh->GetPoint(idx);
    }

///TODO use CheckOrientation!!!
//   if( m_CheckOrientation )
//     to_be_processed = CheckOrientation( m_Element, idx, pt );

  if ( !to_be_processed )
    {
    return false;
    }

  std::list< OutputQEType * > list_qe_to_be_deleted;
  OutputQEType *              temp = m_Element->GetOnext();

  while ( temp != m_Element )
    {
    list_qe_to_be_deleted.push_back(temp);
    temp = temp->GetOnext();
    }

  temp = m_Element->GetSym()->GetOnext();
  while ( temp != m_Element->GetSym() )
    {
    list_qe_to_be_deleted.push_back(temp);
    temp = temp->GetOnext();
    }

  typename std::list< OutputQEType * >::iterator
  it = list_qe_to_be_deleted.begin();

  while ( it != list_qe_to_be_deleted.end() )
    {
    DeleteElement(*it);
    ++it;
    }

  if ( !m_JoinVertexFunction->Evaluate(m_Element) )
    {
    it = list_qe_to_be_deleted.begin();

    while ( it != list_qe_to_be_deleted.end() )
      {
      PushOrUpdateElement(*it);
      ++it;
      }

    JoinVertexFailed();
    }
  else
    {
    OutputPointIdentifier old_id = m_JoinVertexFunction->GetOldPointID();

    OutputPointIdentifier new_id = ( old_id == id_dest ) ? id_org : id_dest;
    DeletePoint(old_id, new_id);

    OutputQEType *edge = this->m_OutputMesh->FindEdge(new_id);
    if ( edge == ITK_NULLPTR )
      {
      itkDebugMacro("edge == 0, at iteration " << this->m_Iteration);
      return false;
      }

    if ( m_Relocate )
      {
      pt.SetEdge(edge);
      this->m_OutputMesh->SetPoint(new_id, pt);
      }

    temp = edge;

    do
      {
      PushOrUpdateElement(temp);
      temp = temp->GetOnext();
      }
    while ( temp != edge );
    }
  return false;
}

template< typename TInput, typename TOutput, typename TCriterion >
unsigned int
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::CheckQEProcessingStatus()
{
  OutputQEType *qe = m_Element;
  OutputQEType *qe_sym = qe->GetSym();

  bool LeftIsTriangle = qe->IsLnextOfTriangle();
  bool RightIsTriangle = qe->GetSym()->IsLnextOfTriangle();

  if ( LeftIsTriangle || RightIsTriangle )
    {
    if ( LeftIsTriangle && RightIsTriangle )
      {
      // two triangles
      bool OriginOrderIsTwo = ( qe->GetOrder() == 2 );
      bool DestinationOrderIsTwo = ( qe_sym->GetOrder() == 2 );

      if ( OriginOrderIsTwo || DestinationOrderIsTwo )
        {
        if ( OriginOrderIsTwo && DestinationOrderIsTwo )
          {
          // isolated component made of two triangles
          // sharing same points but with opposite orientation
          // looks like a samosa
          itkDebugMacro("RemoveSamosa");
          return 1;
          } // end if( OriginOrderIsTwo && DestinationOrderIsTwo )
        else
          {
          // two triangles share three points and two edges
          // the last edge is duplicated = two edge cells
          // having the same points. It is a valid manifold case
          // but you have to decimate it the right way.
          // from the top the drawing of that case looks like an Eye
          itkDebugMacro("RemoveEye");
          return 2;
          } // end else if( OriginOrderIsTwo && DestinationOrderIsTwo )
        }   // end if( OriginOrderIsTwo || DestinationOrderIsTwo )
      else  // if( OriginOrderIsTwo || DestinationOrderIsTwo )
        {
        if ( NumberOfCommonVerticesIn0Ring() > 2 )
          {
          // both points have more than 2 edges on their O-ring
          itkDebugMacro("NumberOfCommonVerticesIn0Ring( ) > 2");
          return 3;
          } //end if( NumberOfCommonVerticesIn0Ring( ) > 2 )
        else
          {
          return 0;
          }
        } // end else if( OriginOrderIsTwo || DestinationOrderIsTwo )
      }   // end if( LeftIsTriangle && RightIsTriangle )
    else  // if( LeftIsTriangle && RightIsTriangle )
      {
      if ( NumberOfCommonVerticesIn0Ring() > 1 )
        {
        itkDebugMacro("NumberOfCommonVerticesIn0Ring( ) > 1");
        return 4;
        }  // end if( NumberOfCommonVerticesIn0Ring( ) > 1 )
      else // if( NumberOfCommonVerticesIn0Ring( ) > 1 )
        {
        if ( RightIsTriangle )
          {
          return 5;
          }
        else
          {
          return 6;
          }
        } // end else if( NumberOfCommonVerticesIn0Ring( ) > 1 )
      }   // end else if( LeftIsTriangle && RightIsTriangle )
    }     // end if( LeftIsTriangle || RightIsTriangle )
  else    // if( LeftIsTriangle || RightIsTriangle )
    {
    if ( NumberOfCommonVerticesIn0Ring() > 0 )
      {
      return 7;
      }
    else
      {
      return 0;
      }
    } // end if( LeftIsTriangle || RightIsTriangle )

  //   return 0;
}

template< typename TInput, typename TOutput, typename TCriterion >
bool
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::ProcessWithTopologicalGuarantee()
{
  if ( m_Priority.first )
    {
    return true;
    }

  ProcessWithoutAnyTopologicalGuarantee();
  return false;
}

template< typename TInput, typename TOutput, typename TCriterion >
SizeValueType
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::NumberOfCommonVerticesIn0Ring() const
{
  OutputQEType *qe = m_Element;
  OutputQEType *e_it  = qe->GetOnext();

  std::list< OutputPointIdentifier > dir_list, sym_list, intersection_list;
  do
    {
    dir_list.push_back( e_it->GetDestination() );
    e_it = e_it->GetOnext();
    }
  while ( e_it != qe );

  qe = qe->GetSym();
  e_it = qe;

  do
    {
    sym_list.push_back( e_it->GetDestination() );
    e_it = e_it->GetOnext();
    }
  while ( e_it != qe );

  dir_list.sort();
  sym_list.sort();

  std::set_intersection( dir_list.begin(), dir_list.end(),
                         sym_list.begin(), sym_list.end(),
                         std::back_inserter(intersection_list) );

  return static_cast< SizeValueType >( intersection_list.size() );
}

template< typename TInput, typename TOutput, typename TCriterion >
void
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::RemoveSamosa()
{
  DeleteElement( m_Element->GetLnext() );
  DeleteElement( m_Element->GetLprev() );
  DeleteElement( m_Element->GetRnext() );
  DeleteElement( m_Element->GetRprev() );
}

template< typename TInput, typename TOutput, typename TCriterion >
void
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::TagElementOut(OutputQEType *iEdge)
{
  QueueMapIterator map_it = m_QueueMapper.find(iEdge);

  if ( map_it != m_QueueMapper.end() )
    {
    map_it->second->m_Priority.first = true;
    map_it->second->m_Priority.second = static_cast< MeasureType >( 0. );
    m_PriorityQueue->Update(map_it->second);
    }
  else
    {
    PriorityQueueItemType *qi = new PriorityQueueItemType( iEdge,
                                                           PriorityType( true, static_cast< MeasureType >( 0. ) ) );

    m_QueueMapper[iEdge] = qi;
    m_PriorityQueue->Push(qi);
    }
}

template< typename TInput, typename TOutput, typename TCriterion >
void
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::RemoveEye()
{
  OutputQEType *qe = m_Element;
  OutputQEType *qe_sym = m_Element->GetSym();

  if ( qe->GetSym()->GetOrder() == 2 )
    {
    qe = qe_sym;
    }

  TagElementOut(qe);
  TagElementOut( qe->GetOnext() );
  TagElementOut( qe->GetSym()->GetOnext() );
  TagElementOut( qe->GetSym()->GetOprev() );
}

template< typename TInput, typename TOutput, typename TCriterion >
bool
EdgeDecimationQuadEdgeMeshFilter< TInput, TOutput, TCriterion >::IsCriterionSatisfied()
{
  if ( m_PriorityQueue->Empty() )
    {
    return true;
    }
  else
    {
    return this->m_Criterion->is_satisfied(this->GetOutput(), 0, m_Priority.second);
    }
}
}
#endif
