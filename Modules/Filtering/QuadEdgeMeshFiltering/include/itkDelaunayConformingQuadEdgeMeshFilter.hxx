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
#ifndef itkDelaunayConformingQuadEdgeMeshFilter_hxx
#define itkDelaunayConformingQuadEdgeMeshFilter_hxx

#include "itkDelaunayConformingQuadEdgeMeshFilter.h"

namespace itk
{
// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh >
DelaunayConformingQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::DelaunayConformingQuadEdgeMeshFilter()
{
  this->m_NumberOfEdgeFlips = 0;
  this->m_FlipEdge = FlipEdgeFunctionType::New();
  this->m_PriorityQueue = PriorityQueueType::New();
}

// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh >
DelaunayConformingQuadEdgeMeshFilter< TInputMesh, TOutputMesh >::
~DelaunayConformingQuadEdgeMeshFilter()
{
  OutputEdgeCellType *edge;

  while ( !m_PriorityQueue->Empty() )
    {
    edge = m_PriorityQueue->Peek()->m_Element;

    m_PriorityQueue->Pop();
    delete m_QueueMapper[edge];
    m_QueueMapper.erase(edge);
    }
}

// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh >
void DelaunayConformingQuadEdgeMeshFilter< TInputMesh, TOutputMesh >::InitializePriorityQueue()
{
  OutputMeshType *output = this->GetOutput();

  OutputEdgeCellType *edge = ITK_NULLPTR;

  CriterionValueType value = 0.;

  for ( OutputCellsContainerIterator
        outCellIterator = output->GetEdgeCells()->Begin();
        outCellIterator != output->GetEdgeCells()->End();
        ++outCellIterator )
    {
    if ( ( edge = dynamic_cast< OutputEdgeCellType * >(
             outCellIterator.Value() ) ) )
      {
      value = Dyer07Criterion( output, edge->GetQEGeom() );

      if ( value > 0.0 )
        {
        PriorityQueueItemType *qi =
          new PriorityQueueItemType( edge, PriorityType(true, value) );
        m_QueueMapper[edge] = qi;
        m_PriorityQueue->Push(qi);
        }
      }
    }

  OutputEdgeCellListIterator const_edge_it =
    m_ListOfConstrainedEdges.begin();

  while ( const_edge_it != m_ListOfConstrainedEdges.end() )
    {
    QueueMapIterator queue_it = m_QueueMapper.find(*const_edge_it);

    if ( queue_it == m_QueueMapper.end() )
      {
      queue_it->second->m_Priority = PriorityType(false, value);
      m_PriorityQueue->Update(queue_it->second);
      }
    else
      {
      PriorityQueueItemType *qi =
        new PriorityQueueItemType( edge, PriorityType(false, 0.0) );
      m_QueueMapper[edge] = qi;
      m_PriorityQueue->Push(qi);
      }

    ++const_edge_it;
    }
}

// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh >
void DelaunayConformingQuadEdgeMeshFilter< TInputMesh, TOutputMesh >::Process()
{
  OutputMeshType *output = this->GetOutput();

  m_FlipEdge->SetInput(output);

  typename std::vector< OutputQEType * > list_qe(5);
  typename std::vector< OutputQEType * >::iterator it;

  OutputEdgeCellType *edge;
  OutputQEType *      qe;
  OutputQEType *      e_it;

  CriterionValueType value;

  while ( !m_PriorityQueue->Empty() )
    {
    if ( !m_PriorityQueue->Peek()->m_Priority.first )
      {
      break;
      }

    edge = m_PriorityQueue->Peek()->m_Element;
    qe = edge->GetQEGeom();

    list_qe[0] = qe->GetLnext();
    list_qe[1] = qe->GetLprev();
    list_qe[2] = qe->GetRnext();
    list_qe[3] = qe->GetRprev();

    m_PriorityQueue->Pop();
    delete m_QueueMapper[edge];
    m_QueueMapper.erase(edge);

    qe = m_FlipEdge->Evaluate(qe);
    if ( qe != ITK_NULLPTR )
      {
      ++this->m_NumberOfEdgeFlips;
      list_qe[4] = qe;

      for ( it = list_qe.begin(); it != list_qe.end(); ++it )
        {
        e_it = *it;
        if ( e_it )
          {
          value = Dyer07Criterion(output, e_it);
          if ( value > 0.0 )
            {
            edge = output->FindEdgeCell( e_it->GetOrigin(),
                                         e_it->GetDestination() );
            QueueMapIterator queue_it = m_QueueMapper.find(edge);
            if ( queue_it == m_QueueMapper.end() )
              {
              PriorityQueueItemType *qi =
                new PriorityQueueItemType( edge,
                                           PriorityType(true, value) );
              m_QueueMapper[edge] = qi;
              m_PriorityQueue->Push(qi);
              }
            else
              {
              if ( queue_it->second->m_Priority.first )
                {
                queue_it->second->m_Priority = PriorityType(true, value);
                m_PriorityQueue->Update(queue_it->second);
                }
              }
            }
          }
        }
      }
    }
}

// ---------------------------------------------------------------------
template< typename TInputMesh, typename TOutputMesh >
void DelaunayConformingQuadEdgeMeshFilter< TInputMesh, TOutputMesh >::GenerateData()
{
  this->CopyInputMeshToOutputMesh();

  // initialize all required instances
  this->m_NumberOfEdgeFlips = 0;

  this->InitializePriorityQueue();
  this->Process();
}
template< typename TInputMesh, typename TOutputMesh >
void DelaunayConformingQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "NumberOfEdgeFlips: " << m_NumberOfEdgeFlips << std::endl;
}

} // end namespace itk

#endif
