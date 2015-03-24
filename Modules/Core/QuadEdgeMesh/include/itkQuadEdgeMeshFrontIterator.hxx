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
#ifndef itkQuadEdgeMeshFrontIterator_hxx
#define itkQuadEdgeMeshFrontIterator_hxx

#include "itkQuadEdgeMeshFrontIterator.h"

namespace itk
{
// ---------------------------------------------------------------------
template< typename TMesh, typename TQE >
QuadEdgeMeshFrontBaseIterator< TMesh, TQE >::
QuadEdgeMeshFrontBaseIterator(MeshType *mesh, bool start, QEType *seed) :
  m_Mesh(mesh),
  m_Seed(seed),
  m_Start(start),
  m_Front(ITK_NULLPTR),
  m_CurrentEdge(ITK_NULLPTR)
{
  if ( !mesh )
    {
    // Invalidate the iterator and call it quits
    m_Start = false;
    return;
    }
  if ( !seed )
    {
    seed = FindDefaultSeed();
    if ( !seed )
      {
      // Invalidate the iterator and call it quits
      m_Start = false;
      return;
      }
    }
  m_Front = new FrontType;
  m_Front->push_back( FrontAtom(seed, 0) );
  m_IsPointVisited = IsVisitedContainerType::New();
  m_IsPointVisited->SetElement(seed->GetOrigin(), true);
  m_IsPointVisited->SetElement(seed->GetDestination(), true);
  m_CurrentEdge = seed;
}

// ---------------------------------------------------------------------
template< typename TMesh, typename TQE >
QuadEdgeMeshFrontBaseIterator< TMesh, TQE >::
~QuadEdgeMeshFrontBaseIterator()
{
  delete m_Front;
}

// ---------------------------------------------------------------------
template< typename TMesh, typename TQE >
QuadEdgeMeshFrontBaseIterator< TMesh, TQE > &
QuadEdgeMeshFrontBaseIterator< TMesh, TQE >::operator++()
{
  // We continue only if not previously marked as finish...
  if ( !m_Start )
    {
    return ( *this );
    }

  // ... or until the front is empty:
  if ( m_Front->empty() )
    {
    m_Start = false;
    return ( *this );
    }

  // Sort on the Cost:
  m_Front->sort();
  // Consider the edge with lowest Cost:
  FrontTypeIterator fit = m_Front->begin();
  QEType *          edge = fit->m_Edge;

  // Traverse the Onext ring in search of an unvisited Origin:
  typedef typename QEType::IteratorGeom QEIterator;
  for ( QEIterator qit  = edge->BeginGeomOnext();
        qit != edge->EndGeomOnext();
        qit++ )
    {
    QEType *oEdge = qit.Value();
    // Things are quite straightforward except when QEType
    // is in fact a QEDual (in disguise) AND oEdge->GetDestination( )
    // is m_NoFace [in which case oEdge->GetDestination() has a value
    // but oEdge->IsDestination() is false]. When this is the case
    // we consider oEdge->GetDestination() as already visited.
    if ( ( m_IsPointVisited->IndexExists( oEdge->GetDestination() ) )
         || ( !oEdge->IsDestinationSet() ) )
      {
      continue;
      }
    else
      {
      // Mark the destination as visited:
      m_IsPointVisited->SetElement(oEdge->GetDestination(), true);

      // Compute the Cost of the new OriginType:
      CoordRepType oCost = this->GetCost(oEdge) + fit->m_Cost;

      // Push the Sym() on the front:
      m_Front->push_back( FrontAtom(oEdge->GetSym(), oCost) );

      // We still want to handle oEdge
      m_CurrentEdge = oEdge;
      return ( *this );
      }
    }

  // All the edge->Origin() neighbours were already visited. Remove
  // the edge from the front, and move to next edge...
  m_Front->pop_front();
  m_CurrentEdge = (QEType *)ITK_NULLPTR;
  return ( this->operator++() );
}

/**
 * Find in the cell container an arbitrary underlying edge with type
 * QEType.
 */
template< typename TMesh, typename TQE >
typename QuadEdgeMeshFrontBaseIterator< TMesh, TQE >::QEType *
QuadEdgeMeshFrontBaseIterator< TMesh, TQE >::FindDefaultSeed()
{
  if ( QEType * edge = dynamic_cast< QEType * >( m_Mesh->GetEdge() ) )
    {
    return edge;
    }
  typedef typename QEType::DualType QEDual;
  if ( QEDual * edge = dynamic_cast< QEDual * >( m_Mesh->GetEdge() ) )
    {
    return edge->GetRot();
    }
  return (QEType *)ITK_NULLPTR;
}
}
#endif
