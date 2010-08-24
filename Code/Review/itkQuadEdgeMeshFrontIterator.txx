/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshFrontIterator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshFrontIterator_txx
#define __itkQuadEdgeMeshFrontIterator_txx

#include "itkQuadEdgeMeshFrontIterator.h"

namespace itk
{
// ---------------------------------------------------------------------
template< class TMesh, class TQE >
QuadEdgeMeshFrontBaseIterator< TMesh, TQE >::QuadEdgeMeshFrontBaseIterator(MeshType *mesh,
                                                                           bool start,
                                                                           QEType *seed):
  m_Mesh(mesh), m_Seed(seed), m_Start(start)
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
template< class TMesh, class TQE >
QuadEdgeMeshFrontBaseIterator< TMesh, TQE >::
~QuadEdgeMeshFrontBaseIterator()
{
  if ( m_Front )
    {
    delete m_Front;
    }
}

// ---------------------------------------------------------------------
template< class TMesh, class TQE >
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

  // All the edge->Origin() neighbours were allready visited. Remove
  // the edge from the front, and move to next edge...
  m_Front->pop_front();
  m_CurrentEdge = (QEType *)0;
  return ( this->operator++() );
}

/**
 * Find in the cell container an arbitrary underlying edge with type
 * QEType.
 */
template< class TMesh, class TQE >
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
  return (QEType *)0;
}
}
#endif
