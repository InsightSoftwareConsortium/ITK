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
#ifndef itkQuadEdgeMeshPolygonCell_hxx
#define itkQuadEdgeMeshPolygonCell_hxx

#include "itkQuadEdgeMeshPolygonCell.h"

#include "itkCellInterfaceVisitor.h"

namespace itk
{
// ---------------------------------------------------------------------
template< typename TCellInterface >
QuadEdgeMeshPolygonCell< TCellInterface >
::QuadEdgeMeshPolygonCell(PointIdentifier nPoints)
{
  this->m_Ident = 0;

  // Create entry point
  EdgeCellType *edge = new EdgeCellType;
  m_EdgeCellList.push_back(edge);
  m_EdgeRingEntry = edge->GetQEGeom();

  // Create the rest
  QuadEdgeType *last = m_EdgeRingEntry;
  for ( PointIdentifier i = 1; i < nPoints; i++ )
    {
    edge = new EdgeCellType();
    m_EdgeCellList.push_back(edge);
    QuadEdgeType *edgeGeom = edge->GetQEGeom();

    edgeGeom->Splice( last->GetSym() );
    last = edgeGeom;
    }

  // Last topological connection, i.e., close the face
  m_EdgeRingEntry->Splice( last->GetSym() );

  //MakePointIds( );
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
QuadEdgeMeshPolygonCell< TCellInterface >
::QuadEdgeMeshPolygonCell(QuadEdgeType *e)
{
  this->m_Ident = 0;
  this->m_EdgeRingEntry = e;

  //MakePointIds( );
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
QuadEdgeMeshPolygonCell< TCellInterface >
::~QuadEdgeMeshPolygonCell()
{
  // this disconnect this cell from the
  // QuadEdgeMesh container if there was any.
  // poping this cell from the container is supposed to
  // have been done beforehand.
  this->m_Ident = 0;

  // first case, the polygon was created directly
  // just delete the edges in the edge list
  // the edgecell destructor should  take care of the QuadEdges
  while ( !m_EdgeCellList.empty() )
    {
    EdgeCellType *edge = m_EdgeCellList.back();
    m_EdgeCellList.pop_back();
    delete edge;
    }

  // second case, the polygon cell was created by
  // QuadEdgeMesh::AddFace( ) and the QuadEdgeMesh
  // should take care of everything.
  // We could iterate around the face to check if all
  // QuadEdges have now IsLeftSet( ) = false.
  //
  // TO BE DONE.
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshPolygonCell< TCellInterface >::SelfAutoPointer
QuadEdgeMeshPolygonCell< TCellInterface >
::New()
{
  SelfAutoPointer ptr(new Self);

  ptr.TakeOwnership();
  return ( ptr );
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
void QuadEdgeMeshPolygonCell< TCellInterface >
::Accept(CellIdentifier cellId, MultiVisitor *mv)
{
  typedef CellInterfaceVisitor< PixelType, CellTraits > IntVis;
  typename IntVis::Pointer v = mv->GetVisitor( this->GetType() );
  if ( v )
    {
    v->VisitFromCell(cellId, this);
    }
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
unsigned int QuadEdgeMeshPolygonCell< TCellInterface >
::GetNumberOfPoints() const
{
  // The constructor creates one edge by default
  unsigned int                 n = 0;
  PointIdInternalConstIterator it  = this->InternalPointIdsBegin();
  PointIdInternalConstIterator end = this->InternalPointIdsEnd();

  while ( it != end )
    {
    ++it;
    ++n;
    }
  // it's impossible to get n < 3 except the empty case
  if ( n > 2 )
    {
    return n;
    }
  else
    {
    return 0;
    }
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshPolygonCell< TCellInterface >::CellFeatureCount
QuadEdgeMeshPolygonCell< TCellInterface >
::GetNumberOfBoundaryFeatures(int dimension) const
{
  switch ( dimension )
    {
    case 0:
      return ( this->GetNumberOfPoints() );
    case 1:
      return ( this->GetNumberOfPoints() );
    default:
      return ( 0 );
    }
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
bool QuadEdgeMeshPolygonCell< TCellInterface >
::GetBoundaryFeature(int dimension, CellFeatureIdentifier cellId, CellAutoPointer & cell)
{
  /// \todo
  (void)dimension;
  (void)cellId;
  (void)cell;
  return ( false );
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
void
QuadEdgeMeshPolygonCell< TCellInterface >
::SetPointIds(PointIdConstIterator first)
{
  if ( this->GetNumberOfPoints() > 2 )
    {
    PointIdConstIterator    i2  = first;
    PointIdInternalIterator i1  = this->InternalPointIdsBegin();
    PointIdInternalIterator end = this->InternalPointIdsEnd();

    while ( i1 != end )
      {
      i1.Value()->SetOrigin(*i2);
      ++i1;
      ++i2;
      }
    }
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
void
QuadEdgeMeshPolygonCell< TCellInterface >
::InternalSetPointIds(PointIdInternalConstIterator first)
{
  if ( this->GetNumberOfPoints() > 2 )
    {
    PointIdInternalConstIterator i2  = first;
    PointIdInternalIterator      i1  = this->InternalPointIdsBegin();
    PointIdInternalIterator      end = this->InternalPointIdsEnd();

    while ( i1 != end )
      {
      i1.Value()->SetOrigin(*i2);
      ++i1;
      ++i2;
      }
    }
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
void QuadEdgeMeshPolygonCell< TCellInterface >
::SetPointIds(PointIdConstIterator first,
              PointIdConstIterator last)
{
  PointIdInternalIterator i1  = this->InternalPointIdsBegin();
  PointIdInternalIterator end = this->InternalPointIdsEnd();
  PointIdConstIterator    i2  = first;

  while ( i1 != end && i2 != last )
    {
    i1.Value()->SetOrigin(*i2);
    ++i1;
    ++i2;
    }
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
void QuadEdgeMeshPolygonCell< TCellInterface >
::InternalSetPointIds(PointIdInternalConstIterator first,
                      PointIdInternalConstIterator last)
{
  PointIdInternalIterator      i1  = this->InternalPointIdsBegin();
  PointIdInternalIterator      end = this->InternalPointIdsEnd();
  PointIdInternalConstIterator i2  = first;

  while ( i1 != end && i2 != last )
    {
    i1.Value()->SetOrigin(*i2);
    ++i1;
    ++i2;
    }
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
void QuadEdgeMeshPolygonCell< TCellInterface >
::SetPointId(int localId, PointIdentifier pId)
{
  int                     n = 0;
  PointIdInternalIterator it  = this->InternalPointIdsBegin();
  PointIdInternalIterator end = this->InternalPointIdsEnd();

  while ( it != end && n <= localId )
    {
    if ( n == localId )
      {
      it.Value()->SetOrigin(pId);
      it.Value()->GetOnext()->SetOrigin(pId);
      }
    ++it;
    ++n;
    }
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshPolygonCell< TCellInterface >::PointIdentifier
QuadEdgeMeshPolygonCell< TCellInterface >
::GetPointId(int localId) const
{
  int                          n = 0;
  PointIdInternalConstIterator it = this->InternalPointIdsBegin();

  while ( it != this->InternalPointIdsEnd() && n <= localId )
    {
    if ( n == localId )
      {
      return ( it.Value()->GetOrigin() );
      }
    it++;
    n++;
    }
  return ( PointIdentifier(-1) );
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshPolygonCell< TCellInterface >::PointIdInternalIterator
QuadEdgeMeshPolygonCell< TCellInterface >
::InternalPointIdsBegin()
{
  return m_EdgeRingEntry->BeginGeomLnext();
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshPolygonCell< TCellInterface >::PointIdInternalIterator
QuadEdgeMeshPolygonCell< TCellInterface >
::InternalPointIdsEnd()
{
  return m_EdgeRingEntry->EndGeomLnext();
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshPolygonCell< TCellInterface >::PointIdInternalConstIterator
QuadEdgeMeshPolygonCell< TCellInterface >
::InternalGetPointIds() const
{
  const QuadEdgeType *         edge = const_cast< QuadEdgeType * >( m_EdgeRingEntry );
  PointIdInternalConstIterator iterator( edge->BeginGeomLnext() );

  return iterator;
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshPolygonCell< TCellInterface >::PointIdInternalConstIterator
QuadEdgeMeshPolygonCell< TCellInterface >
::InternalPointIdsBegin() const
{
  const QuadEdgeType *         edge = const_cast< QuadEdgeType * >( m_EdgeRingEntry );
  PointIdInternalConstIterator iterator( edge->BeginGeomLnext() );

  return iterator;
}

// ---------------------------------------------------------------------
template< typename TCellInterface >
typename QuadEdgeMeshPolygonCell< TCellInterface >::PointIdInternalConstIterator
QuadEdgeMeshPolygonCell< TCellInterface >
::InternalPointIdsEnd() const
{
  const QuadEdgeType *         edge = const_cast< const QuadEdgeType * >( m_EdgeRingEntry );
  PointIdInternalConstIterator iterator = edge->EndGeomLnext();

  return iterator;
}
} // end namespace itk

#endif
