/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshPolygonCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshPolygonCell_txx
#define __itkQuadEdgeMeshPolygonCell_txx

#include "itkCellInterfaceVisitor.h"

namespace itk
{
// ---------------------------------------------------------------------
template< class TCellInterface >
QuadEdgeMeshPolygonCell< TCellInterface >
::QuadEdgeMeshPolygonCell( unsigned int nPoints )
{
  this->m_Ident = 0;

  // Create entry point
  EdgeCellType* edge = new EdgeCellType;
  m_EdgeRingEntry = edge->GetQEGeom( );

  // Create the rest
  QuadEdgeType* last = m_EdgeRingEntry;
  for( unsigned int i = 1; i < nPoints; i++ )
    {
    edge = new EdgeCellType( );
    QuadEdgeType* edgeGeom = edge->GetQEGeom( );

    edgeGeom->Splice( last->GetSym() );
    last = edgeGeom;
    }

  // Last topological connection, i.e., close the face
  m_EdgeRingEntry->Splice( last->GetSym() );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
QuadEdgeMeshPolygonCell< TCellInterface >
::QuadEdgeMeshPolygonCell( QuadEdgeType* e )
{
  this->m_Ident = 0;
  this->m_EdgeRingEntry = e;
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshPolygonCell< TCellInterface >::SelfAutoPointer
QuadEdgeMeshPolygonCell< TCellInterface >
::New()
{
  SelfAutoPointer ptr( new Self );
  ptr.TakeOwnership();
  return( ptr );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
void QuadEdgeMeshPolygonCell< TCellInterface >
::Accept( unsigned long cellId, MultiVisitor* mv )
{
  typedef CellInterfaceVisitor< PixelType, CellTraits > IntVis;
  typename IntVis::Pointer v = mv->GetVisitor( this->GetType() );
  if( v ) 
    {
    v->VisitFromCell( cellId, this );
    }
}

// ---------------------------------------------------------------------
template< class TCellInterface >
unsigned int QuadEdgeMeshPolygonCell< TCellInterface >
::GetNumberOfPoints() const
{
  unsigned int n = 0;
  PointIdConstIterator it = this->PointIdsBegin();
  while( it != this->PointIdsEnd() )
    {
    it++;
    n++;
    }
  return n;
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshPolygonCell< TCellInterface >::CellFeatureCount
QuadEdgeMeshPolygonCell< TCellInterface >
::GetNumberOfBoundaryFeatures( int dimension ) const
{
  /// \todo
  (void)dimension;
  return 0;
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    bool QuadEdgeMeshPolygonCell< TCellInterface >::
    GetBoundaryFeature( int dimension, CellFeatureIdentifier cellId,
                        CellAutoPointer& cell )
{
  /// \todo
  (void)dimension;
  (void)cellId;
  (void)cell;
  return( false );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
void 
QuadEdgeMeshPolygonCell< TCellInterface >
::SetPointIds( PointIdConstIterator first )
{
  PointIdConstIterator i2 = first;
  PointIdIterator i1 = this->PointIdsBegin();
  while( i1 != this->PointIdsEnd() )
    {
    i1.Value()->SetOrigin( *i2 );
    i1++;
    i2++;
    }
}

// ---------------------------------------------------------------------
template< class TCellInterface >
void QuadEdgeMeshPolygonCell< TCellInterface >
::SetPointIds( PointIdConstIterator first,
               PointIdConstIterator last )
{
  PointIdIterator i1 = this->PointIdsBegin();
  PointIdConstIterator i2 = first;
  while( i1 != this->PointIdsEnd() && i2 != last )
    {
    i1.Value()->SetOrigin( *i2 );
    i1++;
    i2++;
    }
}

// ---------------------------------------------------------------------
template< class TCellInterface >
void QuadEdgeMeshPolygonCell< TCellInterface >
::SetPointId( int localId, PointIdentifier pId )
{
  int n = 0;
  PointIdIterator it = this->PointIdsBegin();
  while( it != this->PointIdsEnd() && n <= localId )
    {
    if( n == localId )
      {
      it.Value()->SetOrigin( pId );
      it.Value()->GetOnext()->SetOrigin( pId );
      }
    it++; 
    n++;
    }
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshPolygonCell< TCellInterface >::PointIdIterator
QuadEdgeMeshPolygonCell< TCellInterface >
::PointIdsBegin()
{
  return  m_EdgeRingEntry->BeginGeomLnext();
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshPolygonCell< TCellInterface >::PointIdIterator
QuadEdgeMeshPolygonCell< TCellInterface >
::PointIdsEnd()
{
  return m_EdgeRingEntry->EndGeomLnext();
}

// ---------------------------------------------------------------------
// FIXME: This method has a suspicious API and an implementation that
// is the duplicate of PointIdsBegin();
template< class TCellInterface >
typename QuadEdgeMeshPolygonCell< TCellInterface >::PointIdConstIterator
QuadEdgeMeshPolygonCell< TCellInterface >
::GetPointIds() const
{
  const QuadEdgeType * edge = const_cast< QuadEdgeType* >( m_EdgeRingEntry );
  PointIdConstIterator iterator( edge->BeginGeomLnext() );
  return iterator;
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshPolygonCell< TCellInterface >::PointIdConstIterator
QuadEdgeMeshPolygonCell< TCellInterface >
::PointIdsBegin() const
{
  const QuadEdgeType * edge = const_cast< QuadEdgeType* >( m_EdgeRingEntry );
  PointIdConstIterator iterator( edge->BeginGeomLnext() );
  return iterator;
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshPolygonCell< TCellInterface >::PointIdConstIterator
QuadEdgeMeshPolygonCell< TCellInterface >
::PointIdsEnd() const
{
  const QuadEdgeType * edge = const_cast< const QuadEdgeType* >( m_EdgeRingEntry );
  PointIdConstIterator iterator = edge->EndGeomLnext();
  return iterator;
}

} // end namespace itk

#endif
