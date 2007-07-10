/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshLineCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshLineCell_txx
#define __itkQuadEdgeMeshLineCell_txx

#include "itkCellInterfaceVisitor.h"
#include "itkQuadEdgeMeshLineCell.h"

namespace itk
{

// ---------------------------------------------------------------------
template< class TCellInterface >
QuadEdgeMeshLineCell< TCellInterface >
::QuadEdgeMeshLineCell( )
{
  m_Identifier = 0;
  m_QuadEdgeGeom = new QEType;

  QEType* e2 = new QEType;
  QEDual* e1 = new QEDual;
  QEDual* e3 = new QEDual;
  this->m_QuadEdgeGeom->SetRot( e1 );
  e1->SetRot( e2 );
  e2->SetRot( e3 );
  e3->SetRot( this->m_QuadEdgeGeom );
  this->m_QuadEdgeGeom->SetOnext( this->m_QuadEdgeGeom );
  e1->SetOnext( e3 );
  e2->SetOnext( e2 );
  e3->SetOnext( e1 );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
QuadEdgeMeshLineCell< TCellInterface >
::~QuadEdgeMeshLineCell()
{
  // here suppose that the edge is isolated
  bool FoundNullPointer = false;
  if( m_QuadEdgeGeom )
    {
    if( m_QuadEdgeGeom->GetRot( ) )
      {
      if( m_QuadEdgeGeom->GetRot( )->GetRot( ) )
        {
        if( m_QuadEdgeGeom->GetRot( )->GetRot( )->GetRot( ) )
          {
          delete m_QuadEdgeGeom->GetRot( )->GetRot( )->GetRot( ); //e3
          delete m_QuadEdgeGeom->GetRot( )->GetRot( );            //e2
          delete m_QuadEdgeGeom->GetRot( );                       //e1
          delete m_QuadEdgeGeom;
          }
        else
          {
            FoundNullPointer = true;
          }
        }
      else
        {
          FoundNullPointer = true;
        }
      }
    else
      {
        FoundNullPointer = true;
      }
    }
  else
    {
      FoundNullPointer = true;
    }

  if( FoundNullPointer )
    {
    //Throw exception here
    }
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::SelfAutoPointer
QuadEdgeMeshLineCell< TCellInterface >
::New()
{
  SelfAutoPointer ptr( new Self );
  ptr.TakeOwnership();
  return( ptr );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
void
QuadEdgeMeshLineCell< TCellInterface >
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
typename QuadEdgeMeshLineCell< TCellInterface >::CellFeatureCount
QuadEdgeMeshLineCell< TCellInterface >
::GetNumberOfBoundaryFeatures( int dimension ) const
{
  if( dimension == 0 )
    {
    return  2;
    }

  if( dimension == 1 )
    {
    return 1;
    }

  return 0;
}

// ---------------------------------------------------------------------
template< class TCellInterface >
bool
QuadEdgeMeshLineCell< TCellInterface >
::GetBoundaryFeature( int dimension, CellFeatureIdentifier cellId,
                      CellAutoPointer& cell )
{
  // TODO : FIXME
  (void)dimension;
  (void)cellId;
  (void)cell;
  return( false );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
void
QuadEdgeMeshLineCell< TCellInterface >
::SetPointIds( PointIdConstIterator first )
{
  PointIdConstIterator i = first;
  this->GetQEGeom( )->SetOrigin( *i );
  i++;
  this->GetQEGeom( )->SetDestination( *i );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
void
QuadEdgeMeshLineCell< TCellInterface >
::SetPointIds( PointIdConstIterator first,
               PointIdConstIterator last )
{
  this->SetOrigin( *first );
  this->SetDestination( *last );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
void
QuadEdgeMeshLineCell< TCellInterface >
::SetPointId( int localId, PointIdentifier pId )
{
  if( localId == 0 )      this->GetQEGeom( )->SetOrigin( pId );
  else if( localId == 1 ) this->GetQEGeom( )->SetDestination( pId );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::PointIdIterator
QuadEdgeMeshLineCell< TCellInterface >
::PointIdsBegin()
{
   return( PointIdIterator( this->m_QuadEdgeGeom, PointIdIterator::OperatorSym, true ) );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::PointIdIterator
QuadEdgeMeshLineCell< TCellInterface >
::PointIdsEnd()
{
  return( PointIdIterator( this->m_QuadEdgeGeom, PointIdIterator::OperatorSym, false ) );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::PointIdConstIterator
QuadEdgeMeshLineCell< TCellInterface >
::GetPointIds() const
{
  return( PointIdConstIterator( this->m_QuadEdgeGeom, PointIdConstIterator::OperatorSym, true ) );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::PointIdConstIterator
QuadEdgeMeshLineCell< TCellInterface >
::PointIdsBegin() const
{
  return( PointIdConstIterator( this->m_QuadEdgeGeom, PointIdConstIterator::OperatorSym, true ) );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::PointIdConstIterator
QuadEdgeMeshLineCell< TCellInterface >
::PointIdsEnd() const
{
  return( PointIdConstIterator( this->m_QuadEdgeGeom, PointIdConstIterator::OperatorSym, false ) );
}


// ---------------------------------------------------------------------
template< class TCellInterface >
void
QuadEdgeMeshLineCell< TCellInterface >
::SetIdent( CellIdentifier cid )
{
  this->m_Identifier = cid;
  this->m_QuadEdgeGeom->SetIdent( cid );
  this->m_QuadEdgeGeom->GetSym( )->SetIdent( cid );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::CellIdentifier
QuadEdgeMeshLineCell< TCellInterface >
::GetIdent()
{
  return this->m_Identifier;
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::CellGeometry
QuadEdgeMeshLineCell< TCellInterface >
::GetType() const
{
  return  Superclass::LINE_CELL;
}

// ---------------------------------------------------------------------
template< class TCellInterface >
int
QuadEdgeMeshLineCell< TCellInterface >
::GetTopologyId()
{
  return Superclass::LINE_CELL;
}

// ---------------------------------------------------------------------
template< class TCellInterface >
unsigned int
QuadEdgeMeshLineCell< TCellInterface >
::GetDimension() const
{
  return Self::CellDimension;
}

// ---------------------------------------------------------------------
template< class TCellInterface >
unsigned int
QuadEdgeMeshLineCell< TCellInterface >
::GetNumberOfPoints() const
{
  return 2;
}

} // end namespace itk

#endif
