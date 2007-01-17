// -------------------------------------------------------------------------
// itkQuadEdgeMeshLineCell.txx
// $Revision: 1.2 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-17 19:18:04 $
// -------------------------------------------------------------------------
// This code is an implementation of the well known quad edge (QE) data
// structure in the ITK library. Although the original QE can handle non
// orientable 2-manifolds and its dual and its mirror, this implementation
// is specifically dedicated to handle orientable 2-manifolds along with
// their dual.
//
// Any comment, criticism and/or donation is welcome.
//
// Please contact any member of the team:
//
// - The frog master (Eric Boix)       eboix@ens-lyon.fr
// - The duck master (Alex Gouaillard) gouaillard@creatis.insa-lyon.fr
// - The cow  master (Leonardo Florez) florez@creatis.insa-lyon.fr
// -------------------------------------------------------------------------

#ifndef __itkQuadEdgeMeshLineCell_txx
#define __itkQuadEdgeMeshLineCell_txx

#include "itkCellInterfaceVisitor.h"
#include "itkQuadEdgeMeshLineCell.h"

namespace itk
{

// ---------------------------------------------------------------------
template< class TCellInterface >
QuadEdgeMeshLineCell< TCellInterface >
::QuadEdgeMeshLineCell( bool makeEdge )
{
  m_Identifier = 0;

  if( makeEdge )
    {
    this->MakeEdge();
    }
}

// ---------------------------------------------------------------------
template< class TCellInterface >
QuadEdgeMeshLineCell< TCellInterface >
::~QuadEdgeMeshLineCell()
{
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
  typedef itk::CellInterfaceVisitor< PixelType, CellTraits > IntVis;
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
  return false;
}

// ---------------------------------------------------------------------
template< class TCellInterface >
void
QuadEdgeMeshLineCell< TCellInterface >
::SetPointIds( PointIdConstIterator first )
{
  PointIdConstIterator i = first;
  this->SetOrg( *i );
  i++;
  this->SetDest( *i );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
void 
QuadEdgeMeshLineCell< TCellInterface >
::SetPointIds( PointIdConstIterator first,
               PointIdConstIterator last )
{
  this->SetOrg( *first );
  this->SetDest( *last );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
void 
QuadEdgeMeshLineCell< TCellInterface >
::SetPointId( int localId, PointIdentifier pId )
{
  switch( localId )
    {
    case 0:
      {
      this->SetOrg( pId );
      break;
      }
    case 1:
      {
      this->SetDest( pId );
      break;
      }
    }
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::PointIdIterator
QuadEdgeMeshLineCell< TCellInterface >
::PointIdsBegin()
{
  return this->BeginGeomSym();
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::PointIdIterator
QuadEdgeMeshLineCell< TCellInterface >
::PointIdsEnd()
{
  return this->EndGeomSym();
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::PointIdConstIterator
QuadEdgeMeshLineCell< TCellInterface >
::GetPointIds() const
{
  return this->BeginGeomSym();
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::PointIdConstIterator
QuadEdgeMeshLineCell< TCellInterface >
::PointIdsBegin() const
{
  return this->BeginGeomSym();
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::PointIdConstIterator
QuadEdgeMeshLineCell< TCellInterface >
::PointIdsEnd() const
{
  return this->EndGeomSym();
}

// ---------------------------------------------------------------------
template< class TCellInterface >
void
QuadEdgeMeshLineCell< TCellInterface >
::MakeEdge()
{
  Self *   e2 = new Self( false );
  QEDual * e1 = new QEDual();
  QEDual * e3 = new QEDual();             
  
  this->SetRot( e1 );             
  e1->SetRot( e2 );               
  e2->SetRot( e3 );               
  e3->SetRot( this );             
  
  this->SetOnext( this );         
  e1->SetOnext( e3 );             
  e2->SetOnext( e2 );             
  e3->SetOnext( e1 );             
}

// ---------------------------------------------------------------------
template< class TCellInterface >
void
QuadEdgeMeshLineCell< TCellInterface >
::SetIdent( CellIdentifier cid ) 
{
  this->m_Identifier = cid;
}

// ---------------------------------------------------------------------
template< class TCellInterface >
typename QuadEdgeMeshLineCell< TCellInterface >::CellIdentifier 
QuadEdgeMeshLineCell< TCellInterface >
::GetIdent()
{ 
  return this->m_Identifier;
}


} // end namespace itk

#endif 

