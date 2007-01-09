// -------------------------------------------------------------------------
// itkQELineCell.txx
// $Revision: 1.1 $
// $Author: sylvain $
// $Name:  $
// $Date: 2007-01-09 00:58:17 $
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

#ifndef __ITKQUADEDGEMESH__LINECELL__TXX__
#define __ITKQUADEDGEMESH__LINECELL__TXX__

#include <itkCellInterfaceVisitor.h>

namespace itkQE
{
// ---------------------------------------------------------------------
template< class TCellInterface >
    LineCell< TCellInterface >::
    LineCell( bool makeEdge )
        : Superclass( ), QEType( ),
          m_Ident( 0 )
{
    if( makeEdge )
        this->MakeEdge( );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    typename LineCell< TCellInterface >::SelfAutoPointer
    LineCell< TCellInterface >::
    New( )
{
    SelfAutoPointer ptr( new Self );
    ptr.TakeOwnership( );
    return( ptr );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    void LineCell< TCellInterface >::
    Accept( unsigned long cellId, MultiVisitor* mv )
{
    typedef itk::CellInterfaceVisitor< PixelType, CellTraits > IntVis;
    typename IntVis::Pointer v = mv->GetVisitor( this->GetType( ) );
    if( v ) v->VisitFromCell( cellId, this );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    typename LineCell< TCellInterface >::CellFeatureCount
    LineCell< TCellInterface >::
    GetNumberOfBoundaryFeatures( int dimension ) const
{
    if( dimension == 0 )      return( 2 );
    else if( dimension == 1 ) return( 1 );
    else                      return( 0 );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    bool LineCell< TCellInterface >::
    GetBoundaryFeature( int dimension, CellFeatureIdentifier cellId,
                        CellAutoPointer& cell )
{
    // TODO
    (void)dimension;
    (void)cellId;
    (void)cell;
    return( false );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    void LineCell< TCellInterface >::
    SetPointIds( PointIdConstIterator first )
{
    PointIdConstIterator i = first;
    this->SetOrg( *i );
    i++;
    this->SetDest( *i );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    void LineCell< TCellInterface >::
    SetPointIds( PointIdConstIterator first,
                 PointIdConstIterator last )
{
  (void)last;
    this->SetPointIds( first );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    void LineCell< TCellInterface >::
    SetPointId( int localId, PointIdentifier pId )
{
    if( localId == 0 )      this->SetOrg( pId );
    else if( localId == 1 ) this->SetDest( pId );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    typename LineCell< TCellInterface >::PointIdIterator
    LineCell< TCellInterface >::
    PointIdsBegin( )
{
    return( this->BeginGeomSym( ) );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    typename LineCell< TCellInterface >::PointIdIterator
    LineCell< TCellInterface >::
    PointIdsEnd( )
{
    return( this->EndGeomSym( ) );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    typename LineCell< TCellInterface >::PointIdConstIterator
    LineCell< TCellInterface >::
    GetPointIds( ) const
{
    return( this->BeginGeomSym( ) );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    typename LineCell< TCellInterface >::PointIdConstIterator
    LineCell< TCellInterface >::
    PointIdsBegin( ) const
{
    return( this->BeginGeomSym( ) );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    typename LineCell< TCellInterface >::PointIdConstIterator
    LineCell< TCellInterface >::
    PointIdsEnd( ) const
{
    return( this->EndGeomSym( ) );
}

} // enamespace

#endif // __ITKQUADEDGEMESH__LINECELL__TXX__

// eof - itkQELineCell.txx
