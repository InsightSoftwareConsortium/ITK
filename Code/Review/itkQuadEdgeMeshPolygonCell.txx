// -------------------------------------------------------------------------
// itkQuadEdgeMeshPolygonCell.txx
// $Revision: 1.2 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-24 22:52:30 $
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

#ifndef __ITKQUADEDGEMESH__POLYGONCELL__TXX__
#define __ITKQUADEDGEMESH__POLYGONCELL__TXX__

#include <itkCellInterfaceVisitor.h>

namespace itkQE
{
// ---------------------------------------------------------------------
template< class TCellInterface >
    PolygonCell< TCellInterface >::
    PolygonCell( int nPoints )
        : Superclass( ),
          m_Ident( 0 ), m_EdgeRingEntry( 0 )
{
    // Create entry point
    m_EdgeRingEntry = new QEType( );
    m_EdgeRingEntry->MakeEdge( );

    // Create the rest
    QEType* last = m_EdgeRingEntry;
    for( int i = 1; i < nPoints; i++ )
    {
        QEType* edge = new QEType( );
        edge->MakeEdge( );

        edge->Splice( last->GetSym( ) );
        last = edge;
    } // rof

    // Last topological connection, i.e., close the face
    m_EdgeRingEntry->Splice( last->GetSym( ) );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    PolygonCell< TCellInterface >::
    PolygonCell( QEType* e )
        : Superclass( ),
          m_Ident( 0 ), m_EdgeRingEntry( e )
{ }

// ---------------------------------------------------------------------
template< class TCellInterface >
    typename PolygonCell< TCellInterface >::SelfAutoPointer
    PolygonCell< TCellInterface >::
    New( )
{
    SelfAutoPointer ptr( new Self );
    ptr.TakeOwnership( );
    return( ptr );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    void PolygonCell< TCellInterface >::
    Accept( unsigned long cellId, MultiVisitor* mv )
{
    typedef itk::CellInterfaceVisitor< PixelType, CellTraits > IntVis;
    typename IntVis::Pointer v = mv->GetVisitor( this->GetType( ) );
    if( v ) v->VisitFromCell( cellId, this );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    unsigned int PolygonCell< TCellInterface >::
    GetNumberOfPoints( ) const
{
    unsigned int n = 0;
    PointIdConstIterator it = this->PointIdsBegin( );
    for( ; it != this->PointIdsEnd( ); it++, n++ );
    return( n );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    typename PolygonCell< TCellInterface >::CellFeatureCount
    PolygonCell< TCellInterface >::
    GetNumberOfBoundaryFeatures( int dimension ) const
{
    /// \todo
    (void)dimension;
    return( 0 );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    bool PolygonCell< TCellInterface >::
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
    void PolygonCell< TCellInterface >::
    SetPointIds( PointIdConstIterator first )
{
    PointIdIterator i1 = this->PointIdsBegin( );
    PointIdConstIterator i2 = first;
    for( ; i1 != this->PointIdsEnd( ); i1++, i2++ )
        i1.Value( )->SetOrigin( *i2 );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    void PolygonCell< TCellInterface >::
    SetPointIds( PointIdConstIterator first,
                 PointIdConstIterator last )
{
    PointIdIterator i1 = this->PointIdsBegin( );
    PointIdConstIterator i2 = first;
    for( ; i1 != this->PointIdsEnd( ) && i2 != last; i1++, i2++ )
        i1.Value( )->SetOrigin( *i2 );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    void PolygonCell< TCellInterface >::
    SetPointId( int localId, PointIdentifier pId )
{
    int n = 0;
    PointIdIterator it = this->PointIdsBegin( );
    for( ; it != this->PointIdsEnd( ) && n <= localId; it++, n++ )
    {
        if( n == localId )
        {
            it.Value( )->SetOrigin( pId );
            it.Value( )->GetOnext( )->SetOrigin( pId );
        } // fi
    } // rof
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    typename PolygonCell< TCellInterface >::PointIdIterator
    PolygonCell< TCellInterface >::
    PointIdsBegin( )
{
    return( m_EdgeRingEntry->BeginGeomLnext( ) );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    typename PolygonCell< TCellInterface >::PointIdIterator
    PolygonCell< TCellInterface >::
    PointIdsEnd( )
{
    return( m_EdgeRingEntry->EndGeomLnext( ) );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    typename PolygonCell< TCellInterface >::PointIdConstIterator
    PolygonCell< TCellInterface >::
    GetPointIds( ) const
{
   return( const_cast< const QEType* >( m_EdgeRingEntry )-> BeginGeomLnext( ) );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    typename PolygonCell< TCellInterface >::PointIdConstIterator
    PolygonCell< TCellInterface >::
    PointIdsBegin( ) const
{
   return( const_cast< const QEType* >( m_EdgeRingEntry )-> BeginGeomLnext( ) );
}

// ---------------------------------------------------------------------
template< class TCellInterface >
    typename PolygonCell< TCellInterface >::PointIdConstIterator
    PolygonCell< TCellInterface >::
    PointIdsEnd( ) const
{
   return( const_cast< const QEType* >( m_EdgeRingEntry )-> EndGeomLnext( ) );
}

} // enamespace

#endif // __ITKQUADEDGEMESH__POLYGONCELL__TXX__

// eof - itkQuadEdgeMeshPolygonCell.txx
