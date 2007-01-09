// -------------------------------------------------------------------------
// itkQEPoint.txx
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

#ifndef __ITKQUADEDGEMESH__POINT__TXX__
#define __ITKQUADEDGEMESH__POINT__TXX__

#include "itkQEBaseIterator.h"

namespace itkQE
{

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename QEType >
    void Point< TCoordRep, VPointDimension, QEType >::
    Initialise( )
{
    m_Edge = (QEType*)0;
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename QEType >
    Point< TCoordRep, VPointDimension, QEType >::
    Point( )
        : Superclass( )
{
    this->Initialise( );
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename QEType >
    Point< TCoordRep, VPointDimension, QEType >::
    Point( const Self& r )
        : Superclass( r )
{
    this->Initialise( );
    m_Edge = r.m_Edge;
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename QEType >
    Point< TCoordRep, VPointDimension, QEType >::
    Point( const ValueType r[ PointDimension ] )
        : Superclass( r )
{
    this->Initialise( );
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename QEType >
    Point< TCoordRep, VPointDimension, QEType >::
    Point( const VectorType& vec )
        : Superclass( )
{
    this->operator=( vec );
    this->Initialise( );
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename QEType >
    typename Point< TCoordRep, VPointDimension, QEType >::
    Self&
    Point< TCoordRep, VPointDimension, QEType >::
    operator=( const Self& r )
{
    this->Superclass::operator=( r );
    m_Edge = r.m_Edge;
    return( *this );
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename QEType >
    typename Point< TCoordRep, VPointDimension, QEType >::
    Self&
    Point< TCoordRep, VPointDimension, QEType >::
    operator=( const Superclass& r )
{
    this->Superclass::operator=( r );
    this->Initialise( );
    return( *this );
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename QEType >
    typename Point< TCoordRep, VPointDimension, QEType >::
    Self&
    Point< TCoordRep, VPointDimension, QEType >::
    operator=( const ValueType r[ PointDimension ] )
{
    this->Superclass::operator=( r );
    this->Initialise( );
    return( *this );
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename QEType >
    typename Point< TCoordRep, VPointDimension, QEType >::
    Self&
    Point< TCoordRep, VPointDimension, QEType >::
    operator=( const VectorType& vec )
{
    this->BaseArray::operator=( vec );
    this->Initialise( );
    return( *this );
}

template< class TCoordRep, unsigned int VPointDimension, typename QEType >
    bool Point< TCoordRep, VPointDimension, QEType >::
    IsInternal( )
{
   return( this->GetEdge( )->IsOrgInternal( ) );
}

/** Return the valence of this Point i.e. the number of edges constituting
 *  the Onext ring to which this point belongs.
 *  @return the valence when an entry is the Onext ring is present,
 *          and -1 otherwise.
 */
template< class TCoordRep, unsigned int VPointDimension, typename QEType >
    int Point< TCoordRep, VPointDimension, QEType >::
    GetValence( )
{
   if( ! this->GetEdge( ) )
   {
      return( -1 );
   }
   return( this->GetEdge( )->GetOrder( ) );
}

} // fnamespace

#endif // __ITKQUADEDGEMESH__POINT__TXX__

// eof - itkQEPoint.txx
