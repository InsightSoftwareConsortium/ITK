// -------------------------------------------------------------------------
// itkQuadEdgeMeshPoint.txx
// $Revision: 1.1 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-16 22:30:06 $
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

#ifndef __itkQuadEdgeMeshPoint_txx
#define __itkQuadEdgeMeshPoint_txx

#include "itkQuadEdgeMeshPoint.h"

namespace itk
{

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
void
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::Initialize( )
{
  m_Edge = static_cast< TQuadEdge * >( NULL );
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::QuadEdgeMeshPoint()
{
  this->Initialize();
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::QuadEdgeMeshPoint( const Self& r ) : Superclass( r )
{
  this->Initialize();
  m_Edge = r.m_Edge;
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::QuadEdgeMeshPoint( const ValueArrayType & r ) : Superclass( r )
{
  this->Initialize();
}

/*
// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::QuadEdgeMeshPoint( const VectorType& vec ) : Superclass()
{
  this->operator=( vec );
  this->Initialize();
}
*/

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge > &
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::operator=( const Self& r )
{
  this->Superclass::operator=( r );
  m_Edge = r.m_Edge;
  return  *this;
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge > &
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::operator=( const Superclass & r )
{
  this->Superclass::operator=( r );
  this->Initialize();
  return *this;
}

// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge > &
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::operator=( const ValueArrayType & r )
{
  this->Superclass::operator=( r );
  this->Initialize();
  return *this;
}

/*
// ---------------------------------------------------------------------
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge > &
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::operator=( const VectorType& vec )
{
  this->BaseArray::operator=( vec );
  this->Initialize();
  return *this;
}
*/

template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
bool 
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::IsInternal() const
{
   if( this->GetEdge() )
     {
     return this->GetEdge()->IsOrgInternal();
     }
   return false;
}

/** Return the valence of this QuadEdgeMeshPoint i.e. the number of edges constituting
 *  the Onext ring to which this point belongs.
 *  @return the valence when an entry in the Onext ring is present,
 *          and -1 otherwise.
 */
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
int QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::GetValence() const
{
   int valence = -1; // error code by default

   if( this->GetEdge() )
     {
     valence =  this->GetEdge()->GetOrder();
     }

   return valence;
}

/** Set Edge
 *  
 */
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
void
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::SetEdge( const TQuadEdge * inputEdge ) 
{ 
  m_Edge = inputEdge; 
}

/** Get Edge
 *  
 */
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
const TQuadEdge * 
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::GetEdge() const 
{ 
  return  m_Edge; 
}

/** Get Edge non-const version
 *  
 */
template< class TCoordRep, unsigned int VPointDimension, typename TQuadEdge >
const TQuadEdge * 
QuadEdgeMeshPoint< TCoordRep, VPointDimension, TQuadEdge >
::GetEdge()
{ 
  return  m_Edge; 
}


} 

#endif 

// eof - itkQuadEdgeMeshPoint.txx
