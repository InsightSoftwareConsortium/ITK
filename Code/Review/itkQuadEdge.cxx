// -------------------------------------------------------------------------
// itkQuadEdge.cxx
// $Revision: 1.1 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-13 12:42:15 $
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

#include "itkQuadEdge.h"

namespace itk
{

// ---------------------------------------------------------------------
QuadEdge::
QuadEdge( ) : m_Onext( 0 ), m_Rot( 0 )
{
}

/**
 * \brief Basic quad-edge topological method.
 *
 * This method describes all possible topological operations on an edge.
 * It is its own inverse. It works in two ways:
 *   1. If this->GetOrg( ) != b->GetOrg( ), it slice a face in two.
 *   2. If this->GetOrg( ) == b->GetOrg( ), it unifies two faces.
 *
 * \warning This class only handles of the connectivity and is not aware
 *    of the geometry that lies at the \ref GeometricalQuadEdge level.
 *    It is strongly discouraged to use this method. Instead you should
 *    use \ref itkQE::Mesh::Splice it's geometry aware version.
 * \sa \ref DoxySurgeryConnectivity
 */
void QuadEdge::Splice( Self* b )
{
  Self* aNext     = this->GetOnext( );
  Self* bNext     = b->GetOnext( );
  Self* alpha     = aNext->GetRot( );
  Self* beta      = bNext->GetRot( );
  Self* alphaNext = alpha->GetOnext( );
  Self* betaNext  = beta->GetOnext( );

  this->SetOnext( bNext );
  b->SetOnext( aNext );
  alpha->SetOnext( betaNext );
  beta->SetOnext( alphaNext );
}

// ---------------------------------------------------------------------
bool QuadEdge::
    IsEdgeInOnextRing( Self* testEdge )
{
  Iterator it = this->BeginOnext( ); 
  for( ; it != this->EndOnext( ); it++ )
    {
    if( this == testEdge )
      {
      return true;
      }
    }
  return false;
}

// ---------------------------------------------------------------------
bool QuadEdge::
    IsLnextGivenSizeCyclic( const int size )
{
  // Verify that when iterating size times with Lnext()
  // we end up on "this": this would prove that the size of Lnext()
  // ring is the given argument.
  Self* iterated = this;

  for( int i = 0; i < size; i++ )
    {
    iterated = iterated->GetLnext( );
    }
  return ( this == iterated );
}

// ---------------------------------------------------------------------
unsigned int QuadEdge::
    GetOrder( ) const
{
  unsigned int order = 0;
  ConstIterator it = this->BeginOnext( ); 
  while( it != this->EndOnext( ) )
    {
    it++;
    order++;
    }
  return order;
}

} 

