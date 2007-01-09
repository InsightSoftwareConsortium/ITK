// -------------------------------------------------------------------------
// itkQEEulerOperatorJoinFacetFunction.txx
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
// - The duck master (Alex Gouaillard) alexandre.gouaillard@sun.com
// - The cow  master (Leonardo Florez) florez@creatis.insa-lyon.fr
// -------------------------------------------------------------------------
#ifndef __ITKQUADEDGEMESH__ITKQEEULEROPERATORJOINFACETFUNCTION__TXX__
#define __ITKQUADEDGEMESH__ITKQEEULEROPERATORJOINFACETFUNCTION__TXX__

#include "itkQEMesh.h"  // Just to mark the dependance towards this class.

namespace itkQE
{

template < class TMesh, class TQEType >
  typename EulerOperatorJoinFacetFunction< TMesh, TQEType >::OutputType
  EulerOperatorJoinFacetFunction< TMesh, TQEType >::
  Evaluate( QEType* e )
{
   if( !this->m_Mesh )
   {
      itkDebugMacro( "No mesh present." );
      return( (QEType*) 0 );
   }

   if(  !e->IsInternal( ) )
   {
      itkDebugMacro( "The edge is either border or wire." );
      return( (QEType*) 0 );
   }

   /*     Initial state                           Final state
    *
    *          Dest                                 Dest
    *          /|\                                  /  \
    *         / | \                                /    \
    *        /  |  \                              /      \
    *       /   |   \                            /        \
    *      /    |    \                          /          \
    *     /     |     \                        /            \
    *    /      |      \                      /              \
    *   /       ^       \                    /                \
    *  o  F1    |  F2    o                  o    new Face      o
    *   \       e       /                    \                /
    *    \      |      /                      \              /
    *     ^     |     /                     returned e      /
    *      \    |    /                          \          /
    *     oNext |   /                            v        /
    *        \  |  /                              \      /
    *         \ | /                                \    /
    *          \|/                                  \  /
    *          Org                                  Org
    */
   QEType* return_e = e->GetOnext( )->GetSym( );
   
   // delete the edge and the two associated faces
   this->m_Mesh->LightWeightDeleteEdge( e );
   
   // Build a new face in replacement of the one we deleted:
   this->m_Mesh->AddFace( return_e );
   this->m_Mesh->Modified( );
   return( return_e );
}

} // namespace itkQE

#endif // __ITKQUADEDGEMESH__ITKQEEULEROPERATORJOINFACETFUNCTION__TXX__

// eof - itkQEEulerOperatorJoinFacetFunction.txx

