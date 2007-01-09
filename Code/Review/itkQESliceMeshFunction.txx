// -------------------------------------------------------------------------
// itkQESliceMeshFunction.txx
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
#ifndef __ITKQUADEDGEMESH__ITKQESLICEMESHFUNCTION__TXX__
#define __ITKQUADEDGEMESH__ITKQESLICEMESHFUNCTION__TXX__

#include "itkQEMesh.h"  // Just to mark the dependance towards this class.
#include "itkQESliceOpenMeshFunction.h"
#include "itkQESliceAtPointOrgMeshFunction.h"

namespace itkQE
{

/**
 * Assumption is made that the Org() of the edge argument is on the
 * boundary of the Mesh.
 */
template < class TMesh, class TQEType, class TOutput >
  typename SliceMeshFunction< TMesh >::OutputType
  SliceMeshFunction< TMesh >::Evaluate( QEType* e )
{
   //
   //     Initial state                           Final state
   //
   //          Dest                                  Dest
   //          /|\                                  / /\ \
   //         / | \                                / /  \ \
   //        /  |  \                              /  |  |  \
   //       /   |   \                            /  /    \  \
   //      /    |    \                          /   |    ^   \
   //     /     |     \                        /    |   new   \
   //    /      |      \                      /    /      \    \
   //   /       ^       \                    /     |      |     \
   //   V  F1   |  F2   V                    V     e      |     V
   //   \       e       /                    | F1  |      | Fnew|
   //    \      |      /                     |    /        \    |      /
   //     \     |     /   F4                 |    |   N    |    | F4  /
   //      \    |    /      __               |   /    O     \   |    /
   //       \   |   /    __/                 |   |          |   |   /
   //        \  |  /  __/             F3     |  /     F      \  |  /
   //   F3    \ | /__/    F5                 |  |     A      |  | /  F5
   //          \|//                          | /      C       \ |/
   // ---------Org------------       -------Org       E      newOrg------
   //
   //         NO FACE
   //
   // In the process we created a new edge, duplicated Org to a new
   // point newOrg (but with the same geometrical coordinates) and
   // replaced F2 by a newly constructed face Fnew

   // Check the assumption:
   if( e->IsOrgInternal( ) )
   {
       itkDebugMacro( "Org() of incoming edge must be on boundary." );
       return false;
   }

   typedef SliceOpenMeshFunction< OutputMeshType, QEType > SliceOpen;
   typename SliceOpen::Pointer sliceOpen = SliceOpen::New( );
   sliceOpen->SetInput( this->m_Mesh );

   QEPrimal* newEdge = sliceOpen->Evaluate( e );

   // The situation should now be the following (the * denotes a new
   // face constructed by SpliceOpen() in remplacement of the deleted F2).
   //
   //            Dest
   //           / /\ \
   //          / /  \ \
   //         /  |  |  \
   //        /  /    \  \
   //       /   |    |   \
   //      /    | N  |    \
   //     /    /  O   \    \
   //    /     |      |     \
   //    V     e  F  new    V
   //    \ F1  |  A   |  *  /
   //     \    \  C   /    /
   //      \    | E  |    /  F4
   //       \   |    |   /      __/
   //        \  \    /  /    __/
   //         \  |  |  /  __/
   //   F3     \ \  / /__/     F5
   //           \ \/ //
   // ---------<-Org----------------
   //
   // We are left with Slicing at Org:
   typedef SliceAtPointOrgMeshFunction< OutputMeshType, QEType >
                                                            SliceAtPointOrg;
   typename SliceAtPointOrg::Pointer sliceAtPointOrg = SliceAtPointOrg::New( );
   sliceAtPointOrgFunction->SetInput( this->m_Mesh );
   if( ! sliceAtPointOrg->Evaluate()( newEdge ) )
   {
      itkWarningMacro( "Slicing at resulting point was impossible." );
   }
   this->m_Mesh->Modified( );
   return true;
}

} // namespace itkQE

#endif // __ITKQUADEDGEMESH__ITKQESLICEMESHFUNCTION__TXX__

// eof - itkQESliceMeshFunction.txx

