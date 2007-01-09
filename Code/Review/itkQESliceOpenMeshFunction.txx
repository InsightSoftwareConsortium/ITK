// -------------------------------------------------------------------------
// itkQESliceOpenMeshFunction.txx
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
#ifndef __ITKQUADEDGEMESH__ITKQESLICEOPENMESHFUNCTION__TXX__
#define __ITKQUADEDGEMESH__ITKQESLICEOPENMESHFUNCTION__TXX__

#include "itkQEMesh.h"  // Just to mark the dependance towards this class.

namespace itkQE
{

template < class TMesh, class TQEType >
  typename SliceOpenMeshFunction< TMesh, TQEType >::OutputType
  SliceOpenMeshFunction< TMesh, TQEType >::
  Evaluate( QEType* e )
{
   if( !this->m_Mesh )
   {
      itkDebugMacro( "No mesh present." );
      return( (QEType*) 0 );
   }

   if( !e->IsInternal( ) )
   {
      itkDebugMacro( "Both faces must be set." );
      return( (QEType*) 0 );
   }

   if( !e->GetSym( )->IsOrgInternal( ) )
   {
      itkDebugMacro( "Dest() is not an internal point." );
   }

   /*     Initial state                           Final state
    *
    *          Dest                                  Dest
    *          /|\                                  / /\ \
    *         / | \                                / /  \ \
    *        /  |  \                              /  |  |  \
    *       /   |  dPrev                         /  /    \  \
    *      /    |    \                          /   |    |   \
    *     /     |     v                        /    | N  |    \
    *    /      |      \                      /    /  O   \    \
    *   /       ^       \                    /     |      |     \
    *   V  F1   |  F2   V                    V     e  F  new    V
    *   \       e       /                    \     |  A   |     /
    *    \      |      /                      \    \  C   /    /
    *     \     |     ^                        \    | E  |    /
    *      \    |    /                          \   |    |   /
    *       \   |  oPrev                         \  \    /  /
    *        \  |  /                              \  |  |  /
    *         \ | /                                \ \  / /
    *          \|/                                  \ \/ /
    *          Org                                   Org
    */
   typedef typename MeshType::VertexRefType   VertexRefType;
   typedef typename MeshType::EdgeCellType    EdgeCellType;
   typedef typename MeshType::CellIdentifier  CellIdentifier;
   typedef typename MeshType::CellAutoPointer CellAutoPointer;
   this->m_Mesh->DeleteFace( e->GetRight( ) );
   VertexRefType orgPid  = e->GetOrg( );
   VertexRefType destPid = e->GetDest( );

   // Create an new isolated edge and set it's geometry:
   EdgeCellType* newEdge = new EdgeCellType( true );
   // see the code of e.g. AddFace
   newEdge->SetOrg (  orgPid );
   newEdge->SetDest( destPid );

   // Insert newEdge at Org
   QEType* oPrev = e->GetOprev( );
   oPrev->InsertAfterNextBorderEdgeWithUnsetLeft( newEdge );
   // Insert newEdge at Dest
   QEType* dPrev = e->GetSym()->GetOprev( );
   dPrev->InsertAfterNextBorderEdgeWithUnsetLeft(newEdge->GetSym());

   // Add the new edge to the container
   this->m_Mesh->PushOnContainer( newEdge );

   // Build a new face in replacement of the one we deleted:
   this->m_Mesh->AddFace( oPrev );
   this->m_Mesh->Modified( );
   return( newEdge );
}

} // namespace itkQE

#endif // __ITKQUADEDGEMESH__ITKQESLICEOPENMESHFUNCTION__TXX__

// eof - itkQESliceOpenMeshFunction.txx

