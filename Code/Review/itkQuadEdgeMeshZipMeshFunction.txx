// -------------------------------------------------------------------------
// itkQuadEdgeMeshZipMeshFunction.txx
// $Revision: 1.2 $
// $Author: hanfei $
// $Name:  $
// $Date: 2007-08-02 23:38:12 $
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
#ifndef __itkQuadEdgeMeshZipMeshFunction_txx
#define __itkQuadEdgeMeshZipMeshFunction_txx

namespace itk
{

template < class TMesh, class TQEType >
typename QuadEdgeMeshZipMeshFunction< TMesh, TQEType >::OutputType
QuadEdgeMeshZipMeshFunction< TMesh, TQEType >::
Evaluate( QEType* e )
{
  if( !this->m_Mesh )
    {
    itkDebugMacro( "No mesh present." );
    return( QEType::m_NoPoint );
    }

  if( e->IsLeftSet( ) )
    {
    itkDebugMacro( "Incoming edge must be adjacent to NOFACE." );
    return( QEType::m_NoPoint );
    }
  //     Initial state                          Final state        //
  //                                                               // 
  //   |               |                         \       /         //
  //   |               |                          \     /          //
  //   |               |                           \   /           //
  //   |               |                            \ /            //
  //   VTrashed      Vkept                           V             //
  //    \             /                              |             //
  //     \           /                               |             //
  //      \         ^                                |             //
  //       \       /                                 |             //
  //        \     e                                  |             //
  //         \   /                                   |             //
  //          \ /                                    |             //
  //  ------- Org --------                --------- Org ---------  //
  //         / | \                                 / | \           //
  //        /  |  \                               /  |  \          //
  //       /   |   \                             /   |   \         //
  //
  // This is the original situation:
  //
  //         \                        /                            //
  //          \                      ^                             //
  //           \                    a                              //
  //            \                  /                               //
  //    ------ VRite            VLeft --------                     //
  //            / \             / \                                //
  //           /   \           /   \                               //
  //          /     b         ^     \                              //
  //         /       \       /       \                             //
  //        /         v     e   Rite  \                            //
  //       /           \   /    Face   \                           //
  //      /             \ /             \                          //
  //     V ------------ Org ------------ VOpposite                 //
  //                   / | \                                       //
  //                  /  |  \                                      //
  //
  // Store for latter usage (since e and e->GetRight() will be deleted):
  QEType* a = e->GetLnext( );
  QEType* b = e->GetOnext( )->GetSym( );
  OutputType VLeft = e->GetDestination( );
  OutputType VRite = b->GetOrigin( );
  bool wasFacePresent = e->IsRightSet( );

  // Delete the Edge e and it's right face:
  if( wasFacePresent )
    {
    this->m_Mesh->DeleteFace( e->GetRight( ) );
    }
  this->m_Mesh->DeleteEdge( e );

  // We should be cautious and consider the case when the very
  // initial situation was the following:
  //               \       /                               //
  //                \     /                                //
  //                 \   /                                 //
  //                  \ /                                  //
  //             VRite = VLeft ------- VOpposite           //
  //                  / \            _/                    //
  //                 /   \         _/                      //
  //                /     \   *  _/                        //
  //                |     |    _/                          //
  //                \     /  _/                            //
  //                 \   / _/                              //
  //                  \ /_/                                //
  //        --------- Org ---------                        //
  //                 / | \                                 //
  //                /  |  \                                //
  //               /   |   \                               //
  //
  // in which case the current situation is the following:
  //
  //               \       /                               //
  //                \     /                                //
  //                 \   /                                 //
  //                  \ /                                  //
  //             VRite = VLeft ------- VOpposite           //
  //                  /              _/                    //
  //                 /             _/                      //
  //                /            _/                        //
  //                |          _/                          //
  //                \        _/                            //
  //                 \     _/                              //
  //                  \  _/                                //
  //        --------- Org ---------                        //
  //                 / | \                                 //
  //                /  |  \                                //
  //               /   |   \                               //
  //
  // and hence the connectivity part of "Zip" job is allready done.
  // Check for that case:
  // 
  OutputType resultingPointId = QEType::m_NoPoint;
  if( VLeft != VRite )
    {
    // We are now left with the following situation
    //
    //         \                        /                  //
    //          \                      ^                   //
    //           \                    a                    //
    //            \                  /                     //
    //    ----- VRite              VLeft --------          //
    //            / \               \                      //
    //           /   \               \                     //
    //          /     b               \                    //
    //         /       \               \                   //
    //        /         v               \                  //
    //       /           \               \                 //
    //      /             \               \                //
    //     V ------------ Org ------------ VOpposite       //
    //                   / | \                             //
    //                  /  |  \                            //
    //
    // where we just miss a simple Mesh::Splice() to obtain::
    //
    //          \     /                                    //
    //           \   /                                     //
    //            \ /                                      //
    //  ---- VRite = VLeft ------ VOpposite                //
    //            / \             /                        //
    //           /   \           /                         //
    //          /     b         /                          //
    //         /       \       /                           //
    //        /         v     /                            //
    //       /           \   /                             //
    //      /             \ /                              //
    //     V ------------ Org ------------                 //
    //                   / | \                             //
    //                  /  |  \                            //
    //
    resultingPointId = this->m_Mesh->Splice( a, b );
    }

  // We restore the deleted face (when it was present):
  if( wasFacePresent )
    {
    this->m_Mesh->AddFace( b );
    }

  this->m_Mesh->Modified( );
  return( resultingPointId );
}

} // namespace itk

#endif

// eof - itkQuadEdgeMeshZipMeshFunction.txx
