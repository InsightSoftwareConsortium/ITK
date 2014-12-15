/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkQuadEdgeMeshZipMeshFunction_hxx
#define itkQuadEdgeMeshZipMeshFunction_hxx

#include "itkQuadEdgeMeshZipMeshFunction.h"

namespace itk
{
template< typename TMesh, typename TQEType >
typename QuadEdgeMeshZipMeshFunction< TMesh, TQEType >::OutputType
QuadEdgeMeshZipMeshFunction< TMesh, TQEType >::Evaluate(QEType *e)
{
  if ( !this->m_Mesh )
    {
    itkDebugMacro("No mesh present.");
    return ( QEType::m_NoPoint );
    }

  if ( e->IsLeftSet() )
    {
    itkDebugMacro("Incoming edge must be adjacent to NOFACE.");
    return ( QEType::m_NoPoint );
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
  QEType *   a = e->GetLnext();
  QEType *   b = e->GetOnext()->GetSym();
  OutputType VLeft = e->GetDestination();
  OutputType VRite = b->GetOrigin();
  bool       wasFacePresent = e->IsRightSet();
  OutputType resultingPointId = QEType::m_NoPoint;

  // We should be cautious and consider the case when the very
  // initial situation was the following:
  //                                                       //
  //                   *                                   //
  //        *                     *                        //
  //                                                       //
  //             VRite = VLeft                             //
  //                  / \                                  //
  //                 /   \                                 //
  //                /     \                                //
  //       *        |  *  |        *                       //
  //                \     /                                //
  //                 \   /                                 //
  //                  \ /                                  //
  //        --------- Org ---------                        //
  //                 / | \                                 //
  //                /  |  \                                //
  //               /   |   \                               //
  if ( VRite == VLeft )
    {
    if ( e->IsWire() && b->IsWire() )
      {
      this->m_Mesh->LightWeightDeleteEdge(e);
      this->m_Mesh->LightWeightDeleteEdge(b);
      return ( resultingPointId );
      }
    }

  // Delete the Edge e and it's right face:
  if ( wasFacePresent )
    {
    this->m_Mesh->DeleteFace( e->GetRight() );
    }
  this->m_Mesh->LightWeightDeleteEdge(e);

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
  // and hence the connectivity part of "Zip" job is already done.
  // Check for that case:
  //
  if ( VLeft != VRite )
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
    resultingPointId = this->m_Mesh->Splice(a, b);
    }

  // We restore the deleted face (when it was present):
  if ( wasFacePresent )
    {
    this->m_Mesh->AddFace(b);
    }

  this->m_Mesh->Modified();
  return ( resultingPointId );
}
} // namespace itk

#endif

// eof - itkQuadEdgeMeshZipMeshFunction.hxx
