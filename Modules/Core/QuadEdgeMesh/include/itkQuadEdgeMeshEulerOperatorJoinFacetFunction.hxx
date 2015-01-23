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
#ifndef itkQuadEdgeMeshEulerOperatorJoinFacetFunction_hxx
#define itkQuadEdgeMeshEulerOperatorJoinFacetFunction_hxx

#include "itkQuadEdgeMeshEulerOperatorJoinFacetFunction.h"

namespace itk
{
template< typename TMesh, typename TQEType >
typename QuadEdgeMeshEulerOperatorJoinFacetFunction< TMesh, TQEType >::OutputType
QuadEdgeMeshEulerOperatorJoinFacetFunction< TMesh, TQEType >::Evaluate(QEType *e)
{
#ifndef NDEBUG
  if ( !e )
    {
    itkDebugMacro("Input is not an edge.");
    return ( (QEType *)0 );
    }

  if ( !this->m_Mesh )
    {
    itkDebugMacro("No mesh present.");
    return ( (QEType *)0 );
    }
#endif

  if ( !e->IsInternal() )
    {
    itkDebugMacro("The edge is either border or wire.");
    return ( (QEType *)ITK_NULLPTR );
    }

  //     Initial state                           Final state        //
  //
  //          Dest                                 Dest             //
  //          /|\                                  /  \             //
  //         / | \                                /    \            //
  //        /  |  \                              /      \           //
  //       /   |   \                            /        \          //
  //      /    |    \                          /          \         //
  //     /     |     \                        /            \        //
  //    /      |      \                      /              \       //
  //   /       ^       \                    /                \      //
  //  o  F1    |  F2    o                  o    new Face      o     //
  //   \       e       /                    \                /      //
  //    \      |      /                      \              /       //
  //     ^     |     /                     returned e      /        //
  //      \    |    /                          \          /         //
  //     oNext |   /                            v        /          //
  //        \  |  /                              \      /           //
  //         \ | /                                \    /            //
  //          \|/                                  \  /             //
  //          Org                                  Org              //
  //
  QEType *return_e = e->GetOnext()->GetSym();

  // delete the edge and the two associated faces
  this->m_Mesh->LightWeightDeleteEdge(e);

  // Build a new face in replacement of the one we deleted:
  this->m_Mesh->AddFace(return_e);
  this->m_Mesh->Modified();
  return ( return_e );
}
}

#endif

// eof - itkQuadEdgeMeshEulerOperatorJoinFacetFunction.hxx
