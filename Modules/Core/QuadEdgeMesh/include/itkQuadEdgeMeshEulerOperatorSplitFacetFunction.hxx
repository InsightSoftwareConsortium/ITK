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
#ifndef itkQuadEdgeMeshEulerOperatorSplitFacetFunction_hxx
#define itkQuadEdgeMeshEulerOperatorSplitFacetFunction_hxx

#include "itkQuadEdgeMeshEulerOperatorSplitFacetFunction.h"

namespace itk
{
template< typename TMesh, typename TQEType >
typename QuadEdgeMeshEulerOperatorSplitFacetFunction< TMesh, TQEType >::OutputType
QuadEdgeMeshEulerOperatorSplitFacetFunction< TMesh, TQEType >::Evaluate(QEType *h, QEType *g)
{
  //
  //  g->Dest() ---<----- X                    destPid  --------- X        //
  //         /     g       \                         / \           \       //
  //        /               \                   dLnext  \           \      //
  //       /                 \                     /     \           \     //
  //      /                   \                   v       ^           \    //
  //     /      g->Left()      \                 /         \           \   //
  //    X           =           X               X        newEdge        X  //
  //     \      h->Left()      /                 \           \         /   //
  //      \                   /                   \           \       ^    //
  //       \                 /                     \           \     /     //
  //        \               /                       \           \ oLnext   //
  //         \       h     /                         \           \ /       //
  //          X ----->--- h->Dest()                   X --------- orgPid   //
  //

  if ( !h || !g )
    {
    itkDebugMacro("At least one of the Input is not an edge.");
    return ( (QEType *)ITK_NULLPTR );
    }

  if ( !this->m_Mesh )
    {
    itkDebugMacro("No mesh present.");
    return ( (QEType *)ITK_NULLPTR );
    }

  if ( h == g )
    {
    itkDebugMacro("Provided edges should be different.");
    return ( (QEType *)ITK_NULLPTR );
    }

  if ( h->GetLeft() != g->GetLeft() )
    {
    itkDebugMacro("The edges are not around the same face.");
    return ( (QEType *)ITK_NULLPTR );
    }

  if ( ( h->GetLnext() == g )
       || ( g->GetLnext() == h ) )
    {
    itkDebugMacro("Provided edges should NOT be consecutive.");
    return ( (QEType *)ITK_NULLPTR );
    }

  typedef typename MeshType::VertexRefType VertexRefType;

  this->m_Mesh->DeleteFace( h->GetLeft() );
  VertexRefType orgPid  = h->GetDestination();
  VertexRefType destPid = g->GetDestination();

  // Create an new isolated edge and set it's geometry:
  EdgeCellType *newEdge = new EdgeCellType;
  QEType *      newEdgeGeom = newEdge->GetQEGeom();

  // see the code of e.g. AddFace
  newEdgeGeom->SetOrigin(orgPid);
  newEdgeGeom->SetDestination(destPid);

  // Insert newEdge at Org
  QEType *oLnext = h->GetLnext();
  oLnext->InsertAfterNextBorderEdgeWithUnsetLeft(newEdgeGeom);
  // Insert newEdge at Dest
  QEType *dLnext = g->GetLnext();
  dLnext->InsertAfterNextBorderEdgeWithUnsetLeft( newEdgeGeom->GetSym() );

  // Add the new edge to the container
  this->m_Mesh->PushOnContainer(newEdge);

  // Build two new faces
  this->m_Mesh->AddFace(h);
  this->m_Mesh->AddFace(g);
  this->m_Mesh->Modified();
  return ( newEdgeGeom );
}
}

#endif

// eof - itkQuadEdgeMeshEulerOperatorSplitFacetFunction.hxx
