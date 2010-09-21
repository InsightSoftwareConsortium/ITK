/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshEulerOperatorSplitFacetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshEulerOperatorSplitFacetFunction_txx
#define __itkQuadEdgeMeshEulerOperatorSplitFacetFunction_txx

#include "itkQuadEdgeMeshEulerOperatorSplitFacetFunction.h"

namespace itk
{
template< class TMesh, class TQEType >
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
    return ( (QEType *)0 );
    }

  if ( !this->m_Mesh )
    {
    itkDebugMacro("No mesh present.");
    return ( (QEType *)0 );
    }

  if ( h == g )
    {
    itkDebugMacro("Provided edges should be different.");
    return ( (QEType *)0 );
    }

  if ( h->GetLeft() != g->GetLeft() )
    {
    itkDebugMacro("The edges are not around the same face.");
    return ( (QEType *)0 );
    }

  if ( ( h->GetLnext() == g )
       || ( g->GetLnext() == h ) )
    {
    itkDebugMacro("Provided edges should NOT be consecutive.");
    return ( (QEType *)0 );
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

// eof - itkQuadEdgeMeshEulerOperatorSplitFacetFunction.txx
