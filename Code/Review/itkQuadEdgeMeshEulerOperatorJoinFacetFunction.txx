/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshEulerOperatorJoinFacetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshEulerOperatorJoinFacetFunction_txx
#define __itkQuadEdgeMeshEulerOperatorJoinFacetFunction_txx

#include "itkQuadEdgeMeshEulerOperatorJoinFacetFunction.h"

namespace itk
{
template< class TMesh, class TQEType >
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
    return ( (QEType *)0 );
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

// eof - itkQuadEdgeMeshEulerOperatorJoinFacetFunction.txx
