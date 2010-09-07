/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction_txx
#define __itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction_txx

#include "itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.h"

namespace itk
{
template< class TMesh, class TQEType >
typename QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction< TMesh, TQEType >::OutputType
QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction< TMesh, TQEType >::Evaluate(QEType *g)
{
  if ( !g )
    {
    itkDebugMacro("Input is not an edge.");
    return ( (QEType *)0 );
    }

  if ( !this->m_Mesh )
    {
    itkDebugMacro("No mesh present.");
    return ( (QEType *)0 );
    }

  if ( !g->IsInternal() )
    {
    itkDebugMacro("The edge is either border or wire.");
    return ( (QEType *)0 );
    }

  // None of the incident facets of g->GetDestination() is a hole.

  //one-ring
  std::vector< PointIdentifier > pList;
  QEType *                       g_sym = g->GetSym();
  typedef typename QEType::IteratorGeom QEIterator;
  for (  QEIterator it = g_sym->BeginGeomOnext();
         it != g_sym->EndGeomOnext();
         it++ )
    {
    QEType *one_edge = it.Value();
    if ( !one_edge->IsInternal() )
      {
      itkDebugMacro("DeleteVertex requires a full one-ring, i.e. no holes.");
      return ( (QEType *)0 );
      }
    pList.push_back( one_edge->GetDestination() );
    }

  // Condition: There are at least two distinct facets incident to the facets
  // that are incident to g->GetDestination().(This prevents the operation
  // from collapsing a volume into two facets glued together with opposite
  // orientations, such as would happen with any vertex of a tetrahedron.)
  PointIdentifier PointId1, PointId2;
  PointId1 = pList.back();
  pList.pop_back();
  PointId2 = pList.back();
  pList.pop_back();
  FaceRefType FirstFace = this->m_Mesh->FindEdge(PointId1,
                                                 PointId2)->GetLeft();
  bool SecondFaceFound = false;
  while ( ( pList.size() ) && ( !SecondFaceFound ) )
    {
    PointId1 = PointId2;
    PointId2 = pList.back();
    pList.pop_back();
    if ( this->m_Mesh->FindEdge(PointId1,
                                PointId2)->GetLeft() != FirstFace )
      {
      SecondFaceFound = true;
      }
    }
  if ( !SecondFaceFound )
    {
    itkDebugMacro(
      "DeleteVertex requires at least two distinct \
    facets incident to the facets that are incident to g->GetDestination()."                                                     );
    return ( (QEType *)0 );
    }

  // let's do the job now.
  QEType *h = g->GetLprev();
  QEType *temp;
  this->m_OldPointID = g->GetDestination();
  this->m_Mesh->LightWeightDeleteEdge(g);
  g = h->GetLnext();
  while ( g != h )
    {
    while ( ( g->GetDestination() != this->m_OldPointID ) && ( g != h ) )
      {
      g = g->GetLnext();
      }
    if ( g != h )
      {
      temp = g->GetLprev();
      this->m_Mesh->LightWeightDeleteEdge(g);
      g = temp;
      }
    }
  this->m_Mesh->AddFace(h);

  return ( h );
}
} // namespace itk

#endif

// eof - itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.txx
