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
#ifndef itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction_hxx
#define itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction_hxx

#include "itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.h"

namespace itk
{
template< typename TMesh, typename TQEType >
typename QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction< TMesh, TQEType >::OutputType
QuadEdgeMeshEulerOperatorDeleteCenterVertexFunction< TMesh, TQEType >::Evaluate(QEType *g)
{
  if ( !g )
    {
    itkDebugMacro("Input is not an edge.");
    return ( (QEType *)ITK_NULLPTR );
    }

  if ( !this->m_Mesh )
    {
    itkDebugMacro("No mesh present.");
    return ( (QEType *)ITK_NULLPTR );
    }

  if ( !g->IsInternal() )
    {
    itkDebugMacro("The edge is either border or wire.");
    return ( (QEType *)ITK_NULLPTR );
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
      return ( (QEType *)ITK_NULLPTR );
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
    return ( (QEType *)ITK_NULLPTR );
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

// eof - itkQuadEdgeMeshEulerOperatorDeleteCenterVertexFunction.hxx
