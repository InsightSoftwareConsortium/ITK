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
#ifndef itkQuadEdgeMeshEulerOperatorSplitVertexFunction_hxx
#define itkQuadEdgeMeshEulerOperatorSplitVertexFunction_hxx

#include "itkQuadEdgeMeshEulerOperatorSplitVertexFunction.h"

namespace itk
{
template< typename TMesh, typename TQEType >
typename QuadEdgeMeshEulerOperatorSplitVertexFunction< TMesh, TQEType >::OutputType
QuadEdgeMeshEulerOperatorSplitVertexFunction< TMesh, TQEType >::Evaluate(QEType *h, QEType *g)
{
  if ( !this->m_Mesh )
    {
    itkDebugMacro("No mesh present.");
    return ( (QEType *)ITK_NULLPTR );
    }

  if ( ( h == (QEType *)ITK_NULLPTR ) || ( g == (QEType *)ITK_NULLPTR ) )
    {
    itkDebugMacro("One or more argument(s) is(are) null.");
    return ( (QEType *)ITK_NULLPTR );
    }

  if ( h == g )
    {
    itkDebugMacro("The two half-edges are the same. No antenna allowed.");
    return ( (QEType *)ITK_NULLPTR );
    }

  if ( h->GetDestination() != g->GetDestination() )
    {
    itkDebugMacro("The two half-edges must be incident to the same vertex.");
    return ( (QEType *)ITK_NULLPTR );
    }

  // delete the faces
  this->m_Mesh->DeleteFace( h->GetRight() );
  this->m_Mesh->DeleteFace( g->GetRight() );

  // splice to create a new point and disconnect the rings
  this->m_NewPoint = this->m_Mesh->Splice( h->GetSym(), g->GetSym() );

  // then add a new edge
  QEType *ReturnedEdge = this->m_Mesh->AddEdge( g->GetDestination(),
                                                h->GetDestination() );

  // Build two new faces
  this->m_Mesh->AddFace( h->GetSym() );
  this->m_Mesh->AddFace( g->GetSym() );
  this->m_Mesh->Modified();

  return ( ReturnedEdge );
}
} // namespace itk

#endif

// eof - itkQuadEdgeMeshEulerOperatorSplitVertexFunction.hxx
