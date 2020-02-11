/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction_hxx
#define itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction_hxx

#include "itkQuadEdgeMeshEulerOperatorCreateCenterVertexFunction.h"

namespace itk
{
template <typename TMesh, typename TQEType>
typename QuadEdgeMeshEulerOperatorCreateCenterVertexFunction<TMesh, TQEType>::OutputType
QuadEdgeMeshEulerOperatorCreateCenterVertexFunction<TMesh, TQEType>::Evaluate(QEType * e)
{
  // Is there any input ?
#ifndef NDEBUG
  if (!e)
  {
    itkDebugMacro("Input is not an edge.");
    return ((QEType *)nullptr);
  }

  if (!this->m_Mesh)
  {
    itkDebugMacro("No mesh present.");
    return ((OutputType) nullptr);
  }

  // Is left face set ?
  if (!e->IsLeftSet())
  {
    itkDebugMacro("Argument edge has no left face.");
    return ((OutputType) nullptr);
  }
#endif

  // remove left face
  this->m_Mesh->DeleteFace(e->GetLeft());

  // create new point geometry
  unsigned int sum = 0;
  VectorType   vec;
  vec.Fill(0);
  PointIdentifier pid = this->m_Mesh->FindFirstUnusedPointIndex();
  using AssociatedBarycenters = std::map<QEType *, PointIdentifier>;
  AssociatedBarycenters m_AssocBary;
  using QEIterator = typename QEType::IteratorGeom;
  QEIterator lit = e->BeginGeomLnext();
  while (lit != e->EndGeomLnext())
  {
    QEType * g = lit.Value();
    vec += this->m_Mesh->GetVector(g->GetOrigin());
    sum++;
    m_AssocBary[g] = pid;
    lit++;
  } // rof
  vec /= CoordRepType(sum);
  PointType p;
  for (unsigned int i = 0; i < 3; i++)
  {
    p[i] = vec[i];
  }

  // add new point to mesh
  this->m_NewPointID = this->m_Mesh->AddPoint(p);
  PointIdentifier tempPoint;

  // create edges and faces
  tempPoint = e->GetDestination();
  this->m_Mesh->AddFaceTriangle(this->m_NewPointID, e->GetOrigin(), tempPoint);
  QEType * edgeRef = this->m_Mesh->FindEdge(this->m_NewPointID, tempPoint);
  while (!edgeRef->IsLeftSet())
  {
    tempPoint = edgeRef->GetLnext()->GetDestination();
    this->m_Mesh->AddFaceTriangle(this->m_NewPointID, edgeRef->GetLnext()->GetOrigin(), tempPoint);
    edgeRef = this->m_Mesh->FindEdge(this->m_NewPointID, tempPoint);
  }

  return (e->GetLnext());
}

} // end namespace itk

#endif
