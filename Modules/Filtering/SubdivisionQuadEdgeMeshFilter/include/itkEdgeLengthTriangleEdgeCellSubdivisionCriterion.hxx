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

#ifndef itkEdgeLengthTriangleEdgeCellSubdivisionCriterion_hxx
#define itkEdgeLengthTriangleEdgeCellSubdivisionCriterion_hxx

#include "itkEdgeLengthTriangleEdgeCellSubdivisionCriterion.h"

namespace itk
{
template <typename TMesh>
void
EdgeLengthTriangleEdgeCellSubdivisionCriterion<TMesh>::Compute(MeshType * mesh, SubdivisionCellContainer & edgeList)
{
  edgeList.clear();
  typename MeshType::CellsContainer::ConstPointer edges = mesh->GetEdgeCells();
  if (!edges)
  {
    itkExceptionMacro("<<Input mesh has no edges");
  }

  typename MeshType::CellsContainer::ConstIterator eter = edges->Begin();
  while (eter != edges->End())
  {
    auto * edge = dynamic_cast<typename MeshType::EdgeCellType *>(eter.Value());
    if (edge)
    {
      if (mesh->ComputeEdgeLength(edge->GetQEGeom()) > m_MaximumLength)
      {
        edgeList.push_back(edge->GetQEGeom());
      }
    }
    ++eter;
  }
}

} // namespace itk
#endif
