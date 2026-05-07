/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#ifndef itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilter_hxx
#define itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilter_hxx


namespace itk
{
template <typename TInputMesh, typename TOutputMesh>
TriangleEdgeCellSubdivisionQuadEdgeMeshFilter<TInputMesh,
                                              TOutputMesh>::TriangleEdgeCellSubdivisionQuadEdgeMeshFilter() = default;

template <typename TInputMesh, typename TOutputMesh>
void
TriangleEdgeCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::SetCellsToBeSubdivided(
  const SubdivisionCellContainer & EdgesList)
{
  this->m_EdgesToBeSubdivided = EdgesList;
  this->Modified();
}

template <typename TInputMesh, typename TOutputMesh>
void
TriangleEdgeCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::AddSubdividedEdge(InputQEType * edge)
{
  this->m_EdgesToBeSubdivided.push_back(edge);
  this->Modified();
}

template <typename TInputMesh, typename TOutputMesh>
void
TriangleEdgeCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::GenerateOutputPoints()
{
  // 1. Copy points from input to output
  this->CopyInputMeshToOutputMeshPoints();

  // 2. Initialize edgePoints container
  this->m_EdgesPointIdentifier->Initialize();

  this->m_Uniform = this->m_EdgesToBeSubdivided.empty();

  if (this->m_Uniform)
  {
    typename InputCellsContainer::ConstPointer edges = this->GetInput()->GetEdgeCells();
    if (!edges)
    {
      itkExceptionMacro("<<Input mesh has no edges");
    }

    typename InputCellsContainer::ConstIterator eter = edges->Begin();
    while (eter != edges->End())
    {
      auto * edge = dynamic_cast<typename InputMeshType::EdgeCellType *>(eter.Value());
      if (edge)
      {
        this->AddNewEdgePoints(edge->GetQEGeom());
      }
      ++eter;
    }
  }
  else
  {
    SubdivisionCellContainerConstIterator it = this->m_EdgesToBeSubdivided.begin();
    SubdivisionCellContainerConstIterator end = this->m_EdgesToBeSubdivided.end();

    while (it != end)
    {
      if (*it)
      {
        this->AddNewEdgePoints(*it);
      }
      ++it;
    }
  }
}

template <typename TInputMesh, typename TOutputMesh>
void
TriangleEdgeCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>::PrintSelf(std::ostream & os,
                                                                                  Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // namespace itk
#endif
