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

#ifndef itkIterativeTriangleCellSubdivisionQuadEdgeMeshFilter_hxx
#define itkIterativeTriangleCellSubdivisionQuadEdgeMeshFilter_hxx

#include "itkIterativeTriangleCellSubdivisionQuadEdgeMeshFilter.h"

namespace itk
{
template <typename TInputMesh, typename TCellSubdivisionFilter>
IterativeTriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TCellSubdivisionFilter>::
  IterativeTriangleCellSubdivisionQuadEdgeMeshFilter()
{
  this->m_CellSubdivisionFilter = CellSubdivisionFilterType::New();
  this->m_ResolutionLevels = 1;
}

template <typename TInputMesh, typename TCellSubdivisionFilter>
void
IterativeTriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TCellSubdivisionFilter>::SetCellsToBeSubdivided(
  const SubdivisionCellContainer & cellIdList)
{
  this->m_CellSubdivisionFilter->SetCellsToBeSubdivided(cellIdList);
  this->Modified();
}

template <typename TInputMesh, typename TCellSubdivisionFilter>
void
IterativeTriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TCellSubdivisionFilter>::AddSubdividedCellId(
  OutputCellIdentifier cellId)
{
  this->m_CellsToBeSubdivided.push_back(cellId);
  this->Modified();
}

template <typename TInputMesh, typename TCellSubdivisionFilter>
void
IterativeTriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TCellSubdivisionFilter>::GenerateData()
{
  this->CopyInputMeshToOutputMeshGeometry();

  unsigned int resolution = this->m_ResolutionLevels;
  while (resolution != 0)
  {
    this->m_CellSubdivisionFilter->SetInput(this->GetOutput());
    this->m_CellSubdivisionFilter->Update();
    OutputMeshPointer mesh = this->m_CellSubdivisionFilter->GetOutput();
    mesh->DisconnectPipeline();
    this->GraftOutput(mesh);
    --resolution;
  }
}

template <typename TInputMesh, typename TCellSubdivisionFilter>
void
IterativeTriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TCellSubdivisionFilter>::PrintSelf(std::ostream & os,
                                                                                                  Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  std::cout << indent << "Subdivision Resolution Levels: " << m_ResolutionLevels << std::endl;
}
} // namespace itk
#endif
