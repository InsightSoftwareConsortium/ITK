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
#include <algorithm>

namespace itk
{
template <typename TInputMesh, typename TCellSubdivisionFilter>
IterativeTriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TCellSubdivisionFilter>::
  IterativeTriangleCellSubdivisionQuadEdgeMeshFilter()
{
  this->m_ResolutionLevels = 1;
}

template <typename TInputMesh, typename TCellSubdivisionFilter>
void
IterativeTriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TCellSubdivisionFilter>::SetCellsToBeSubdivided(
  const SubdivisionCellContainer & cellIdList)
{
  this->m_CellsToBeSubdivided = cellIdList;
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
  this->m_CellSubdivisionFilterVector.clear();
  const auto resolution = std::max(static_cast<unsigned int>(1), this->m_ResolutionLevels);
  for (size_t i = 0; i < resolution; ++i)
  {
    this->m_CellSubdivisionFilterVector.push_back(TCellSubdivisionFilter::New());
  }
  this->m_CellSubdivisionFilterVector.at(0)->SetInput(this->GetInput());
  this->m_CellSubdivisionFilterVector.at(0)->SetCellsToBeSubdivided(this->m_CellsToBeSubdivided);
  this->m_CellSubdivisionFilterVector.at(0)->Update();
  this->GraftOutput(this->m_CellSubdivisionFilterVector.at(0)->GetOutput());
  for (size_t i = 1; i < resolution; ++i)
  {
    this->m_CellSubdivisionFilterVector.at(i)->SetInput(this->GetOutput());
    this->m_CellSubdivisionFilterVector.at(i)->SetCellsToBeSubdivided(
      this->m_CellSubdivisionFilterVector.at(i - 1)->GetCellsToBeSubdivided());
    this->m_CellSubdivisionFilterVector.at(i)->Update();
    this->GraftOutput(this->m_CellSubdivisionFilterVector.at(i)->GetOutput());
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
