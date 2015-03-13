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

#ifndef itkConditionalSubdivisionQuadEdgeMeshFilter_hxx
#define itkConditionalSubdivisionQuadEdgeMeshFilter_hxx

#include "itkConditionalSubdivisionQuadEdgeMeshFilter.h"

namespace itk
{
template <typename TInputMesh, typename TSubdivisionFilter>
ConditionalSubdivisionQuadEdgeMeshFilter<TInputMesh, TSubdivisionFilter>::ConditionalSubdivisionQuadEdgeMeshFilter()
{
  this->m_SubdivisionFilter = SubdivisionFilterType::New();
}

template <typename TInputMesh, typename TSubdivisionFilter>
void
ConditionalSubdivisionQuadEdgeMeshFilter<TInputMesh, TSubdivisionFilter>::SetSubdivisionCriterion(
  CriterionType * criterion)
{
  this->m_SubdivisionCriterion = criterion;
  this->Modified();
}

template <typename TInputMesh, typename TSubdivisionFilter>
void
ConditionalSubdivisionQuadEdgeMeshFilter<TInputMesh, TSubdivisionFilter>::GenerateData()
{
  this->CopyInputMeshToOutputMeshGeometry();
  this->m_SubdivisionCriterion->Compute(this->GetOutput(), this->m_CellsToBeSubdivided);

  while (!this->m_CellsToBeSubdivided.empty())
  {
    this->m_SubdivisionFilter->SetInput(this->GetOutput());
    this->m_SubdivisionFilter->SetCellsToBeSubdivided(this->m_CellsToBeSubdivided);
    this->m_SubdivisionFilter->Update();
    OutputMeshPointer mesh = this->m_SubdivisionFilter->GetOutput();
    mesh->DisconnectPipeline();
    this->GraftOutput(mesh);
    this->m_SubdivisionCriterion->Compute(this->GetOutput(), this->m_CellsToBeSubdivided);
  }
}

template <typename TInputMesh, typename TSubdivisionFilter>
void
ConditionalSubdivisionQuadEdgeMeshFilter<TInputMesh, TSubdivisionFilter>::PrintSelf(std::ostream & os,
                                                                                    Indent         indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // namespace itk
#endif
