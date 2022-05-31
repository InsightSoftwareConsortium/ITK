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

#ifndef itkConditionalSubdivisionQuadEdgeMeshFilter_h
#define itkConditionalSubdivisionQuadEdgeMeshFilter_h

#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"
#include "itkQuadEdgeMeshSubdivisionCriterion.h"
#include "itkConceptChecking.h"

namespace itk
{
/**
 * \class ConditionalSubdivisionQuadEdgeMeshFilter
 *
 * \brief FIXME
 * \ingroup SubdivisionQuadEdgeMeshFilter
 */
template <typename TInputMesh, typename TSubdivisionFilter>
class ConditionalSubdivisionQuadEdgeMeshFilter
  : public QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, typename TSubdivisionFilter::OutputMeshType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ConditionalSubdivisionQuadEdgeMeshFilter);

  using Self = ConditionalSubdivisionQuadEdgeMeshFilter;
  using Superclass = QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, typename TSubdivisionFilter::OutputMeshType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using SubdivisionFilterType = TSubdivisionFilter;
  using SubdivisionFilterPointer = typename SubdivisionFilterType::Pointer;

  using InputMeshType = TInputMesh;
  using InputMeshPointer = typename InputMeshType::Pointer;

  using OutputMeshType = typename SubdivisionFilterType::OutputMeshType;
  using OutputMeshPointer = typename OutputMeshType::Pointer;

  using SubdivisionCellContainer = typename SubdivisionFilterType::SubdivisionCellContainer;

  using CriterionType = QuadEdgeMeshSubdivisionCriterion<SubdivisionFilterType>;
  using CriterionPointer = typename CriterionType::Pointer;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(ConditionalSubdivisionQuadEdgeMeshFilter, QuadEdgeMeshToQuadEdgeMeshFilter);
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro(
    SameTypeCheck,
    (Concept::SameType<typename SubdivisionFilterType::InputMeshType, typename SubdivisionFilterType::OutputMeshType>));
  itkConceptMacro(
    SameTypeCheckMesh,
    (Concept::SameType<typename SubdivisionFilterType::OutputMeshType, typename CriterionType::MeshType>));
  itkConceptMacro(SameTypeCheckContainer,
                  (Concept::SameType<typename SubdivisionFilterType::SubdivisionCellContainer,
                                     typename CriterionType::SubdivisionCellContainer>));
#endif

  void
  SetSubdivisionCriterion(CriterionType * criterion);

protected:
  ConditionalSubdivisionQuadEdgeMeshFilter();

  ~ConditionalSubdivisionQuadEdgeMeshFilter() override = default;

  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  SubdivisionFilterPointer m_SubdivisionFilter;
  SubdivisionCellContainer m_CellsToBeSubdivided;
  CriterionPointer         m_SubdivisionCriterion;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkConditionalSubdivisionQuadEdgeMeshFilter.hxx"
#endif

#endif
