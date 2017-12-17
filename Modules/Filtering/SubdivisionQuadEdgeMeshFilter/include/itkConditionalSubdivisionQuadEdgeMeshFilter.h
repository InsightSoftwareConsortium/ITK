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
  typedef ConditionalSubdivisionQuadEdgeMeshFilter                                                  Self;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, typename TSubdivisionFilter::OutputMeshType> Superclass;
  typedef SmartPointer<Self>                                                                        Pointer;
  typedef SmartPointer<const Self>                                                                  ConstPointer;

  typedef TSubdivisionFilter                      SubdivisionFilterType;
  typedef typename SubdivisionFilterType::Pointer SubdivisionFilterPointer;

  typedef TInputMesh                      InputMeshType;
  typedef typename InputMeshType::Pointer InputMeshPointer;

  typedef typename SubdivisionFilterType::OutputMeshType OutputMeshType;
  typedef typename OutputMeshType::Pointer               OutputMeshPointer;

  typedef typename SubdivisionFilterType::SubdivisionCellContainer SubdivisionCellContainer;

  typedef QuadEdgeMeshSubdivisionCriterion<SubdivisionFilterType> CriterionType;
  typedef typename CriterionType::Pointer                         CriterionPointer;

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

  ~ConditionalSubdivisionQuadEdgeMeshFilter() ITK_OVERRIDE {}

  void
  GenerateData() ITK_OVERRIDE;

  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  SubdivisionFilterPointer m_SubdivisionFilter;
  SubdivisionCellContainer m_CellsToBeSubdivided;
  CriterionPointer         m_SubdivisionCriterion;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ConditionalSubdivisionQuadEdgeMeshFilter);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkConditionalSubdivisionQuadEdgeMeshFilter.hxx"
#endif

#endif
