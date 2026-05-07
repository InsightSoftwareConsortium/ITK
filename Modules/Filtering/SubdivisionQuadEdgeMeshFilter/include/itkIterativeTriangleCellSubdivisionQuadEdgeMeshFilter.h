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

#ifndef itkIterativeTriangleCellSubdivisionQuadEdgeMeshFilter_h
#define itkIterativeTriangleCellSubdivisionQuadEdgeMeshFilter_h

#include "itkQuadEdgeMeshToQuadEdgeMeshFilter.h"
#include "itkConceptChecking.h"
#include <vector>

namespace itk
{
/**
 * \class IterativeTriangleCellSubdivisionQuadEdgeMeshFilter
 *
 * \brief FIXME
 * \ingroup SubdivisionQuadEdgeMeshFilter
 */
template <typename TInputMesh, typename TCellSubdivisionFilter>
class IterativeTriangleCellSubdivisionQuadEdgeMeshFilter
  : public QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, typename TCellSubdivisionFilter::OutputMeshType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(IterativeTriangleCellSubdivisionQuadEdgeMeshFilter);

  using Self = IterativeTriangleCellSubdivisionQuadEdgeMeshFilter;
  using Superclass = QuadEdgeMeshToQuadEdgeMeshFilter<TInputMesh, typename TCellSubdivisionFilter::OutputMeshType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using CellSubdivisionFilterType = TCellSubdivisionFilter;
  using CellSubdivisionFilterPointer = typename CellSubdivisionFilterType::Pointer;
  using CellSubdivisionFilterPointerVector = typename std::vector<CellSubdivisionFilterPointer>;

  using InputMeshType = TInputMesh;
  using InputMeshPointer = typename InputMeshType::Pointer;

  using OutputMeshType = typename CellSubdivisionFilterType::OutputMeshType;
  using OutputMeshPointer = typename OutputMeshType::Pointer;

  using OutputCellIdentifier = typename CellSubdivisionFilterType::OutputCellIdentifier;
  using SubdivisionCellContainer = typename CellSubdivisionFilterType::SubdivisionCellContainer;
  using SubdivisionCellContainerConstIterator =
    typename CellSubdivisionFilterType::SubdivisionCellContainerConstIterator;

  /** Run-time type information (and related methods).   */
  itkOverrideGetNameOfClassMacro(IterativeTriangleCellSubdivisionQuadEdgeMeshFilter);
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro(SameTypeCheck,
                  (Concept::SameType<typename CellSubdivisionFilterType::InputMeshType,
                                     typename CellSubdivisionFilterType::OutputMeshType>));
#endif

  itkSetMacro(ResolutionLevels, unsigned int);
  itkGetConstMacro(ResolutionLevels, unsigned int);
  void
  SetCellsToBeSubdivided(const SubdivisionCellContainer &);
  itkGetConstReferenceMacro(CellsToBeSubdivided, SubdivisionCellContainer);

  void
  AddSubdividedCellId(OutputCellIdentifier cellId);

protected:
  IterativeTriangleCellSubdivisionQuadEdgeMeshFilter();

  ~IterativeTriangleCellSubdivisionQuadEdgeMeshFilter() override = default;

  void
  GenerateData() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  CellSubdivisionFilterPointerVector m_CellSubdivisionFilterVector;
  SubdivisionCellContainer           m_CellsToBeSubdivided;
  unsigned int                       m_ResolutionLevels;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkIterativeTriangleCellSubdivisionQuadEdgeMeshFilter.hxx"
#endif

#endif
