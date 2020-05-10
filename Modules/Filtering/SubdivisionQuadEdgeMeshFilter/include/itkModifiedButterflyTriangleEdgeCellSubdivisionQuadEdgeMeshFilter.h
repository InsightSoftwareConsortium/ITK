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

#ifndef itkModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter_h
#define itkModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter_h

#include "itkTriangleEdgeCellSubdivisionQuadEdgeMeshFilter.h"

namespace itk
{
/**
 * \class ModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter
 *
 * \brief FIXME     Add documentation here
 * \ingroup SubdivisionQuadEdgeMeshFilter
 */
template <typename TInputMesh, typename TOutputMesh = TInputMesh>
class ModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter
  : public TriangleEdgeCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter);

  using Self = ModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter;
  using Superclass = TriangleEdgeCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputMeshType = typename Superclass::InputMeshType;
  using InputMeshPointer = typename Superclass::InputMeshPointer;
  using InputMeshConstPointer = typename Superclass::InputMeshConstPointer;
  using InputPointsContainer = typename Superclass::InputPointsContainer;
  using InputPointsContainerPointer = typename Superclass::InputPointsContainerPointer;
  using InputPointsContainerConstIterator = typename Superclass::InputPointsContainerConstIterator;
  using InputPointsContainerIterator = typename Superclass::InputPointsContainerIterator;
  using InputCellsContainer = typename Superclass::InputCellsContainer;
  using InputCellsContainerPointer = typename Superclass::InputCellsContainerPointer;
  using InputCellsContainerIterator = typename Superclass::InputCellsContainerIterator;
  using InputCellsContainerConstIterator = typename Superclass::InputCellsContainerConstIterator;
  using InputPointType = typename Superclass::InputPointType;
  using InputCoordType = typename Superclass::InputCoordRepType;
  using InputPointIdentifier = typename Superclass::InputPointIdentifier;
  using InputCellIdentifier = typename Superclass::InputCellIdentifier;
  using InputCellType = typename Superclass::InputCellType;
  using InputQEType = typename Superclass::InputQEType;
  using InputMeshTraits = typename Superclass::InputMeshTraits;
  using InputPointIdIterator = typename Superclass::InputPointIdIterator;

  using OutputMeshType = typename Superclass::OutputMeshType;
  using OutputMeshPointer = typename Superclass::OutputMeshPointer;
  using OutputPointsContainerPointer = typename Superclass::OutputPointsContainerPointer;
  using OutputPointsContainerIterator = typename Superclass::OutputPointsContainerIterator;
  using OutputPointType = typename Superclass::OutputPointType;
  using OutputVectorType = typename Superclass::OutputVectorType;
  using OutputCoordType = typename Superclass::OutputCoordType;
  using OutputPointIdentifier = typename Superclass::OutputPointIdentifier;
  using OutputCellIdentifier = typename Superclass::OutputCellIdentifier;
  using OutputCellType = typename Superclass::OutputCellType;
  using OutputQEType = typename Superclass::OutputQEType;
  using OutputMeshTraits = typename Superclass::OutputMeshTraits;
  using OutputPointIdIterator = typename Superclass::OutputPointIdIterator;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(ModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter,
               TriangleEdgeCellSubdivisionQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

protected:
  ModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter() = default;
  ~ModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter() override = default;

  void
  AddNewEdgePoints(InputQEType * edge) override;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkModifiedButterflyTriangleEdgeCellSubdivisionQuadEdgeMeshFilter.hxx"
#endif

#endif
