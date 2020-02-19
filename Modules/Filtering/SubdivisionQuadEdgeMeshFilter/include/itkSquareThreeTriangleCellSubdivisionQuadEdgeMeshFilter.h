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

#ifndef itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter_h
#define itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter_h

#include "itkTriangleCellSubdivisionQuadEdgeMeshFilter.h"

namespace itk
{
/**
 * \class SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter
 *
 * \brief FIXME     Add documentation here
 * \ingroup SubdivisionQuadEdgeMeshFilter
 */
template <class TInputMesh, class TOutputMesh>
class SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter
  : public TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter);

  using Self = SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter;
  using Superclass = TriangleCellSubdivisionQuadEdgeMeshFilter<TInputMesh, TOutputMesh>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using InputMeshType = typename Superclass::InputMeshType;
  using InputMeshPointer = typename Superclass::InputMeshPointer;
  using InputMeshConstPointer = typename Superclass::InputMeshConstPointer;
  using InputPointsContainerPointer = typename Superclass::InputPointsContainerPointer;
  using InputPointsContainerIterator = typename Superclass::InputPointsContainerIterator;
  using InputCellsContainer = typename Superclass::InputCellsContainer;
  using InputCellsContainerPointer = typename Superclass::InputCellsContainerPointer;
  using InputCellsContainerIterator = typename Superclass::InputCellsContainerIterator;
  using InputCellsContainerConstIterator = typename Superclass::InputCellsContainerConstIterator;
  using InputPointType = typename Superclass::InputPointType;
  using InputVectorType = typename Superclass::InputVectorType;
  using InputCoordType = typename Superclass::InputCoordType;
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
  using OutputCellsContainerPointer = typename Superclass::OutputCellsContainerPointer;
  using OutputCellsContainerIterator = typename Superclass::OutputCellsContainerIterator;
  using OutputPointType = typename Superclass::OutputPointType;
  using OutputVectorType = typename Superclass::OutputVectorType;
  using OutputCoordType = typename Superclass::OutputCoordType;
  using OutputPointIdentifier = typename Superclass::OutputPointIdentifier;
  using OutputCellIdentifier = typename Superclass::OutputCellIdentifier;
  using OutputCellType = typename Superclass::OutputCellType;
  using OutputQEType = typename Superclass::OutputQEType;
  using OutputMeshTraits = typename Superclass::OutputMeshTraits;
  using OutputPointIdIterator = typename Superclass::OutputPointIdIterator;

  using EdgePointIdentifierContainer = typename Superclass::EdgePointIdentifierContainer;
  using EdgePointIdentifierContainerPointer = typename Superclass::EdgePointIdentifierContainerPointer;
  using EdgePointIdentifierContainerIterator = typename Superclass::EdgePointIdentifierContainerIterator;
  using EdgePointIdentifierContainerConstIterator = typename Superclass::EdgePointIdentifierContainerConstIterator;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter, TriangleCellSubdivisionQuadEdgeMeshFilter);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

protected:
  SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter() = default;
  ~SquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter() override = default;

  void
  AddNewCellPoints(InputCellType * cell) override;
  void
  GenerateOutputCells() override;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSquareThreeTriangleCellSubdivisionQuadEdgeMeshFilter.hxx"
#endif

#endif
