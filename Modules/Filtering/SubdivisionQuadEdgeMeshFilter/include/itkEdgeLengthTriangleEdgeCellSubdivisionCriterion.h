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

#ifndef itkEdgeLengthTriangleEdgeCellSubdivisionCriterion_h
#define itkEdgeLengthTriangleEdgeCellSubdivisionCriterion_h

#include "itkQuadEdgeMeshSubdivisionCriterion.h"
#include "itkObjectFactory.h"
#include "itkNumericTraits.h"


namespace itk
{
/**
 *\class EdgeLengthTriangleEdgeCellSubdivisionCriterion
 *\brief
 *\ingroup SubdivisionQuadEdgeMeshFilter
 */
template <typename TSubdivisionFilter>
class EdgeLengthTriangleEdgeCellSubdivisionCriterion : public QuadEdgeMeshSubdivisionCriterion<TSubdivisionFilter>
{
public:
  using Self = EdgeLengthTriangleEdgeCellSubdivisionCriterion;
  using Superclass = QuadEdgeMeshSubdivisionCriterion<TSubdivisionFilter>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using MeshType = typename Superclass::MeshType;
  using MeshPointer = typename Superclass::MeshPointer;
  using MeshConstPointer = typename Superclass::MeshConstPointer;
  using PointsContainerPointer = typename Superclass::PointsContainerPointer;
  using PointsContainerConstIterator = typename Superclass::PointsContainerConstIterator;
  using PointsContainerIterator = typename Superclass::PointsContainerIterator;
  using CellsContainer = typename Superclass::CellsContainer;
  using CellsContainerPointer = typename Superclass::CellsContainerPointer;
  using CellsContainerIterator = typename Superclass::CellsContainerIterator;
  using CellsContainerConstIterator = typename Superclass::CellsContainerConstIterator;
  using PointType = typename Superclass::PointType;
  using CoordinateType = typename Superclass::CoordinateType;
  using PointIdentifier = typename Superclass::PointIdentifier;
  using CellIdentifier = typename Superclass::CellIdentifier;
  using CellType = typename Superclass::CellType;
  using QEType = typename Superclass::QEType;
  using PointIdIterator = typename Superclass::PointIdIterator;
  using SubdivisionCellContainer = typename Superclass::SubdivisionCellContainer;

  /** Run-time type information (and related methods).   */
  itkOverrideGetNameOfClassMacro(EdgeLengthTriangleEdgeCellSubdivisionCriterion);
  itkNewMacro(Self);

  void
  Compute(MeshType * mesh, SubdivisionCellContainer & edgeList) override;

  itkGetConstMacro(MaximumLength, CoordinateType);
  itkSetMacro(MaximumLength, CoordinateType);

protected:
  EdgeLengthTriangleEdgeCellSubdivisionCriterion() { m_MaximumLength = NumericTraits<CoordinateType>::max(); }
  ~EdgeLengthTriangleEdgeCellSubdivisionCriterion() override = default;

private:
  CoordinateType m_MaximumLength;
};

} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkEdgeLengthTriangleEdgeCellSubdivisionCriterion.hxx"
#endif

#endif
