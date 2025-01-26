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

#ifndef itkCellAreaTriangleCellSubdivisionCriterion_h
#define itkCellAreaTriangleCellSubdivisionCriterion_h

#include "itkQuadEdgeMeshSubdivisionCriterion.h"
#include "itkObjectFactory.h"
#include "itkTriangleHelper.h"
#include "itkNumericTraits.h"


namespace itk
{
/**
 *\class CellAreaTriangleCellSubdivisionCriterion
 *\brief
 *\ingroup SubdivisionQuadEdgeMeshFilter
 */
template <typename TSubdivisionFilter>
class CellAreaTriangleCellSubdivisionCriterion : public QuadEdgeMeshSubdivisionCriterion<TSubdivisionFilter>
{
public:
  using Self = CellAreaTriangleCellSubdivisionCriterion;
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
  itkOverrideGetNameOfClassMacro(CellAreaTriangleCellSubdivisionCriterion);
  itkNewMacro(Self);

  void
  Compute(MeshType * mesh, SubdivisionCellContainer & cellIds) override;

  itkGetConstMacro(MaximumArea, CoordinateType);
  itkSetMacro(MaximumArea, CoordinateType);

protected:
  CellAreaTriangleCellSubdivisionCriterion() { m_MaximumArea = NumericTraits<CoordinateType>::max(); }
  ~CellAreaTriangleCellSubdivisionCriterion() override = default;

private:
  CoordinateType m_MaximumArea;
};

} // namespace itk
#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkCellAreaTriangleCellSubdivisionCriterion.hxx"
#endif

#endif
