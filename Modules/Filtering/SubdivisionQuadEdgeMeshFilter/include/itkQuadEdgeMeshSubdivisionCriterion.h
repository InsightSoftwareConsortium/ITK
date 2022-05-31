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

#ifndef itkQuadEdgeMeshSubdivisionCriterion_h
#define itkQuadEdgeMeshSubdivisionCriterion_h

#include "itkObject.h"

namespace itk
{
/**
 *\class QuadEdgeMeshSubdivisionCriterion
 *\brief
 *\ingroup SubdivisionQuadEdgeMeshFilter
 */
template <typename TCellSubdivisionFilter>
class ITK_EXPORT QuadEdgeMeshSubdivisionCriterion : public Object
{
public:
  using Self = QuadEdgeMeshSubdivisionCriterion;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using MeshType = typename TCellSubdivisionFilter::InputMeshType;
  using MeshPointer = typename MeshType::Pointer;
  using MeshConstPointer = typename MeshType::ConstPointer;
  using PointsContainerPointer = typename MeshType::PointsContainerPointer;
  using PointsContainerConstIterator = typename MeshType::PointsContainerConstIterator;
  using PointsContainerIterator = typename MeshType::PointsContainerIterator;
  using CellsContainer = typename MeshType::CellsContainer;
  using CellsContainerPointer = typename MeshType::CellsContainerPointer;
  using CellsContainerIterator = typename MeshType::CellsContainerIterator;
  using CellsContainerConstIterator = typename MeshType::CellsContainerConstIterator;
  using PointType = typename MeshType::PointType;
  using CoordRepType = typename MeshType::CoordRepType;
  using PointIdentifier = typename MeshType::PointIdentifier;
  using CellIdentifier = typename MeshType::CellIdentifier;
  using CellType = typename MeshType::CellType;
  using QEType = typename MeshType::QEType;
  using PointIdIterator = typename MeshType::PointIdIterator;
  using SubdivisionCellContainer = typename TCellSubdivisionFilter::SubdivisionCellContainer;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(QuadEdgeMeshSubdivisionCriterion, Object);

  virtual void
  Compute(MeshType * mesh, SubdivisionCellContainer & edgeList) = 0;

protected:
  QuadEdgeMeshSubdivisionCriterion() = default;
  ~QuadEdgeMeshSubdivisionCriterion() override = default;
};

} // namespace itk
#endif
