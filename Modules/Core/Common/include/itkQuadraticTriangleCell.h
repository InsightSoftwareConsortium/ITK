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
#ifndef itkQuadraticTriangleCell_h
#define itkQuadraticTriangleCell_h

#include "itkQuadraticEdgeCell.h"
#include "itkQuadraticTriangleCellTopology.h"
#include "itkMakeFilled.h"

#include <array>

namespace itk
{
/** \class QuadraticTriangleCell
 *  \brief Represents a second order triangular patch for a Mesh.
 *
 * \ingroup MeshObjects
 * \ingroup ITKCommon
 */
template <typename TCellInterface>
class ITK_TEMPLATE_EXPORT QuadraticTriangleCell
  : public TCellInterface
  , private QuadraticTriangleCellTopology
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(QuadraticTriangleCell);

  /** Standard class type aliases. */
  itkCellCommonTypedefs(QuadraticTriangleCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(QuadraticTriangleCell);

  /** The type of boundary for this triangle's vertices. */
  using VertexType = VertexCell<TCellInterface>;
  using VertexAutoPointer = typename VertexType::SelfAutoPointer;

  /** The type of boundary for this triangle's edges. */
  using EdgeType = QuadraticEdgeCell<TCellInterface>;
  using EdgeAutoPointer = typename EdgeType::SelfAutoPointer;

  /** Triangle-specific topology numbers. */
  static constexpr unsigned int NumberOfPoints = 6;
  static constexpr unsigned int NumberOfVertices = 3;
  static constexpr unsigned int NumberOfEdges = 3;
  static constexpr unsigned int CellDimension = 2;

  /** Implement the standard CellInterface. */
  CellGeometryEnum
  GetType() const override
  {
    return CellGeometryEnum::QUADRATIC_TRIANGLE_CELL;
  }
  void
  MakeCopy(CellAutoPointer &) const override;

  unsigned int
  GetDimension() const override;

  unsigned int
  GetNumberOfPoints() const override;

  CellFeatureCount
  GetNumberOfBoundaryFeatures(int dimension) const override;

  bool
  GetBoundaryFeature(int dimension, CellFeatureIdentifier, CellAutoPointer &) override;
  void
  SetPointIds(PointIdConstIterator first) override;

  void
  SetPointIds(PointIdConstIterator first, PointIdConstIterator last) override;

  void
  SetPointId(int localId, PointIdentifier) override;
  PointIdIterator
  PointIdsBegin() override;

  PointIdConstIterator
  PointIdsBegin() const override;

  PointIdIterator
  PointIdsEnd() override;

  PointIdConstIterator
  PointIdsEnd() const override;

  /** Triangle-specific interface. */
  virtual CellFeatureCount
  GetNumberOfVertices() const;

  virtual CellFeatureCount
  GetNumberOfEdges() const;

  virtual bool
  GetVertex(CellFeatureIdentifier, VertexAutoPointer &);
  virtual bool
  GetEdge(CellFeatureIdentifier, EdgeAutoPointer &);

  /** Cell visitor interface. */
  itkCellVisitMacro(CellGeometryEnum::QUADRATIC_TRIANGLE_CELL);

  /** Given the parametric coordinates of a point in the cell
   *  determine the value of its Shape Functions
   *  returned through an itkArray<InterpolationWeightType>).  */
  void
  EvaluateShapeFunctions(const ParametricCoordArrayType & parametricCoordinates,
                         ShapeFunctionsArrayType &        weights) const override;

public:
  QuadraticTriangleCell() = default;

  ~QuadraticTriangleCell() override = default;

protected:
  /** Store the number of points needed for a triangle. */
  std::array<PointIdentifier, NumberOfPoints> m_PointIds{ MakeFilled<std::array<PointIdentifier, NumberOfPoints>>(
    NumericTraits<PointIdentifier>::max()) };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkQuadraticTriangleCell.hxx"
#endif

#endif
