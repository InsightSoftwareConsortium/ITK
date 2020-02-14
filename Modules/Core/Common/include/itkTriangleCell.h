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
#ifndef itkTriangleCell_h
#define itkTriangleCell_h

#include "itkLineCell.h"
#include "itkTriangleCellTopology.h"

#include <vector>

namespace itk
{
/** \class TriangleCell
 * TriangleCell represents a triangle for a Mesh.
 *
 * Template parameters for TriangleCell:
 *
 * TPixelType =
 *     The type associated with a point, cell, or boundary for use in storing
 *     its data.
 *
 * TCellTraits =
 *     Type information of mesh containing cell.
 *
 * \ingroup MeshObjects
 * \ingroup ITKCommon
 */

template <typename TCellInterface>
class ITK_TEMPLATE_EXPORT TriangleCell
  : public TCellInterface
  , private TriangleCellTopology
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(TriangleCell);

  /** Standard class type aliases. */
  itkCellCommonTypedefs(TriangleCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(TriangleCell, CellInterface);

  /** The type of boundary for this triangle's vertices. */
  using VertexType = VertexCell<TCellInterface>;
  using VertexAutoPointer = typename VertexType::SelfAutoPointer;

  /** The type of boundary for this triangle's edges. */
  using EdgeType = LineCell<TCellInterface>;
  using EdgeAutoPointer = typename EdgeType::SelfAutoPointer;

  /** Triangle-specific topology numbers. */
  static constexpr unsigned int NumberOfPoints = 3;
  static constexpr unsigned int NumberOfVertices = 3;
  static constexpr unsigned int NumberOfEdges = 3;
  static constexpr unsigned int CellDimension = 2;

  /** Implement the standard CellInterface. */
  CellGeometryEnum
  GetType() const override
  {
    return CellGeometryEnum::TRIANGLE_CELL;
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

  bool
  EvaluatePosition(CoordRepType *,
                   PointsContainer *,
                   CoordRepType *,
                   CoordRepType[],
                   double *,
                   InterpolationWeightType *) override;

  /** Cell visitor interface. */
  itkCellVisitMacro(CellGeometryEnum::TRIANGLE_CELL);

  /** \brief Compute Area to a TriangleCell given a PointsContainer.  */
  CoordRepType
  ComputeArea(PointsContainer *);

  PointType
  ComputeBarycenter(CoordRepType *, PointsContainer *);

  PointType
  ComputeCenterOfGravity(PointsContainer *);

  PointType
  ComputeCircumCenter(PointsContainer *);

public:
  TriangleCell()
    : m_PointIds(NumberOfPoints, NumericTraits<PointIdentifier>::max())
  {}
#if defined(__GNUC__) && (__GNUC__ > 5) || defined(__clang__)
  ~TriangleCell() override = default;
#else
  ~TriangleCell() override{};
#endif

protected:
  /** Store the number of points needed for a triangle. */
  std::vector<PointIdentifier> m_PointIds;

private:
  /** Computes the SQUARED distance between a point and a line segment defined
   * by two other points */
  double
  DistanceToLine(PointType x, PointType p1, PointType p2, double & t, CoordRepType * closestPoint);

  double
  DistanceToLine(PointType x, PointType p1, PointType p2, double & t, PointType & closestPoint);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTriangleCell.hxx"
#endif

#endif
