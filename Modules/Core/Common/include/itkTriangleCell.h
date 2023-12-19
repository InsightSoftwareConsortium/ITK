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
#ifndef itkTriangleCell_h
#define itkTriangleCell_h

#include "itkLineCell.h"
#include "itkTriangleCellTopology.h"
#include "itkMakeFilled.h"

#include <array>

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
  ITK_DISALLOW_COPY_AND_MOVE(TriangleCell);

  /** Standard class type aliases. */
  itkCellCommonTypedefs(TriangleCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkOverrideGetNameOfClassMacro(TriangleCell);

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

  // Standard CellInterface

  /** Implement the standard CellInterface. */
  CellGeometryEnum
  GetType() const override
  {
    return CellGeometryEnum::TRIANGLE_CELL;
  }
  void
  MakeCopy(CellAutoPointer &) const override;

  /** Get the topological dimension of this cell. */
  unsigned int
  GetDimension() const override;

  /** Get the number of points required to define the cell. */
  unsigned int
  GetNumberOfPoints() const override;

  /** Get the number of boundary features of the given dimension. */
  CellFeatureCount
  GetNumberOfBoundaryFeatures(int dimension) const override;

  /** Get the boundary feature of the given dimension specified by the given cell feature Id.
   * The Id can range from 0 to GetNumberOfBoundaryFeatures(dimension)-1.
   */
  bool
  GetBoundaryFeature(int dimension, CellFeatureIdentifier, CellAutoPointer &) override;

  /** Set the point id list used by the cell.  It is assumed that the given iterator can be incremented and safely
   * de-referenced enough times to get all the point ids needed by the cell.
   */
  void
  SetPointIds(PointIdConstIterator first) override;

  /** Set the point id list used by the cell.  It is assumed that the range of iterators [first, last) contains the
   * correct number of points needed to define the cell.  The position *last is NOT referenced, so it can safely be
   * one beyond the end of an array or other container.
   */
  void
  SetPointIds(PointIdConstIterator first, PointIdConstIterator last) override;

  /** Set an individual point identifier in the cell. */
  void
  SetPointId(int localId, PointIdentifier) override;

  /** Get a begin iterator to the list of point identifiers used by the cell. */
  PointIdIterator
  PointIdsBegin() override;

  /** Get a const begin iterator to the list of point identifiers used by the cell. */
  PointIdConstIterator
  PointIdsBegin() const override;

  /** Get an end iterator to the list of point identifiers used by the cell. */
  PointIdIterator
  PointIdsEnd() override;

  /** Get a const end iterator to the list of point identifiers used by the cell. */
  PointIdConstIterator
  PointIdsEnd() const override;

  // Triangle-specific interface

  /** Get the number of vertices defining the triangle. */
  virtual CellFeatureCount
  GetNumberOfVertices() const;

  /** Get the number of edges defined for the triangle. */
  virtual CellFeatureCount
  GetNumberOfEdges() const;

  /** Get the vertex specified by the given cell feature Id.
   * The Id can range from 0 to GetNumberOfVertices()-1.
   */
  virtual bool
  GetVertex(CellFeatureIdentifier, VertexAutoPointer &);

  /** Get the edge specified by the given cell feature Id.
   * The Id can range from 0 to GetNumberOfEdges()-1.
   */
  virtual bool
  GetEdge(CellFeatureIdentifier, EdgeAutoPointer &);

  /** Evaluate the position of a given point inside the cell */
  bool
  EvaluatePosition(CoordRepType *,
                   PointsContainer *,
                   CoordRepType *,
                   CoordRepType[],
                   double *,
                   InterpolationWeightType *) override;

  /** Cell visitor interface. */
  itkCellVisitMacro(CellGeometryEnum::TRIANGLE_CELL);

  /** Compute Area to a TriangleCell given a PointsContainer. */
  CoordRepType
  ComputeArea(PointsContainer *);

  PointType
  ComputeBarycenter(CoordRepType *, PointsContainer *);

  PointType
  ComputeCenterOfGravity(PointsContainer *);

  PointType
  ComputeCircumCenter(PointsContainer *);

public:
  TriangleCell() = default;
#if defined(__GNUC__)
  // A bug in some versions of the GCC and Clang compilers
  // result in an ICE or linker error when "= default" is requested.
  // This was observed in at least gcc 4.8 and 5.4.0, and
  // AppleClang 7.0.2 and 8.0.0. Probably others too.
  // "= default" doesn't gain us much, so just don't use it here.
  ~TriangleCell() override{};
#else
  ~TriangleCell() override = default;
#endif

protected:
  /** Store the number of points needed for a triangle. */
  std::array<PointIdentifier, NumberOfPoints> m_PointIds{ MakeFilled<std::array<PointIdentifier, NumberOfPoints>>(
    NumericTraits<PointIdentifier>::max()) };

private:
  /** Compute the squared distance between a point and a line segment defined by two other points. Returns the
   * parametric coordinate t and point location on line. */
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
