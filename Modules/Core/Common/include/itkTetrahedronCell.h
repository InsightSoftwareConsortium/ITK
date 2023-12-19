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
#ifndef itkTetrahedronCell_h
#define itkTetrahedronCell_h

#include "itkTriangleCell.h"
#include "itkTetrahedronCellTopology.h"
#include "itkMakeFilled.h"

#include <array>

namespace itk
{
/** \class TetrahedronCell
 *  \brief TetrahedronCell represents a tetrahedron for a Mesh.
 *
 * \tparam TPixelType The type associated with a point, cell, or boundary
 * for use in storing its data.
 *
 * \tparam TCellTraits Type information of mesh containing cell.
 * \ingroup MeshObjects
 * \ingroup ITKCommon
 */
template <typename TCellInterface>
class ITK_TEMPLATE_EXPORT TetrahedronCell
  : public TCellInterface
  , private TetrahedronCellTopology
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TetrahedronCell);

  /** Standard class type alias. */
  itkCellCommonTypedefs(TetrahedronCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkOverrideGetNameOfClassMacro(TetrahedronCell);

  /** The type of boundary for this triangle's vertices. */
  using VertexType = VertexCell<TCellInterface>;
  using VertexAutoPointer = typename VertexType::SelfAutoPointer;

  /** The type of boundary for this triangle's edges. */
  using EdgeType = LineCell<TCellInterface>;
  using EdgeAutoPointer = typename EdgeType::SelfAutoPointer;

  /** The type of boundary for this hexahedron's faces. */
  using FaceType = TriangleCell<TCellInterface>;
  using FaceAutoPointer = typename FaceType::SelfAutoPointer;

  /** Tetrahedron-specific topology numbers. */
  static constexpr unsigned int NumberOfPoints = 4;
  static constexpr unsigned int NumberOfVertices = 4;
  static constexpr unsigned int NumberOfEdges = 6;
  static constexpr unsigned int NumberOfFaces = 4;
  static constexpr unsigned int CellDimension = 3;

  // Standard CellInterface

  /** Implement the standard CellInterface. */
  CellGeometryEnum
  GetType() const override
  {
    return CellGeometryEnum::TETRAHEDRON_CELL;
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

  /** Set the point id list used by the cell. It is assumed that the given iterator can be incremented and safely
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

  // Tetrahedron-specific interface

  /** Get the number of vertices defining the tetrahedron. */
  virtual CellFeatureCount
  GetNumberOfVertices() const;

  /** Get the number of edges defined for the tetrahedron. */
  virtual CellFeatureCount
  GetNumberOfEdges() const;

  /** Get the number of faces defined for the tetrahedron. */
  virtual CellFeatureCount
  GetNumberOfFaces() const;

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

  /** Get the face specified by the given cell feature Id.
   * The Id can range from 0 to GetNumberOfFaces()-1.
   */
  virtual bool
  GetFace(CellFeatureIdentifier, FaceAutoPointer &);

  /** Visitor interface. */
  itkCellVisitMacro(CellGeometryEnum::TETRAHEDRON_CELL);

  bool
  EvaluatePosition(CoordRepType *,
                   PointsContainer *,
                   CoordRepType *,
                   CoordRepType[],
                   double *,
                   InterpolationWeightType *) override;

public:
  TetrahedronCell() = default;

  ~TetrahedronCell() override = default;

protected:
  /** Store the number of points needed for a tetrahedron. */
  std::array<PointIdentifier, NumberOfPoints> m_PointIds{ MakeFilled<std::array<PointIdentifier, NumberOfPoints>>(
    NumericTraits<PointIdentifier>::max()) };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTetrahedronCell.hxx"
#endif

#endif
