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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkPolygonCell_h
#define itkPolygonCell_h

#include "itkLineCell.h"
#include "itkPoint.h"
#include <vector>
#include <deque>

namespace itk
{
/** \class PolygonCell
 *  \brief Represents a polygon in a Mesh.
 *
 * PolygonCell represents a polygon for a Mesh.
 *  the points of the polygon can be dynamically changed.
 *
 * \tparam TPixelType The type associated with a point, cell, or boundary
 * for use in storing its data.
 *
 * \tparam TCellTraits Type information of mesh containing cell.
 *
 * \ingroup MeshObjects
 * \ingroup ITKCommon
 */
template <typename TCellInterface>
class ITK_TEMPLATE_EXPORT PolygonCell : public TCellInterface
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PolygonCell);

  /** Standard class type aliases. */
  itkCellCommonTypedefs(PolygonCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(PolygonCell, CellInterface);

  /** Save some template parameter information. */
  static constexpr unsigned int CellDimension = 2;

  /** The type of boundary for this triangle's vertices. */
  using VertexType = VertexCell<TCellInterface>;
  using VertexAutoPointer = typename VertexType::SelfAutoPointer;

  /** The type of boundary for this triangle's edges. */
  using EdgeType = LineCell<TCellInterface>;
  using EdgeAutoPointer = typename EdgeType::SelfAutoPointer;

  using EdgeInfo = FixedArray<int, 2>;
  using EdgeInfoDQ = std::deque<EdgeInfo>;

  /** Need to add POLYGON_CELL into CellInterface. */
  itkCellVisitMacro(CellGeometryEnum::POLYGON_CELL);

  /** Implement the standard CellInterface. */
  CellGeometryEnum
  GetType() const override
  {
    return CellGeometryEnum::POLYGON_CELL;
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

  void AddPointId(PointIdentifier);
  void RemovePointId(PointIdentifier);
  void
  SetPointIds(int dummy, int num, PointIdConstIterator first);

  void
  BuildEdges();

  void
  ClearPoints();

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

  /** Polygon-specific interface. */
  virtual CellFeatureCount
  GetNumberOfVertices() const;

  virtual CellFeatureCount
  GetNumberOfEdges() const;

  virtual bool
  GetVertex(CellFeatureIdentifier, VertexAutoPointer &);
  virtual bool
  GetEdge(CellFeatureIdentifier, EdgeAutoPointer &);

  /** Constructor and destructor */
  PolygonCell() = default;
  PolygonCell(PointIdentifier NumberOfPoints)
  {
    for (PointIdentifier i = 0; i < NumberOfPoints; i++)
    {
      m_PointIds.push_back(NumericTraits<PointIdentifier>::max());
    }
    this->BuildEdges();
  }

  ~PolygonCell() override = default;

protected:
  std::vector<EdgeInfo>        m_Edges;
  std::vector<PointIdentifier> m_PointIds;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPolygonCell.hxx"
#endif

#endif
