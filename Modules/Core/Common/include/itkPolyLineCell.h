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
#ifndef itkPolyLineCell_h
#define itkPolyLineCell_h

#include "itkVertexCell.h"
namespace itk
{
/** \class PolyLineCell
 *  \brief Represents a series of connected line segments for a Mesh.
 *
 * PolyLineCell represents a series of connected line segments for a Mesh.
 *
 * Template parameters for PolyLineCell:
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
class ITK_TEMPLATE_EXPORT PolyLineCell : public TCellInterface
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(PolyLineCell);

  /** Standard class type aliases. */
  itkCellCommonTypedefs(PolyLineCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(PolyLineCell, CellInterface);

  /** The type of boundary for this lines's vertices. */
  using VertexType = VertexCell<TCellInterface>;
  using VertexAutoPointer = typename VertexType::SelfAutoPointer;

  /** Line-specific topology numbers. */
  static constexpr unsigned int CellDimension = 1;

  /** Implement the standard CellInterface. */
  CellGeometryEnum
  GetType() const override
  {
    return CellGeometryEnum::POLYLINE_CELL;
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
  InitializePoints();

  void
  ClearPoints();

  void
  SetPointIds(PointIdConstIterator first) override;

  void
  SetPointIds(PointIdConstIterator first, PointIdConstIterator last) override;

  void
  SetPointIds(int dummy, int num, PointIdConstIterator first);

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

  /** Line-specific interface. */
  virtual CellFeatureCount
  GetNumberOfVertices() const;

  virtual bool
  GetVertex(CellFeatureIdentifier, VertexAutoPointer &);

  /** Visitor interface */
  itkCellVisitMacro(CellGeometryEnum::POLYLINE_CELL);

  void
  InitializePoints(PointIdentifier numberOfPoints)
  {
    m_PointIds.clear();
    for (PointIdentifier i = 0; i < numberOfPoints; ++i)
    {
      m_PointIds.push_back(NumericTraits<PointIdentifier>::max());
    }
  }

  /** Constructor and destructor */
  PolyLineCell() { InitializePoints(2); }

  PolyLineCell(PointIdentifier numberOfPoints) { InitializePoints(numberOfPoints); }

  ~PolyLineCell() override = default;

protected:
  /** For storing the points needed for a line segment. */
  std::vector<PointIdentifier> m_PointIds;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkPolyLineCell.hxx"
#endif

#endif
