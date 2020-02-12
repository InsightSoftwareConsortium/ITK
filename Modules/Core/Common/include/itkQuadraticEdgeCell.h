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
#ifndef itkQuadraticEdgeCell_h
#define itkQuadraticEdgeCell_h

#include "itkVertexCell.h"

namespace itk
{
/** \class QuadraticEdgeCell
 *  \brief Represents a second order line segment for a Mesh.
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
class ITK_TEMPLATE_EXPORT QuadraticEdgeCell : public TCellInterface
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadraticEdgeCell);

  /** Standard class type aliases. */
  itkCellCommonTypedefs(QuadraticEdgeCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(QuadraticEdgeCell, CellInterface);

  /** The type of boundary for this lines's vertices. */
  using VertexType = VertexCell<TCellInterface>;
  using VertexAutoPointer = typename VertexType::SelfAutoPointer;

  /** QuadraticEdge-specific topology numbers. */
  static constexpr unsigned int NumberOfPoints = 3;
  static constexpr unsigned int NumberOfVertices = 2;
  static constexpr unsigned int CellDimension = 1;

  /** Implement the standard CellInterface. */
  CellGeometryEnum
  GetType() const override
  {
    return CellGeometryEnum::QUADRATIC_EDGE_CELL;
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

  /** QuadraticEdge-specific interface. */
  virtual CellFeatureCount
  GetNumberOfVertices() const;

  virtual bool
  GetVertex(CellFeatureIdentifier, VertexAutoPointer &);

  /** Visitor interface */
  itkCellVisitMacro(CellGeometryEnum::QUADRATIC_EDGE_CELL);

  QuadraticEdgeCell()
  {
    for (PointIdentifier i = 0; i < Self::NumberOfPoints; i++)
    {
      m_PointIds[i] = NumericTraits<PointIdentifier>::max();
    }
  }

  ~QuadraticEdgeCell() override = default;

  /** Given the parametric coordinates of a point in the cell
   * returned the values of its ShapeFunctions */
  void
  EvaluateShapeFunctions(const ParametricCoordArrayType & parametricCoordinates,
                         ShapeFunctionsArrayType &        weights) const override;

protected:
  /** Store number of points needed for a line segment. */
  PointIdentifier m_PointIds[NumberOfPoints];
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkQuadraticEdgeCell.hxx"
#endif

#endif
