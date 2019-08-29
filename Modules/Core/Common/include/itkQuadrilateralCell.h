/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkQuadrilateralCell_h
#define itkQuadrilateralCell_h

#include "itkLineCell.h"
#include "itkQuadrilateralCellTopology.h"

namespace itk
{
/** \class QuadrilateralCell
 *  \brief Represents a quadrilateral for a Mesh.
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
class ITK_TEMPLATE_EXPORT QuadrilateralCell
  : public TCellInterface
  , private QuadrilateralCellTopology
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadrilateralCell);

  /** Standard class type aliases. */
  itkCellCommonTypedefs(QuadrilateralCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(QuadrilateralCell, CellInterface);

  /** The type of boundary for this triangle's vertices. */
  using VertexType = VertexCell<TCellInterface>;
  using VertexAutoPointer = typename VertexType::SelfAutoPointer;

  /** The type of boundary for this triangle's edges. */
  using EdgeType = LineCell<TCellInterface>;
  using EdgeAutoPointer = typename EdgeType::SelfAutoPointer;

  /** Quadrilateral-specific topology numbers. */
  static constexpr unsigned int NumberOfPoints = 4;
  static constexpr unsigned int NumberOfVertices = 4;
  static constexpr unsigned int NumberOfEdges = 4;
  static constexpr unsigned int CellDimension = 2;
  static constexpr unsigned int NumberOfDerivatives = 8;

  /** Implement the standard CellInterface. */
  CellGeometry
  GetType() const override
  {
    return Superclass::QUADRILATERAL_CELL;
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

  /** Quadrilateral-specific interface. */
  virtual CellFeatureCount
  GetNumberOfVertices() const;

  virtual CellFeatureCount
  GetNumberOfEdges() const;

  virtual bool
  GetVertex(CellFeatureIdentifier, VertexAutoPointer &);
  virtual bool
  GetEdge(CellFeatureIdentifier, EdgeAutoPointer &);

  /** Evaluate the position inside the cell */
  bool
  EvaluatePosition(CoordRepType *    position,
                   PointsContainer * points,
                   CoordRepType *    closestPoint,
                   CoordRepType[CellDimension],
                   double *                  dist2,
                   InterpolationWeightType * weight) override;

  /** Visitor interface */
  itkCellVisitMacro(Superclass::QUADRILATERAL_CELL);

  /** Constructor and destructor */
  QuadrilateralCell()
  {
    for (PointIdentifier i = 0; i < Self::NumberOfPoints; i++)
    {
      m_PointIds[i] = NumericTraits<PointIdentifier>::max();
    }
  }

#if defined(__GNUC__) && (__GNUC__ > 5)
  ~QuadrilateralCell() override = default;
#else
  ~QuadrilateralCell() override{};
#endif

protected:
  /** Store the number of points needed for a quadrilateral. */
  PointIdentifier m_PointIds[NumberOfPoints];

  void
  InterpolationDerivs(const CoordRepType pointCoords[CellDimension], CoordRepType derivs[NumberOfDerivatives]);
  void
  InterpolationFunctions(const CoordRepType      pointCoords[CellDimension],
                         InterpolationWeightType weights[NumberOfPoints]);
  void
  EvaluateLocation(int &                     itkNotUsed(subId),
                   const PointsContainer *   points,
                   const CoordRepType        pointCoords[PointDimension],
                   CoordRepType              x[PointDimension],
                   InterpolationWeightType * weights);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkQuadrilateralCell.hxx"
#endif

#endif
