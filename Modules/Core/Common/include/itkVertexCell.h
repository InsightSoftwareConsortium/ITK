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
#ifndef itkVertexCell_h
#define itkVertexCell_h

#include "itkCellInterface.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class VertexCell
 *  \brief Represents a single vertex for a Mesh.
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
class ITK_TEMPLATE_EXPORT VertexCell : public TCellInterface
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(VertexCell);

  /** Standard class type aliases. */
  itkCellCommonTypedefs(VertexCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(VertexCell, CellInterface);

  /** Vertex-specific topology numbers. */
  static constexpr unsigned int NumberOfPoints = 1;
  static constexpr unsigned int CellDimension = 0;

  /** Implement the standard CellInterface. */
  CellGeometryEnum
  GetType() const override
  {
    return CellGeometryEnum::VERTEX_CELL;
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

  /** Vertex-specific interface. */
  virtual void SetPointId(PointIdentifier);
  virtual PointIdentifier
  GetPointId();

  /** Cell visitor interface */
  itkCellVisitMacro(CellGeometryEnum::VERTEX_CELL);

  /** Evaluate the position of a given point */
  bool
  EvaluatePosition(CoordRepType *,
                   PointsContainer *,
                   CoordRepType *,
                   CoordRepType[],
                   double *,
                   InterpolationWeightType *) override;

public:
  VertexCell()
  {
    for (PointIdentifier i = 0; i < Self::NumberOfPoints; i++)
    {
      m_PointIds[i] = NumericTraits<PointIdentifier>::max();
    }
  }

  ~VertexCell() override = default;

protected:
  /**
   * Store the number of points needed for a vertex.
   */
  PointIdentifier m_PointIds[NumberOfPoints];
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVertexCell.hxx"
#endif

#endif
