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
#ifndef itkVertexCell_h
#define itkVertexCell_h

#include "itkCellInterface.h"
#include "itkNumericTraits.h"
#include "itkMakeFilled.h"

#include <array>

namespace itk
{
/** \class VertexCell
 *  \brief Represents a single vertex for a Mesh.
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

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(VertexCell);

  /** Vertex-specific topology numbers. */
  static constexpr unsigned int NumberOfPoints = 1;
  static constexpr unsigned int CellDimension = 0;

  // Standard CellInterface

  /** Implement the standard CellInterface. */
  CellGeometryEnum
  GetType() const override
  {
    return CellGeometryEnum::VERTEX_CELL;
  }
  void
  MakeCopy(CellAutoPointer &) const override;

  /** Get the topological dimension of this cell. */
  unsigned int
  GetDimension() const override;

  /** Get the number of points required to define the cell. */
  unsigned int
  GetNumberOfPoints() const override;

  /** A vertex has no boundary entities of any dimension. */
  CellFeatureCount
  GetNumberOfBoundaryFeatures(int dimension) const override;

  /** A vertex has no boundary entities.  Just return null. */
  bool
  GetBoundaryFeature(int dimension, CellFeatureIdentifier, CellAutoPointer &) override;

  /** Set the point id list used by the cell.  It is assumed that the given
   *  iterator can be incremented and safely de-referenced enough times to
   *  get all the point ids needed by the cell. */
  void
  SetPointIds(PointIdConstIterator first) override;

  /** Set the point id list used by the cell.  It is assumed that the range
   *  of iterators [first, last) contains the correct number of points needed to
   *  define the cell.  The position *last is NOT referenced, so it can safely
   *  be one beyond the end of an array or other container. */
  void
  SetPointIds(PointIdConstIterator first, PointIdConstIterator last) override;

  // Vertex-specific interface

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

  /** Set the identifier of the point defining the vertex. */
  virtual void SetPointId(PointIdentifier);

  /** Get the identifier of the point defining the vertex. */
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
  VertexCell() = default;

  ~VertexCell() override = default;

protected:
  /**
   * Store the number of points needed for a vertex.
   */
  std::array<PointIdentifier, NumberOfPoints> m_PointIds{ MakeFilled<std::array<PointIdentifier, NumberOfPoints>>(
    NumericTraits<PointIdentifier>::max()) };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkVertexCell.hxx"
#endif

#endif
