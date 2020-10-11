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
#ifndef itkTetrahedronCell_h
#define itkTetrahedronCell_h

#include "itkTriangleCell.h"
#include "itkTetrahedronCellTopology.h"

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

  /** Standard class type aliasa. */
  itkCellCommonTypedefs(TetrahedronCell);
  itkCellInheritedTypedefs(TCellInterface);

  /** Standard part of every itk Object. */
  itkTypeMacro(TetrahedronCell, CellInterface);

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

  /** Implement the standard CellInterface. */
  CellGeometryEnum
  GetType() const override
  {
    return CellGeometryEnum::TETRAHEDRON_CELL;
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

  /** Tetrahedron-specific interface. */
  virtual CellFeatureCount
  GetNumberOfVertices() const;

  virtual CellFeatureCount
  GetNumberOfEdges() const;

  virtual CellFeatureCount
  GetNumberOfFaces() const;

  virtual bool
  GetVertex(CellFeatureIdentifier, VertexAutoPointer &);
  virtual bool
  GetEdge(CellFeatureIdentifier, EdgeAutoPointer &);
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
  TetrahedronCell()
  {
    for (PointIdentifier i = 0; i < Self::NumberOfPoints; i++)
    {
      m_PointIds[i] = NumericTraits<PointIdentifier>::max();
    }
  }

  ~TetrahedronCell() override = default;

protected:
  /** Store the number of points needed for a tetrahedron. */
  PointIdentifier m_PointIds[NumberOfPoints];
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkTetrahedronCell.hxx"
#endif

#endif
