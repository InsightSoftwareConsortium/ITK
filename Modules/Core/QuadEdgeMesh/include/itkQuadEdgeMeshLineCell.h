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
#ifndef itkQuadEdgeMeshLineCell_h
#define itkQuadEdgeMeshLineCell_h

#include "itkAutoPointer.h"
#include "itkGeometricalQuadEdge.h"

namespace itk
{
/**
 * \class QuadEdgeMeshLineCell
 *
 * \brief Class that connects the QuadEdgeMesh with the Mesh
 *
 * \param TCellInterface Basic type for the itk*Cell.
 *        This usually comes from the MeshTraits.
 *
 * \author  Eric Boix, Alex Gouaillard, Leonardo Florez
 *
 * \ingroup ITKQuadEdgeMesh
 */
template <typename TCellInterface>
class ITK_TEMPLATE_EXPORT QuadEdgeMeshLineCell
  : public TCellInterface
  , public TCellInterface::CellTraits::QuadEdgeType
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(QuadEdgeMeshLineCell);

  /** Standard class type aliases. */
  // itkCellCommonTypedefs
  using Self = QuadEdgeMeshLineCell;
  using ConstSelfAutoPointer = AutoPointer<const Self>;
  using SelfAutoPointer = AutoPointer<Self>;
  using RawPointer = Self *;
  using ConstRawPointer = const Self *;

  // itkCellInheritedTypedefs
  using Superclass = TCellInterface;
  using PixelType = typename Superclass::PixelType;
  using CellType = typename Superclass::CellType;
  using CellAutoPointer = typename Superclass::CellAutoPointer;
  using CellConstAutoPointer = typename Superclass::CellConstAutoPointer;
  using CellRawPointer = typename Superclass::CellRawPointer;
  using CellConstRawPointer = typename Superclass::CellConstRawPointer;
  using CellTraits = typename Superclass::CellTraits;
  using CoordRepType = typename Superclass::CoordRepType;
  using InterpolationWeightType = typename Superclass::InterpolationWeightType;
  using PointIdentifier = typename Superclass::PointIdentifier;
  using CellIdentifier = typename Superclass::CellIdentifier;
  using CellFeatureIdentifier = typename Superclass::CellFeatureIdentifier;
  using CellFeatureCount = typename Superclass::CellFeatureIdentifier;
  using PointType = typename Superclass::PointType;
  using PointsContainer = typename Superclass::PointsContainer;
  using UsingCellsContainer = typename Superclass::UsingCellsContainer;
  using ParametricCoordArrayType = typename Superclass::ParametricCoordArrayType;
  using ShapeFunctionsArrayType = typename Superclass::ShapeFunctionsArrayType;
  static constexpr unsigned int PointDimension = Superclass::PointDimension;
  static constexpr unsigned int CellDimension = 2;

  /** Multivisitor type. */
  using MultiVisitor = typename CellType::MultiVisitor;

  //** */
  using PointIdIterator = typename CellTraits::PointIdIterator;
  using PointIdConstIterator = typename CellTraits::PointIdConstIterator;
  using PointIdInternalIterator = typename CellTraits::PointIdInternalIterator;
  using PointIdInternalConstIterator = typename CellTraits::PointIdInternalConstIterator;

  /** QE types. */
  using QEType = typename CellTraits::QuadEdgeType;
  using VertexRefType = typename QEType::OriginRefType;
  using FaceRefType = typename QEType::DualOriginRefType;
  using PrimalDataType = typename QEType::PrimalDataType;
  using DualDataType = typename QEType::DualDataType;
  using QEDual = typename QEType::DualType;

public:
  /** Standard part of every itk Object. */
  itkTypeMacro(QuadEdgeMeshLineCell, TCellInterface);

  // accessor to the new QEGeom link that replaces now inheritance.
  QEType *
  GetQEGeom() const
  {
    return (m_QuadEdgeGeom);
  }

public:
  /** Object memory management methods. */
  QuadEdgeMeshLineCell();
  ~QuadEdgeMeshLineCell() override;

  /** Accessors for m_Identifier. */
  void
  SetIdent(CellIdentifier cid);

  CellIdentifier
  GetIdent();

  /** TCellInterface abstract methods definition. */
  void
  Accept(CellIdentifier cellId, MultiVisitor * mv) override;

  CellGeometryEnum
  GetType() const override;

  /** Topology related methods. */
  static constexpr CellGeometryEnum
  GetTopologyId()
  {
    return CellGeometryEnum::LINE_CELL;
  }

  unsigned int
  GetDimension() const override;

  unsigned int
  GetNumberOfPoints() const override;

  CellFeatureCount
  GetNumberOfBoundaryFeatures(int dimension) const override;

  bool
  GetBoundaryFeature(int dimension, CellFeatureIdentifier cellId, CellAutoPointer & cell) override;

  /** Useless methods. */
  void
  MakeCopy(CellAutoPointer & cell) const override
  {
    cell.TakeOwnership(new Self);
    cell->SetPointId(0, this->GetQEGeom()->GetOrigin());
    cell->SetPointId(1, this->GetQEGeom()->GetDestination());
  }

  /** ITK Cell API - Iterator-related methods.
   *  The Set methods will work, not the Get.
   *  Hopefully never used ...
   */
  void
  SetPointIds(PointIdConstIterator first) override;

  void
  SetPointIds(PointIdConstIterator first, PointIdConstIterator last) override;

  void
  SetPointId(int localId, PointIdentifier pId) override;

  PointIdIterator
  PointIdsBegin() override
  {
    SynchronizePointsAPI();
    return &m_PointIds[0];
  }

  PointIdIterator
  PointIdsEnd() override
  {
    SynchronizePointsAPI();
    return (&m_PointIds[1] + 1);
  }

  PointIdConstIterator
  GetPointIds() const override
  {
    SynchronizePointsAPI();
    return &m_PointIds[0];
  }

  PointIdConstIterator
  PointIdsBegin() const override
  {
    SynchronizePointsAPI();
    return &m_PointIds[0];
  }

  PointIdConstIterator
  PointIdsEnd() const override
  {
    SynchronizePointsAPI();
    return (&m_PointIds[1] + 1);
  }

  /** helper for backward compatibility */
  void
  SynchronizePointsAPI() const
  {
    m_PointIds[0] = GetQEGeom()->GetOrigin();
    m_PointIds[1] = GetQEGeom()->GetDestination();
  }

  /** QuadEdge internal flavor of cell API */
  virtual void
  InternalSetPointIds(PointIdInternalConstIterator first);

  virtual void
  InternalSetPointIds(PointIdInternalConstIterator first, PointIdInternalConstIterator last);

  virtual PointIdInternalIterator
  InternalPointIdsBegin();

  virtual PointIdInternalIterator
  InternalPointIdsEnd();

  virtual PointIdInternalConstIterator
  InternalGetPointIds() const;

  virtual PointIdInternalConstIterator
  InternalPointIdsBegin() const;

  virtual PointIdInternalConstIterator
  InternalPointIdsEnd() const;

private:
  /**
   * In order to have constant time access at the itk level instead of
   * of doing a search in the Mesh::Cell container.
   */
  CellIdentifier          m_Identifier;
  QEType *                m_QuadEdgeGeom;
  mutable PointIdentifier m_PointIds[2];
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkQuadEdgeMeshLineCell.hxx"
#endif

#endif
