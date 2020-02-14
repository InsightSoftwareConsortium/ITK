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
#ifndef itkQuadEdgeMeshPolygonCell_h
#define itkQuadEdgeMeshPolygonCell_h

#include "itkTriangleCell.h"
#include "itkQuadEdgeMeshLineCell.h"
namespace itk
{
/**
 *\class QuadEdgeMeshPolygonCell
 * Class that connects the QE with itk
 *
 * \param TCellInterface Basic type for the itk*Cell. This usually comes
 *        from the MeshTraits.
 * \author Alexandre Gouaillard, Leonardo Florez-Valencia, Eric Boix
 *
 * This implementation was contributed as a paper to the Insight Journal
 * https://hdl.handle.net/1926/306
 *
 * \ingroup ITKQuadEdgeMesh
 */
template <typename TCellInterface>
class ITK_TEMPLATE_EXPORT QuadEdgeMeshPolygonCell : public TCellInterface
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadEdgeMeshPolygonCell);

  /** Standard class type aliases. */
  // itkCellCommonTypedefs
  using Self = QuadEdgeMeshPolygonCell;
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

  using EdgeCellType = QuadEdgeMeshLineCell<CellType>;
  using EdgeCellListType = std::vector<EdgeCellType *>;

  //** */
  using PointIdIterator = typename CellTraits::PointIdIterator;
  using PointIdConstIterator = typename CellTraits::PointIdConstIterator;
  using PointIdInternalIterator = typename CellTraits::PointIdInternalIterator;
  using PointIdInternalConstIterator = typename CellTraits::PointIdInternalConstIterator;

  /** QE types. */
  using QuadEdgeType = typename CellTraits::QuadEdgeType;
  using VertexRefType = typename QuadEdgeType::OriginRefType;
  using FaceRefType = typename QuadEdgeType::DualOriginRefType;
  using PrimalDataType = typename QuadEdgeType::PrimalDataType;
  using DualDataType = typename QuadEdgeType::DualDataType;
  using QEDual = typename QuadEdgeType::DualType;

public:
  /** Standard part of every itk Object. */
  itkTypeMacro(QuadEdgeMeshPolygonCell, TCellInterface);

  /** Object memory management methods. */
  QuadEdgeMeshPolygonCell(PointIdentifier nPoints = 0);
  QuadEdgeMeshPolygonCell(QuadEdgeType * e);
  ~QuadEdgeMeshPolygonCell() override;

  /** Accessors for m_Ident. */
  void
  SetIdent(CellIdentifier cid)
  {
    m_Ident = cid;
  }
  CellIdentifier
  GetIdent()
  {
    return (m_Ident);
  }

  /** Lnext ring entry accessors. */
  QuadEdgeType *
  GetEdgeRingEntry() const
  {
    return (m_EdgeRingEntry);
  }
  void
  SetEdgeRingEntry(QuadEdgeType * entry)
  {
    m_EdgeRingEntry = entry;
  }

  /** Implement the standard CellInterface. */
  SelfAutoPointer
  New();

  /** TCellInterface abstract methods definition. */
  void
  Accept(CellIdentifier cellId, MultiVisitor * mv) override;

  CellGeometryEnum
  GetType() const override
  {
    return (CellGeometryEnum::POLYGON_CELL);
  }

  /** itk topology related methods. */
  static constexpr CellGeometryEnum
  GetTopologyId()
  {
    return CellGeometryEnum::POLYGON_CELL;
  }

  unsigned int
  GetDimension() const override
  {
    return (Self::CellDimension);
  }

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
    const PointIdentifier numberOfPoints = this->GetNumberOfPoints();
    auto *                newPolygonCell = new Self(numberOfPoints);

    cell.TakeOwnership(newPolygonCell);
    if (numberOfPoints)
    {
      PointIdentifier i = 0;

      PointIdInternalConstIterator it = this->InternalPointIdsBegin();
      PointIdInternalConstIterator end = this->InternalPointIdsEnd();

      while (it != end)
      {
        newPolygonCell->SetPointId(i, it.Value()->GetOrigin());
        ++i;
        ++it;
      }
    }
  }

  /** ITK Cell API - Iterator-related methods. */
  void
  SetPointIds(PointIdConstIterator first) override;

  void
  SetPointIds(PointIdConstIterator first, PointIdConstIterator last) override;

  void
  SetPointId(int localId, PointIdentifier pId) override;

  virtual PointIdentifier
  GetPointId(int localId) const;

  PointIdIterator
  PointIdsBegin() override
  {
    // NOTE ALEX: should update the array on the fly to make it faster
    MakePointIds();
    if (m_PointIds.empty())
    {
      return (static_cast<PointIdIterator>(nullptr));
    }
    else
    {
      return &*(m_PointIds.begin());
    }
  }

  PointIdIterator
  PointIdsEnd() override
  {
    // NOTE ALEX: should update the array on the fly to make it faster
    if (m_PointIds.empty())
    {
      return (static_cast<PointIdIterator>(nullptr));
    }
    else
    {
      return &m_PointIds[m_PointIds.size() - 1] + 1;
    }
  }

  PointIdConstIterator
  PointIdsBegin() const override
  {
    // NOTE ALEX: should update the array on the fly to make it faster
    MakePointIds();
    if (m_PointIds.empty())
    {
      return (static_cast<PointIdIterator>(nullptr));
    }
    else
    {
      return &*(m_PointIds.begin());
    }
  }

  PointIdConstIterator
  PointIdsEnd() const override
  {
    // NOTE ALEX: should update the array on the fly to make it faster
    if (m_PointIds.empty())
    {
      return (static_cast<PointIdIterator>(nullptr));
    }
    else
    {
      return &m_PointIds[m_PointIds.size() - 1] + 1;
    }
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

protected:
  using PointIDListType = std::vector<PointIdentifier>;
  mutable PointIDListType m_PointIds;

private:
  void
  MakePointIds() const
  {
    m_PointIds.clear();

    PointIdInternalConstIterator it = this->InternalPointIdsBegin();
    PointIdInternalConstIterator end = this->InternalPointIdsEnd();

    while (it != end)
    {
      m_PointIds.push_back(it.Value()->GetOrigin());
      ++it;
    }
  }

  /** In order to have constant time access at the itk level instead of
   * doing a search in the Mesh::Cell container.
   */
  CellIdentifier m_Ident;

  /**
   * Entry point into the edge ring.
   */
  QuadEdgeType * m_EdgeRingEntry;

  /**
   * List of EdgeCells created by the constructor for proper deletion
   */
  EdgeCellListType m_EdgeCellList;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkQuadEdgeMeshPolygonCell.hxx"
#endif

#endif
