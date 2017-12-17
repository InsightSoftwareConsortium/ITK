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
template< typename TCellInterface >
class ITK_TEMPLATE_EXPORT QuadEdgeMeshLineCell:
  public TCellInterface, public TCellInterface::CellTraits::QuadEdgeType
{
public:
  /** Standard class typedefs. */
  // itkCellCommonTypedefs
  typedef QuadEdgeMeshLineCell      Self;
  typedef AutoPointer< const Self > ConstSelfAutoPointer;
  typedef AutoPointer< Self >       SelfAutoPointer;
  typedef Self *                    RawPointer;
  typedef const Self *              ConstRawPointer;

  // itkCellInheritedTypedefs
  typedef TCellInterface                                Superclass;
  typedef typename Superclass::PixelType                PixelType;
  typedef typename Superclass::CellType                 CellType;
  typedef typename Superclass::CellAutoPointer          CellAutoPointer;
  typedef typename Superclass::CellConstAutoPointer     CellConstAutoPointer;
  typedef typename Superclass::CellRawPointer           CellRawPointer;
  typedef typename Superclass::CellConstRawPointer      CellConstRawPointer;
  typedef typename Superclass::CellTraits               CellTraits;
  typedef typename Superclass::CoordRepType             CoordRepType;
  typedef typename Superclass::InterpolationWeightType  InterpolationWeightType;
  typedef typename Superclass::PointIdentifier          PointIdentifier;
  typedef typename Superclass::CellIdentifier           CellIdentifier;
  typedef typename Superclass::CellFeatureIdentifier    CellFeatureIdentifier;
  typedef typename Superclass::CellFeatureIdentifier    CellFeatureCount;
  typedef typename Superclass::PointType                PointType;
  typedef typename Superclass::PointsContainer          PointsContainer;
  typedef typename Superclass::UsingCellsContainer      UsingCellsContainer;
  typedef typename Superclass::CellGeometry             CellGeometry;
  typedef typename Superclass::ParametricCoordArrayType ParametricCoordArrayType;
  typedef typename Superclass::ShapeFunctionsArrayType  ShapeFunctionsArrayType;
  itkStaticConstMacro(PointDimension, unsigned int, Superclass::PointDimension);
  itkStaticConstMacro(CellDimension, unsigned int, 2);

  /** Multivisitor type. */
  typedef typename CellType::MultiVisitor MultiVisitor;

  //** */
  typedef typename CellTraits::PointIdIterator              PointIdIterator;
  typedef typename CellTraits::PointIdConstIterator         PointIdConstIterator;
  typedef typename CellTraits::PointIdInternalIterator      PointIdInternalIterator;
  typedef typename CellTraits::PointIdInternalConstIterator PointIdInternalConstIterator;

  /** QE types. */
  typedef typename CellTraits::QuadEdgeType  QEType;
  typedef typename QEType::OriginRefType     VertexRefType;
  typedef typename QEType::DualOriginRefType FaceRefType;
  typedef typename QEType::PrimalDataType    PrimalDataType;
  typedef typename QEType::DualDataType      DualDataType;
  typedef typename QEType::DualType          QEDual;

public:
  /** Standard part of every itk Object. */
  itkTypeMacro(QuadEdgeMeshLineCell, TCellInterface);

  // accessor to the new QEGeom link that replaces now inheritance.
  QEType * GetQEGeom() const { return ( m_QuadEdgeGeom ); }

public:
  /** Object memory management methods. */
  QuadEdgeMeshLineCell();
  ~QuadEdgeMeshLineCell() override;

  /** Accessors for m_Identifier. */
  void SetIdent(CellIdentifier cid);

  CellIdentifier GetIdent();

  /** TCellInterface abstract methods definition. */
  void Accept(CellIdentifier cellId, MultiVisitor *mv) override;

  CellGeometry GetType() const override;

  /** Topology related methods. */
  static int GetTopologyId();

  unsigned int GetDimension() const override;

  unsigned int GetNumberOfPoints() const override;

  CellFeatureCount GetNumberOfBoundaryFeatures(int dimension) const override;

  bool GetBoundaryFeature(int dimension,
                                  CellFeatureIdentifier cellId,
                                  CellAutoPointer & cell) override;

  /** Useless methods. */
  void MakeCopy(CellAutoPointer & cell) const override
  {
    cell.TakeOwnership(new Self);
    cell->SetPointId( 0, this->GetQEGeom()->GetOrigin() );
    cell->SetPointId( 1, this->GetQEGeom()->GetDestination() );
  }

  /** ITK Cell API - Iterator-related methods.
   *  The Set methods will work, not the Get.
   *  Hopefully never used ...
   */
  void SetPointIds(PointIdConstIterator first) override;

  void SetPointIds(PointIdConstIterator first,
                           PointIdConstIterator last) override;

  void SetPointId(int localId, PointIdentifier pId) override;

  PointIdIterator PointIdsBegin() override
  {
    SynchronizePointsAPI();
    return &m_PointIds[0];
  }

  PointIdIterator PointIdsEnd() override
  {
    SynchronizePointsAPI();
    return ( &m_PointIds[1] + 1 );
  }

  PointIdConstIterator GetPointIds() const override
  {
    SynchronizePointsAPI();
    return &m_PointIds[0];
  }

  PointIdConstIterator PointIdsBegin() const override
  {
    SynchronizePointsAPI();
    return &m_PointIds[0];
  }

  PointIdConstIterator PointIdsEnd() const override
  {
    SynchronizePointsAPI();
    return ( &m_PointIds[1] + 1 );
  }

  /** helper for backward compatibility */
  void SynchronizePointsAPI() const
  {
    m_PointIds[0] = GetQEGeom()->GetOrigin();
    m_PointIds[1] = GetQEGeom()->GetDestination();
  }

  /** QuadEdge internal flavor of cell API */
  virtual void InternalSetPointIds(PointIdInternalConstIterator first);

  virtual void InternalSetPointIds(
    PointIdInternalConstIterator first,
    PointIdInternalConstIterator last);

  virtual PointIdInternalIterator InternalPointIdsBegin();

  virtual PointIdInternalIterator InternalPointIdsEnd();

  virtual PointIdInternalConstIterator InternalGetPointIds() const;

  virtual PointIdInternalConstIterator InternalPointIdsBegin() const;

  virtual PointIdInternalConstIterator InternalPointIdsEnd() const;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(QuadEdgeMeshLineCell);

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
#include "itkQuadEdgeMeshLineCell.hxx"
#endif

#endif
