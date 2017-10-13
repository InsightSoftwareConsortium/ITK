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
#ifndef itkQuadEdgeMeshPolygonCell_h
#define itkQuadEdgeMeshPolygonCell_h

#include "itkTriangleCell.h"
#include "itkQuadEdgeMeshLineCell.h"
namespace itk
{
/** \class QuadEdgeMeshPolygonCell
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
template< typename TCellInterface >
class ITK_TEMPLATE_EXPORT QuadEdgeMeshPolygonCell:public TCellInterface
{
public:
  /** Standard class typedefs. */
  // itkCellCommonTypedefs
  typedef QuadEdgeMeshPolygonCell   Self;
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

  typedef QuadEdgeMeshLineCell< CellType > EdgeCellType;
  typedef std::vector< EdgeCellType * >    EdgeCellListType;

  //** */
  typedef typename CellTraits::PointIdIterator              PointIdIterator;
  typedef typename CellTraits::PointIdConstIterator         PointIdConstIterator;
  typedef typename CellTraits::PointIdInternalIterator      PointIdInternalIterator;
  typedef typename CellTraits::PointIdInternalConstIterator PointIdInternalConstIterator;

  /** QE types. */
  typedef typename CellTraits::QuadEdgeType        QuadEdgeType;
  typedef typename QuadEdgeType::OriginRefType     VertexRefType;
  typedef typename QuadEdgeType::DualOriginRefType FaceRefType;
  typedef typename QuadEdgeType::PrimalDataType    PrimalDataType;
  typedef typename QuadEdgeType::DualDataType      DualDataType;
  typedef typename QuadEdgeType::DualType          QEDual;

public:
  /** Standard part of every itk Object. */
  itkTypeMacro(QuadEdgeMeshPolygonCell, TCellInterface);

  /** Object memory management methods. */
  QuadEdgeMeshPolygonCell(PointIdentifier nPoints = 0);
  QuadEdgeMeshPolygonCell(QuadEdgeType *e);
  virtual ~QuadEdgeMeshPolygonCell() ITK_OVERRIDE;

  /** Accessors for m_Ident. */
  void SetIdent(CellIdentifier cid) { m_Ident = cid; }
  CellIdentifier GetIdent()          { return ( m_Ident ); }

  /** Lnext ring entry accessors. */
  QuadEdgeType * GetEdgeRingEntry() const { return ( m_EdgeRingEntry ); }
  void SetEdgeRingEntry(QuadEdgeType *entry) { m_EdgeRingEntry = entry; }

  /** Implement the standard CellInterface. */
  SelfAutoPointer New();

  /** TCellInterface abstract methods definition. */
  virtual void Accept(CellIdentifier cellId, MultiVisitor *mv) ITK_OVERRIDE;

  virtual CellGeometry GetType() const ITK_OVERRIDE { return ( Superclass::POLYGON_CELL ); }

  /** itk topology related methods. */
  static int GetTopologyId()
  {
    return ( Superclass::POLYGON_CELL );
  }

  virtual unsigned int GetDimension() const ITK_OVERRIDE
  {
    return ( Self::CellDimension );
  }

  virtual unsigned int GetNumberOfPoints() const ITK_OVERRIDE;

  virtual CellFeatureCount GetNumberOfBoundaryFeatures(int dimension) const ITK_OVERRIDE;

  virtual bool GetBoundaryFeature(int dimension,
                                  CellFeatureIdentifier cellId,
                                  CellAutoPointer & cell) ITK_OVERRIDE;

  /** Useless methods. */
  virtual void MakeCopy(CellAutoPointer & cell) const ITK_OVERRIDE
  {
    const PointIdentifier numberOfPoints = this->GetNumberOfPoints();
    Self *                newPolygonCell = new Self(numberOfPoints);

    cell.TakeOwnership(newPolygonCell);
    if ( numberOfPoints )
      {
      PointIdentifier i = 0;

      PointIdInternalConstIterator it   = this->InternalPointIdsBegin();
      PointIdInternalConstIterator end  = this->InternalPointIdsEnd();

      while( it != end )
        {
        newPolygonCell->SetPointId( i, it.Value()->GetOrigin() );
        ++i;
        ++it;
        }
      }
  }

  /** ITK Cell API - Iterator-related methods. */
  virtual void SetPointIds(PointIdConstIterator first) ITK_OVERRIDE;

  virtual void SetPointIds(PointIdConstIterator first,
                           PointIdConstIterator last) ITK_OVERRIDE;

  virtual void SetPointId(int localId, PointIdentifier pId) ITK_OVERRIDE;

  virtual PointIdentifier GetPointId(int localId) const;

  virtual PointIdIterator PointIdsBegin() ITK_OVERRIDE
  {
    // NOTE ALEX: should update the array on the fly to make it faster
    MakePointIds();
    if ( m_PointIds.size() == 0 )
      {
      return ( static_cast< PointIdIterator >( ITK_NULLPTR ) );
      }
    else
      {
      return &*( m_PointIds.begin() );
      }
  }

  virtual PointIdIterator PointIdsEnd() ITK_OVERRIDE
  {
    // NOTE ALEX: should update the array on the fly to make it faster
    if ( m_PointIds.size() == 0 )
      {
      return ( static_cast< PointIdIterator >( ITK_NULLPTR ) );
      }
    else
      {
      return &m_PointIds[m_PointIds.size() - 1] + 1;
      }
  }

  virtual PointIdConstIterator PointIdsBegin() const ITK_OVERRIDE
  {
    // NOTE ALEX: should update the array on the fly to make it faster
    MakePointIds();
    if ( m_PointIds.size() == 0 )
      {
      return ( static_cast< PointIdIterator >( ITK_NULLPTR ) );
      }
    else
      {
      return &*( m_PointIds.begin() );
      }
  }

  virtual PointIdConstIterator PointIdsEnd() const ITK_OVERRIDE
  {
    // NOTE ALEX: should update the array on the fly to make it faster
    if ( m_PointIds.size() == 0 )
      {
      return ( static_cast< PointIdIterator >( ITK_NULLPTR ) );
      }
    else
      {
      return &m_PointIds[m_PointIds.size() - 1] + 1;
      }
  }

  /** QuadEdge internal flavor of cell API */
  virtual void InternalSetPointIds(PointIdInternalConstIterator first);

  virtual void InternalSetPointIds(PointIdInternalConstIterator first,
                                   PointIdInternalConstIterator last);

  virtual PointIdInternalIterator InternalPointIdsBegin();

  virtual PointIdInternalIterator InternalPointIdsEnd();

  virtual PointIdInternalConstIterator InternalGetPointIds() const;

  virtual PointIdInternalConstIterator InternalPointIdsBegin() const;

  virtual PointIdInternalConstIterator InternalPointIdsEnd() const;

protected:
  typedef std::vector< PointIdentifier > PointIDListType;
  mutable PointIDListType m_PointIds;

private:
  QuadEdgeMeshPolygonCell(const Self &); // Not impl.
  void operator=(const Self &);          // Not impl.

  void MakePointIds() const
  {
    m_PointIds.clear();

    PointIdInternalConstIterator it   = this->InternalPointIdsBegin();
    PointIdInternalConstIterator end  = this->InternalPointIdsEnd();

    while( it != end )
      {
      m_PointIds.push_back( it.Value()->GetOrigin() );
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
  QuadEdgeType *m_EdgeRingEntry;

  /**
   * List of EdgeCells created by the constructor for proper deletion
   */
  EdgeCellListType m_EdgeCellList;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadEdgeMeshPolygonCell.hxx"
#endif

#endif
