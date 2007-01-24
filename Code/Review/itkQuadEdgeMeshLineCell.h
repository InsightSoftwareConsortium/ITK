// -------------------------------------------------------------------------
// itkQuadEdgeMeshLineCell.h
// $Revision $
// $Author $
// $Name:  $
// $Date: 2007-01-24 23:58:14 $
// -------------------------------------------------------------------------
// This code is an implementation of the well known quad edge (QE) data
// structure in the ITK library. Although the original QE can handle non
// orientable 2-manifolds and its dual and its mirror, this implementation
// is specifically dedicated to handle orientable 2-manifolds along with
// their dual.
//
// Any comment, criticism and/or donation is welcome.
//
// Please contact any member of the team:
//
// - The frog master (Eric Boix)       eboix@ens-lyon.fr
// - The duck master (Alex Gouaillard) alexandre.gouaillard@sun.com
// - The cow  master (Leonardo Florez) florez@creatis.insa-lyon.fr
// -------------------------------------------------------------------------

#ifndef __itkQuadEdgeMeshLineCell_h
#define __itkQuadEdgeMeshLineCell_h

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
 */
template< class TCellInterface >
class QuadEdgeMeshLineCell 
  : public TCellInterface, public TCellInterface::CellTraits::QuadEdgeType
{
public:
  /** Standard class typedefs. */
  // itkCellCommonTypedefs
  typedef QuadEdgeMeshLineCell           Self;
  typedef itk::AutoPointer< const Self > ConstSelfAutoPointer;
  typedef itk::AutoPointer< Self >       SelfAutoPointer;
  typedef Self *                         RawPointer;
  typedef const Self *                   ConstRawPointer;

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
  itkStaticConstMacro( PointDimension, unsigned int, Superclass::PointDimension );
  itkStaticConstMacro( CellDimension, unsigned int, 2 );

  /** Multivisitor type. */
  typedef typename CellType::MultiVisitor MultiVisitor;

  /** QE types. */
  typedef typename Superclass::CellTraits::QuadEdgeType  QEType;
  typedef typename QEType::OriginRefType                 VertexRefType;
  typedef typename QEType::DualOriginRefType             FaceRefType;
  typedef typename QEType::PrimalDataType                PrimalDataType;
  typedef typename QEType::DualDataType                  DualDataType;
  typedef typename QEType::DualType                      QEDual;

  /** Iterator types. */
  typedef typename QEType::IteratorGeom      PointIdIterator;
  typedef typename QEType::ConstIteratorGeom PointIdConstIterator;

public:
  /** Standard part of every itk Object. */
  itkTypeMacro( QuadEdgeMeshLineCell, TCellInterface );

  /** Methods for a QECell. */
  virtual void MakeEdge();
 
  itkQEAccessorsMacro( QEType, Self, QEDual );

public:
  /** Object memory management methods. */
  QuadEdgeMeshLineCell( bool makeEdge = true );
  virtual ~QuadEdgeMeshLineCell();

  /** Accessors for m_Identifier. */
  void SetIdent( CellIdentifier cid );
  CellIdentifier GetIdent();

  /** Implement the standard CellInterface. */
  SelfAutoPointer New();

  /** TCellInterface abstract methods definition. */
  virtual void Accept( unsigned long cellId, MultiVisitor* mv );
  virtual CellGeometry GetType() const;

  /** Topology related methods. */
  static int GetTopologyId();
  virtual unsigned int GetDimension() const;
  virtual unsigned int GetNumberOfPoints() const;
  
  virtual CellFeatureCount GetNumberOfBoundaryFeatures( int dimension ) const;
  virtual bool GetBoundaryFeature( int dimension,
                                   CellFeatureIdentifier cellId,
                                   CellAutoPointer& cell );

  /** Useless methods. */
  virtual void MakeCopy( CellAutoPointer& cell ) const { (void)cell; }

  /** Iterator-related methods. */
  virtual void SetPointIds( PointIdConstIterator first );
  virtual void SetPointIds( PointIdConstIterator first,
                            PointIdConstIterator last );
  virtual void SetPointId( int localId, PointIdentifier pId );

  virtual PointIdIterator PointIdsBegin();
  virtual PointIdIterator PointIdsEnd();

  virtual PointIdConstIterator GetPointIds() const;
  virtual PointIdConstIterator PointIdsBegin() const;
  virtual PointIdConstIterator PointIdsEnd() const;

private:
  QuadEdgeMeshLineCell( const Self& );  //purposely not implemented
  void operator=( const Self& ); //purposely not implemented

  /**
   * In order to have constant time access at the itk level instead of 
   * of doing a search in the Mesh::Cell container.
   */
  CellIdentifier m_Identifier;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkQuadEdgeMeshLineCell.txx"
#endif 

#endif 

