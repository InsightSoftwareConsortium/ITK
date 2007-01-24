// -------------------------------------------------------------------------
// itkQuadEdgeMeshPolygonCell.h
// $Revision: 1.2 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-24 22:52:30 $
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

#ifndef __ITKQUADEDGEMESH__POLYGONCELL__H__
#define __ITKQUADEDGEMESH__POLYGONCELL__H__

namespace itkQE
{

/**
 * Class that connects the QE with itk
 *
 * \param TCellInterface Basic type for the itk*Cell. This usually comes
 *        from the MeshTraits.
 */
template< class TCellInterface >
    class PolygonCell
    : public TCellInterface
{
   public:
   /** Standard class typedefs. */
   // itkCellCommonTypedefs
   typedef PolygonCell                    Self;
   typedef itk::AutoPointer< const Self > ConstSelfAutoPointer;
   typedef itk::AutoPointer< Self >       SelfAutoPointer;
   typedef Self*                          RawPointer;
   typedef const Self*                    ConstRawPointer;

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
   typedef typename QEType::Dual                          QEDual;

   /** Iterator types. */
   typedef typename QEType::IteratorGeom      PointIdIterator;
   typedef typename QEType::ConstIteratorGeom PointIdConstIterator;

   public:
   /** Standard part of every itk Object. */
   itkTypeMacro( PolygonCell, TCellInterface );

   public:
   /** Object memory management methods. */
   PolygonCell( int nPoints );
   PolygonCell( QEType* e );
   virtual ~PolygonCell( ) { }

   /** Accessors for m_Ident. */
   void SetIdent( CellIdentifier cid ) { m_Ident = cid; }
   CellIdentifier GetIdent( )          { return( m_Ident ); }

   /** Lnext ring entry accessors. */
   QEType* GetEdgeRingEntry( ) const      { return( m_EdgeRingEntry ); }
   void SetEdgeRingEntry( QEType* entry ) { m_EdgeRingEntry = entry; }

   /** Implement the standard CellInterface. */
   SelfAutoPointer New( );

   /** TCellInterface abstract methods definition. */
   virtual void Accept( unsigned long cellId, MultiVisitor* mv );
   virtual CellGeometry GetType( ) const { return( Superclass::POLYGON_CELL );}

   /** itk topology related methods. */
   static int GetTopologyId( )           { return( Superclass::POLYGON_CELL );}
   virtual unsigned int GetDimension( ) const { return( Self::CellDimension );}
   virtual unsigned int GetNumberOfPoints( ) const;
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

   virtual PointIdIterator PointIdsBegin( );
   virtual PointIdIterator PointIdsEnd( );

   virtual PointIdConstIterator GetPointIds( ) const;
   virtual PointIdConstIterator PointIdsBegin( ) const;
   virtual PointIdConstIterator PointIdsEnd( ) const;

   private:
   PolygonCell( const Self& );    // Not impl.
   void operator=( const Self& ); // Not impl.

   private:
   /**
    * In order to have constant time access at the itk level instead of 
    * of doing a search in the Mesh::Cell container.
    */
   CellIdentifier m_Ident;

   /**
    * Entry point into the edge ring.
    */
   QEType* m_EdgeRingEntry;
};

} // enamespace

#include "itkQuadEdgeMeshPolygonCell.txx"

#endif // __ITKQUADEDGEMESH__POLYGONCELL__H__

// eof - itkQuadEdgeMeshPolygonCell.h
