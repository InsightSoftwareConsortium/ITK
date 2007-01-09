// -------------------------------------------------------------------------
// itkQEMeshWithDual.h
// $Revision: 1.1 $
// $Author: sylvain $
// $Name:  $
// $Date: 2007-01-09 00:58:17 $
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

#ifndef __ITKQUADEDGEMESHWITHDUAL__MESH__H__
#define __ITKQUADEDGEMESHWITHDUAL__MESH__H__

#include "itkPointSet.h"
#include "itkQEMesh.h"
#include "itkQEMeshTraits.h"

namespace itkQE
{
/**
* QE-based itk::Mesh.
*/
template< typename TPixel, unsigned int VDimension,
   typename TTraits = itkQE::MeshTraits< TPixel, VDimension, bool, bool > >
class MeshWithDual
: public itkQE::Mesh< TPixel, VDimension, TTraits >
{
   public:
   /** Input template parameters. */
   typedef TTraits Traits;
   typedef TPixel  PixelType;

   /** Standard typedefs. */
   typedef MeshWithDual                               Self;
   typedef itkQE::Mesh< TPixel, VDimension, Traits >  Superclass;
   typedef itk::SmartPointer< Self >                  Pointer;
   typedef itk::SmartPointer< const Self >            ConstPointer;

   /** Convenient constants obtained from MeshTraits. */
   itkStaticConstMacro( PointDimension, unsigned int,Traits::PointDimension );
   itkStaticConstMacro( MaxTopologicalDimension, unsigned int,Traits::MaxTopologicalDimension );

   /** Types defined in superclass. */
   typedef typename Superclass::RegionType                          RegionType;
   typedef typename Superclass::CellPixelType                       CellPixelType;
   typedef typename Superclass::CoordRepType                        CoordRepType;
   typedef typename Superclass::InterpolationWeightType             InterpolationWeightType;
   typedef typename Superclass::PointIdentifier                     PointIdentifier;
   typedef typename Superclass::CellIdentifier                      CellIdentifier;
   typedef typename Superclass::CellFeatureIdentifier               CellFeatureIdentifier;
   typedef typename Superclass::PointType                           PointType;
   typedef typename Superclass::PointsContainer                     PointsContainer;
   typedef typename Superclass::CellTraits                          CellTraits;
   typedef typename Superclass::CellsContainer                      CellsContainer;
   typedef typename Superclass::PointCellLinksContainer             PointCellLinksContainer;
   typedef typename Superclass::CellLinksContainer                  CellLinksContainer;
   typedef typename Superclass::PointDataContainer                  PointDataContainer;
   typedef typename Superclass::CellDataContainer                   CellDataContainer;
   typedef typename Superclass::PointLocatorType                    PointLocatorType;
   typedef typename Superclass::BoundingBoxType                     BoundingBoxType;
   typedef typename Superclass::PointsContainerPointer              PointsContainerPointer;
   typedef typename Superclass::CellsContainerPointer               CellsContainerPointer;
   typedef typename Superclass::CellLinksContainerPointer           CellLinksContainerPointer;
   typedef typename Superclass::PointDataContainerPointer           PointDataContainerPointer;
   typedef typename Superclass::CellDataContainerPointer            CellDataContainerPointer;
   typedef typename Superclass::PointLocatorPointer                 PointLocatorPointer;
   typedef typename Superclass::BoundingBoxPointer                  BoundingBoxPointer;
   typedef typename Superclass::PointsContainerConstIterator        PointsContainerConstIterator;
   typedef typename Superclass::PointsContainerIterator             PointsContainerIterator;
   typedef typename Superclass::CellsContainerConstIterator         CellsContainerConstIterator;
   typedef typename Superclass::CellsContainerIterator              CellsContainerIterator;
   typedef typename Superclass::CellLinksContainerIterator          CellLinksContainerIterator;
   typedef typename Superclass::PointDataContainerIterator          PointDataContainerIterator;
   typedef typename Superclass::CellDataContainerIterator           CellDataContainerIterator;
   typedef typename Superclass::PointCellLinksContainerIterator     PointCellLinksContainerIterator;
   typedef typename Superclass::CellFeatureCount                    CellFeatureCount;
   typedef typename Superclass::CellType                            CellType;
   typedef typename Superclass::CellAutoPointer                     CellAutoPointer;
   typedef typename Superclass::CellMultiVisitorType                CellMultiVisitorType;
   typedef typename Superclass::BoundaryAssignmentsContainer        BoundaryAssignmentsContainer;
   typedef typename Superclass::BoundaryAssignmentsContainerPointer BoundaryAssignmentsContainerPointer;
   typedef typename Superclass::BoundaryAssignmentsContainerVector  BoundaryAssignmentsContainerVector;

   /** Specific types for a quad-edge structure. */
   typedef typename Traits::PrimalDataType   PrimalDataType;
   typedef typename Traits::DualDataType     DualDataType;
   typedef typename Traits::QEPrimal         QEPrimal;
   typedef typename Traits::QEDual           QEDual;
   typedef typename Traits::VertexRefType    VertexRefType;
   typedef typename Traits::FaceRefType      FaceRefType;
   typedef typename Traits::VectorType       VectorType;

   /** Possible specialized cell types. */
   typedef typename Superclass::EdgeCellType    EdgeCellType;
   typedef typename Superclass::PolygonCellType PolygonCellType;

   /** Free insertion indexes. */
   typedef typename Superclass::FreePointIndexesType  FreePointIndexesType;
   typedef typename Superclass::FreeCellIndexesType   FreeCellIndexesType;

   /** Auxiliary type. */
   typedef typename Superclass::PointIdList              PointIdList;
   typedef typename QEPrimal::IteratorGeom               QEIterator;
   typedef typename Superclass::Superclass::Superclass   PointSetType;
   typedef typename PointSetType::Pointer                PointSetTypePointer;

   public:
   /** Basic itk::Object interface. */
   itkNewMacro( Self );
   itkTypeMacro( MeshWithDual, itkQE::Mesh );

   virtual QEPrimal* AddFace( PointIdList& points );

   unsigned int GetNumberOfDualPoints( ) const 
   { return m_DualPointsSet->GetNumberOfPoints( ); };

   PointType GetDualPoint( PointIdentifier pid ) 
   { return m_DualPointsSet->GetPoints( )->GetElement( pid ); };

   protected:
   /** Memory management methods. */
   MeshWithDual( );
   virtual ~MeshWithDual( ) { };

   private:
   MeshWithDual( const Self& );  // Not impl.
   void operator=( const Self& );// Not impl.

   protected:
   /** quick an nasty : second point set for dual geometry */
   PointSetTypePointer m_DualPointsSet;

};

}

#include "itkQEMeshWithDual.txx"

#endif
