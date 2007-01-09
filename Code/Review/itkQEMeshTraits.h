// -------------------------------------------------------------------------
// itkQEMeshTraits.h
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

#ifndef __ITKQUADEDGEMESH__MESHTRAITS__H__
#define __ITKQUADEDGEMESH__MESHTRAITS__H__

#include <set>
#include <itkCellInterface.h>
#include <itkMapContainer.h>
#include "itkQEPoint.h"
#include "itkQEQuadEdgeGeom.h"

namespace itkQE
{
/**
 */
template< int VPointDimension, typename TCoordRep,
          typename TInterpolationWeight, typename TPointIdentifier,
          typename TCellIdentifier, typename TCellFeatureIdentifier,
          typename TPoint, typename TPointsContainer,
          typename TUsingCellsContainer, typename TQE >
class CellTraitsInfo
{
    public:
    itkStaticConstMacro( PointDimension, unsigned int, VPointDimension );
    typedef TCoordRep               CoordRepType;
    typedef TInterpolationWeight    InterpolationWeightType;
    typedef TPointIdentifier        PointIdentifier;
    typedef TCellIdentifier         CellIdentifier;
    typedef TCellFeatureIdentifier  CellFeatureIdentifier;
    typedef TPoint                  PointType;
    typedef TPointsContainer        PointsContainer;
    typedef TUsingCellsContainer    UsingCellsContainer;

    /** Iterator types. */
    typedef TQE                             QuadEdgeType;
    typedef typename TQE::IteratorGeom      PointIdIterator;
    typedef typename TQE::ConstIteratorGeom PointIdConstIterator;
};

/**
 * Traits for a QE-based itk::Mesh.
 */
template< typename TPixel, unsigned int VPointDimension,
          typename TPData, typename TDData >
class MeshTraits
{
    public:
    /** Basic types for a mesh trait class. */
    typedef MeshTraits Self;
    typedef TPixel     PixelType;
    typedef TPixel     CellPixelType;
    typedef TPixel     CoordRepType;
    typedef TPixel     InterpolationWeightType;

    itkStaticConstMacro( PointDimension, unsigned int, VPointDimension );
    itkStaticConstMacro( MaxTopologicalDimension, unsigned int,
                         VPointDimension );

    typedef unsigned long PointIdentifier;
    typedef unsigned long CellIdentifier;
    typedef unsigned long CellFeatureIdentifier;

    typedef std::set< CellIdentifier > UsingCellsContainer;
    typedef std::set< CellIdentifier > PointCellLinksContainer;

    /** Quad edge typedefs. */
    typedef TPData PrimalDataType;
    typedef TDData DualDataType;
    typedef QuadEdgeGeom< PointIdentifier, CellIdentifier,
                          PrimalDataType, DualDataType > QEPrimal;
    //typedef QEPrimal QEType;
    typedef typename QEPrimal::Dual             QEDual;
    /// FOR LEO typedef typename QEPrimal::Superclass     QEType;
    /// FOR LEO typedef typename QEPrimal::Dual           QEDual;
    typedef typename QEPrimal::OrgRefType     VertexRefType;
    typedef typename QEPrimal::DualOrgRefType FaceRefType;

    /** Points have an entry in the Onext ring */
    typedef Point< CoordRepType, VPointDimension, QEPrimal >  PointType;
    typedef itk::MapContainer< PointIdentifier, PointType >   PointsContainer;

    /** Standard itk cell interface. */
    typedef CellTraitsInfo< VPointDimension,         CoordRepType,
                            InterpolationWeightType, PointIdentifier,
                            CellIdentifier,          CellFeatureIdentifier,
                            PointType,               PointsContainer,
                            UsingCellsContainer,     QEPrimal > CellTraits;
    typedef itk::CellInterface< CellPixelType, CellTraits > CellType;
    typedef typename CellType::CellAutoPointer              CellAutoPointer;

    /** Containers types. */
    typedef itk::MapContainer< PointIdentifier,
                               PointCellLinksContainer >       CellLinksContainer;
    typedef itk::MapContainer< CellIdentifier, CellType* >     CellsContainer;
    typedef itk::MapContainer< PointIdentifier, PixelType >    PointDataContainer;
    typedef itk::MapContainer< CellIdentifier, CellPixelType > CellDataContainer;

    /** Other useful types. */
    typedef typename PointType::VectorType VectorType;
};

} // enamespace

#endif // __ITKQUADEDGEMESH__MESHTRAITS__H__

// eof - itkQEMeshTraits.h
