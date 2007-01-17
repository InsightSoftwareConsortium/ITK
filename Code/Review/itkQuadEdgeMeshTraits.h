// -------------------------------------------------------------------------
// itkQuadEdgeMeshTraits.h
// $Revision: 1.3 $
// $Author: ibanez $
// $Name:  $
// $Date: 2007-01-17 13:32:35 $
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

#ifndef __itkQuadEdgeMeshTraits_h
#define __itkQuadEdgeMeshTraits_h

#include <set>
#include <itkCellInterface.h>
#include <itkMapContainer.h>
#include "itkQuadEdgeMeshPoint.h"
#include "itkGeometricalQuadEdge.h"

namespace itk
{
/**
 */
template< int VPointDimension, typename TCoordRep,
          typename TInterpolationWeight, typename TPointIdentifier,
          typename TCellIdentifier, typename TCellFeatureIdentifier,
          typename TPoint, typename TPointsContainer,
          typename TUsingCellsContainer, typename TQE >
class QuadEdgeMeshCellTraitsInfo
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
 * Traits for a QuadEdgeMesh
 */
template< typename TPixel, unsigned int VPointDimension,
          typename TPData, typename TDData, 
          typename TCoordRep=float, typename TInterpolationWeight=float >
class QuadEdgeMeshTraits
{
public:
  /** Basic types for a mesh trait class. */
  typedef QuadEdgeMeshTraits                Self;
  typedef TPixel                            PixelType;
  typedef TPixel                            CellPixelType;
  typedef TCoordRep                         CoordRepType;
  typedef TInterpolationWeight              InterpolationWeightType;

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
  typedef GeometricalQuadEdge< PointIdentifier, CellIdentifier,
                        PrimalDataType, DualDataType > QEPrimal;
  //typedef QEPrimal QEType;
  typedef typename QEPrimal::Dual             QEDual;
  /// FOR LEO typedef typename QEPrimal::Superclass     QEType;
  /// FOR LEO typedef typename QEPrimal::Dual           QEDual;
  typedef typename QEPrimal::OrgRefType     VertexRefType;
  typedef typename QEPrimal::DualOrgRefType FaceRefType;

  /** Points have an entry in the Onext ring */
  typedef QuadEdgeMeshPoint< CoordRepType, VPointDimension, QEPrimal >  PointType;
  typedef MapContainer< PointIdentifier, PointType >   PointsContainer;

  /** Standard cell interface. */
  typedef QuadEdgeMeshCellTraitsInfo< VPointDimension, CoordRepType,
                          InterpolationWeightType, PointIdentifier,
                          CellIdentifier,          CellFeatureIdentifier,
                          PointType,               PointsContainer,
                          UsingCellsContainer,     QEPrimal > CellTraits;
  typedef CellInterface< CellPixelType, CellTraits > CellType;
  typedef typename CellType::CellAutoPointer              CellAutoPointer;

  /** Containers types. */
  typedef MapContainer< PointIdentifier,
                        PointCellLinksContainer >       CellLinksContainer;
  typedef MapContainer< CellIdentifier, CellType* >     CellsContainer;
  typedef MapContainer< PointIdentifier, PixelType >    PointDataContainer;
  typedef MapContainer< CellIdentifier, CellPixelType > CellDataContainer;

  /** Other useful types. */
  typedef typename PointType::VectorType VectorType;
};

} 

#endif 


