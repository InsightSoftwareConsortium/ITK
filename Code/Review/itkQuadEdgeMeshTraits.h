/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshTraits.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshTraits_h
#define __itkQuadEdgeMeshTraits_h

#include <set>
#include <itkCellInterface.h>
#include <itkMapContainer.h>
#include "itkQuadEdgeMeshPoint.h"
#include "itkGeometricalQuadEdge.h"
#include "itkQuadEdgeCellTraitsInfo.h"

namespace itk
{
/** \class QuadEdgeMeshTraits
 *  \brief Class holding the traits of the QuadEdgeMesh.
 *
 *  This class is a variant of the MeshTraits that adds the traits
 *  defined in the QuadEdgeMeshCellTraitsInfo class.
 *
 * \author Alexandre Gouaillard, Leonardo Florez-Valencia, Eric Boix
 *
 * This implementation was contributed as a paper to the Insight Journal
 * http://insight-journal.org/midas/handle.php?handle=1926/306
 *
 *  \sa DefaultDynamicMeshTraits
 *  \sa DefaultStaticMeshTraits
 */
template< typename TPixel, unsigned int VPointDimension,
          typename TPData, typename TDData,
          typename TCoordRep = float, typename TInterpolationWeight = float >
class QuadEdgeMeshTraits
{
public:
  /** Basic types for a mesh trait class. */
  typedef QuadEdgeMeshTraits   Self;
  typedef TPixel               PixelType;
  typedef TPixel               CellPixelType;
  typedef TCoordRep            CoordRepType;
  typedef TInterpolationWeight InterpolationWeightType;

  itkStaticConstMacro(PointDimension, unsigned int, VPointDimension);
  itkStaticConstMacro(MaxTopologicalDimension, unsigned int,
                      VPointDimension);

  typedef unsigned long PointIdentifier;
  typedef unsigned long CellIdentifier;
  typedef unsigned char CellFeatureIdentifier; // made small in purpose

  typedef std::set< CellIdentifier > UsingCellsContainer;
  typedef std::set< CellIdentifier > PointCellLinksContainer;

  /** Quad edge typedefs. */
  typedef TPData PrimalDataType;
  typedef TDData DualDataType;
  typedef GeometricalQuadEdge< PointIdentifier, CellIdentifier,
                               PrimalDataType, DualDataType > QEPrimal;
  //typedef QEPrimal QEType;
  typedef typename QEPrimal::DualType QEDual;
  // FOR LEO typedef typename QEPrimal::Superclass     QEType;
  // FOR LEO typedef typename QEPrimal::Dual           QEDual;
  typedef typename QEPrimal::OriginRefType     VertexRefType;
  typedef typename QEPrimal::DualOriginRefType FaceRefType;

  /** The type of point used for hashing.  This should never change from
   * this setting, regardless of the mesh type. */
  typedef Point< CoordRepType, VPointDimension > PointHashType;

  /** Points have an entry in the Onext ring */
  typedef QuadEdgeMeshPoint< CoordRepType, VPointDimension, QEPrimal > PointType;
  typedef MapContainer< PointIdentifier, PointType >                   PointsContainer;

  /** Standard cell interface. */
  typedef QuadEdgeMeshCellTraitsInfo<
    VPointDimension, CoordRepType,
    InterpolationWeightType, PointIdentifier,
    CellIdentifier,          CellFeatureIdentifier,
    PointType,               PointsContainer,
    UsingCellsContainer,     QEPrimal >                 CellTraits;

  typedef CellInterface< CellPixelType, CellTraits > CellType;
  typedef typename CellType::CellAutoPointer         CellAutoPointer;

  /** Containers types. */
  typedef MapContainer< PointIdentifier,
                        PointCellLinksContainer >       CellLinksContainer;
  typedef MapContainer< CellIdentifier, CellType * >    CellsContainer;
  typedef MapContainer< PointIdentifier, PixelType >    PointDataContainer;
  typedef MapContainer< CellIdentifier, CellPixelType > CellDataContainer;

  /** Other useful types. */
  typedef typename PointType::VectorType VectorType;
};
}

#endif
