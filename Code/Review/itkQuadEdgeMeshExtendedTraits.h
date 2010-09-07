/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshExtendedTraits.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshExtendedTraits_h
#define __itkQuadEdgeMeshExtendedTraits_h

#include "itkQuadEdgeCellTraitsInfo.h"
#include <itkMapContainer.h>
#include <set>

namespace itk
{
/**
 * \class QuadEdgeMeshExtendedTraits
 * \ingroup QEMeshObjects
 *
 * \brief Extended traits for a \ref itkQE::Mesh.
 *
 * QuadEdgeMeshExtendedTraits is a simple structure that holds type information
 * for a \ref itkQE::Mesh and its cells. It is used to avoid the passing
 * of many template parameters while still enjoying the benefits of generic
 * programming.
 *
 * @param TCoordRep
 *    Numerical type with which to represent each coordinate value.
 *
 * @param VPointDimension
 *    Geometric dimension of space.
 *
 * @param VMaxTopologicalDimension
 *    Max topological dimension of a cell that can be inserted into this mesh.
 *
 * @param TPixelType
 *    The type stored as data for vertices.
 *
 * @param TPData
 *    The type stored as data for the primal edges.
 *
 * @param TDData
 *    The type stored as data for the dual edges.
 *
 * @param TCellPixelType
 *     The type associated with every cell.
 *
 */
template<
  typename TPixelType = float,
  unsigned int VPointDimension = 3,
  unsigned int VMaxTopologicalDimension = VPointDimension,
  typename TCoordRep = float,
  typename TInterpolationWeightType = float,
  typename TCellPixelType = TPixelType,
  typename TPData = bool,
  typename TDData = bool
  >
class QuadEdgeMeshExtendedTraits
{
public:
  typedef QuadEdgeMeshExtendedTraits Self;
  /** Save the template parameters. */
  typedef TCoordRep      CoordRepType;
  typedef TPixelType     PixelType;
  typedef TPData         PrimalDataType;
  typedef TDData         DualDataType;
  typedef TCellPixelType CellPixelType;

  /** Save all the template parameters. */
  itkStaticConstMacro(PointDimension, unsigned int, VPointDimension);
  itkStaticConstMacro(MaxTopologicalDimension, unsigned int,
                      VPointDimension);

  typedef TInterpolationWeightType InterpolationWeightType;

  /** The type to be used to identify a point.  This should be the index type
   * to the PointsContainer. */
  typedef unsigned long PointIdentifier;

  /** The type to be used to identify a cell.  This should be the index type
   * to the CellsContainer. */
  typedef unsigned long CellIdentifier;

  /** A type that can be used to identifiy individual boundary features on
   * the cells.  Since this will probably be an index into a static array,
   * this will probably never change from an integer setting. */
  typedef unsigned long CellFeatureIdentifier;

  /** The container type that will be used to store boundary links
   * back to cells.  This must conform to the STL "set" interface. */
  typedef std::set< CellIdentifier > UsingCellsContainer;

  /** The CellLinks container should be a container of PointCellLinksContainer,
   * which should be a container conforming to the STL "set" interface. */
  typedef std::set< CellIdentifier > PointCellLinksContainer;

  /** Quad edge typedefs. */
  typedef GeometricalQuadEdge< PointIdentifier, CellIdentifier, PrimalDataType, DualDataType > QEPrimal;
  typedef typename QEPrimal::DualType                                                          QEDual;
  typedef typename QEPrimal::OriginRefType                                                     VertexRefType;
  typedef typename QEPrimal::DualOriginRefType                                                 FaceRefType;

  /** The type of point used by the mesh. This should never change from
   * this setting, regardless of the mesh type. Points have an entry
   * in the Onext ring */
  typedef QuadEdgeMeshPoint<
    CoordRepType, VPointDimension, QEPrimal >                 PointType;

  typedef Point< CoordRepType, VPointDimension > PointHashType;

  /** The container type for use in storing points. It must conform to
   * the IndexedContainer interface. */
  typedef MapContainer< PointIdentifier, PointType > PointsContainer;

  /** Standard itk cell interface. */
  typedef QuadEdgeMeshCellTraitsInfo<
    VPointDimension, CoordRepType,
    InterpolationWeightType, PointIdentifier,
    CellIdentifier,          CellFeatureIdentifier,
    PointType,               PointsContainer,
    UsingCellsContainer,     QEPrimal >                       CellTraits;

  /** The interface to cells to be used by the mesh. */
  typedef CellInterface< CellPixelType, CellTraits > CellType;
  typedef typename CellType::CellAutoPointer         CellAutoPointer;

  /** Containers types. */
  typedef MapContainer< PointIdentifier, PointCellLinksContainer > CellLinksContainer;
  typedef MapContainer< CellIdentifier, CellType * >               CellsContainer;
  typedef MapContainer< PointIdentifier, PixelType >               PointDataContainer;
  typedef MapContainer< CellIdentifier, CellPixelType >            CellDataContainer;

  /** Other useful types. */
  typedef typename PointType::VectorType VectorType;
};
} // enamespace

#endif

// eof - itkQuadEdgeMeshExtendedTraits.h
