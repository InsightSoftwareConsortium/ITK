/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshTypeDefaultDynamic.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkMeshTypeDefaultDynamic_h
#define __itkMeshTypeDefaultDynamic_h

#include "itkCellInterface.h"
#include "itkMapContainer.h"
#include <set>

namespace itk
{

/** \class MeshTypeDefaultDynamic
 * MeshTypeDefaultDynamic is a simple structure that holds type information
 * for a mesh and its cells.  It is used to avoid the passing of many
 * template parameters while still enjoying the benefits of generic
 * programming.
 *
 * Unlike MeshTypeDefault, this version of the MeshType structure is designed
 * to create Mesh instances that will have many insert and delete operations
 * done on them.
 *
 * Template parameters for MeshTypeDefaultDynamic:
 *
 * TPixelType =
 *    The type stored as data for an entity (cell, point, or boundary).
 *
 * VPointDimension =
 *    Geometric dimension of space.
 *
 * VMaxTopologicalDimension =
 *    Max topological dimension of a cell that can be inserted into this mesh.
 *
 * TCoordRep =
 *    Numerical type to store each coordinate value.
 *
 * TInterpolationWeight =
 *    Numerical type to store interpolation weights.
 */
  
template <
  typename TPixelType,
  int VPointDimension = 3,
  int VMaxTopologicalDimension = 3,
  typename TCoordRep = float,
  typename TInterpolationWeight = float
  >
class MeshTypeDefaultDynamic
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef MeshTypeDefaultDynamic  Self;
  
  /**
   * Just save all the template parameters.
   */
  typedef TPixelType  PixelType;
  enum { PointDimension = VPointDimension };
  enum { MaxTopologicalDimension = VMaxTopologicalDimension };  
  typedef TCoordRep  CoordRep;
  typedef TInterpolationWeight  InterpolationWeight;
  
  /**
   * The type to be used to identify a point.  This should be the index type
   * to the PointsContainer.
   */
  typedef unsigned long  PointIdentifier;

  /**
   * The type to be used to identify a cell.  This should be the index type
   * to the CellsContainer.
   */
  typedef unsigned long  CellIdentifier;

  /**
   * The type to be used to identify a boundary.  This should be the index type
   * to the BoundariesContainer.
   */
  typedef unsigned long  BoundaryIdentifier;

  /**
   * A type that can be used to identifiy individual boundary features on
   * the cells.  Since this will probably be an index into a static array,
   * this will probably never change from an integer setting.
   */
  typedef unsigned long  CellFeatureIdentifier;
  
  /**
   * The type of point used by the mesh.  This should never change from
   * this setting, regardless of the mesh type.
   */
  typedef Point< PointDimension , CoordRep >  Point;

  /**
   * The container type for use in storing points.  It must conform to
   * the IndexedContainer interface.
   */
  typedef MapContainer< PointIdentifier , Point >  PointsContainer;

  /**
   * The container type that will be used to store boundary links
   * back to cells.  This must conform to the STL "set" interface.
   */
  typedef std::set< CellIdentifier >            UsingCellsContainer;
  
  /**
   * The information needed for a cell type is now defined, so we can
   * define the cell type.
   */
  typedef MakeCellType                           CellType;
  
  /**
   * The interface to cells to be used by the mesh.
   * This should not be changed.
   */
  typedef CellInterface< PixelType , CellType >  Cell;
  typedef typename Cell::Pointer CellPointer;
  
  /**
   * The container type for use in storing cells.  It must conform to
   * the IndexedContainer interface.
   */
  typedef MapContainer< CellIdentifier , CellPointer >
        CellsContainer;
  
  /**
   * The CellLinks container should be a container of PointCellLinksContainer,
   * which should be a container conforming to the STL "set" interface.
   */
  typedef std::set< CellIdentifier >
        PointCellLinksContainer;

  /**
   * The container type for use in storing point links back to cells.]
   * It must conform to the IndexedContainer interface.
   */
  typedef MapContainer< PointIdentifier , PointCellLinksContainer >
        CellLinksContainer;

  /**
   * The container type for use in storing point data.  It must conform to
   * the IndexedContainer interface.
   */
  typedef MapContainer< PointIdentifier , PixelType >
        PointDataContainer;

  /**
   * The container type for use in storing cell data.  It must conform to
   * the IndexedContainer interface.
   */
  typedef MapContainer< CellIdentifier , PixelType >
        CellDataContainer;

  /**
   * The container type for use in storing explicitly created
   * boundaries.  It must conform to the IndexedContainer interface.
   */
  typedef MapContainer< BoundaryIdentifier , CellPointer >
        BoundariesContainer;

  /**
   * The container type for use in storing data for explicitly
   * created boundaries.  It must conform to the IndexedContainer interface.
   */
  typedef MapContainer< BoundaryIdentifier , PixelType >
        BoundaryDataContainer;
};

} // end namespace itk

#endif
