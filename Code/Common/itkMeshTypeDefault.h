/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshTypeDefault.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * itkMeshTypeDefault is a simple structure that holds type information
 * for a mesh and its cells.  It is used to avoid the passing of many
 * template parameters while still enjoying the benefits of generic
 * programming.
 */

#ifndef __itkMeshTypeDefault_h
#define __itkMeshTypeDefault_h

#include <set>
#include "itkCellInterface.h"
#include "itkVectorContainer.h"

namespace itk
{

/**
 * Template parameters for MeshTypeDefault
 *
 * TPixelType =
 *    The type stored as data for an entity (cell, point, or boundary).
 * VPointDimension =
 *    Geometric dimension of space.
 * VMaxTopologicalDimension =
 *    Max topological dimension of a cell that can be inserted into this mesh
 * TCoordRep =
 *    Numerical type to store each coordinate value.
 */
  
template <
  typename TPixelType,
  int VPointDimension = 3,
  int VMaxTopologicalDimension = 3,
  typename TCoordRep = double
  >
class MeshTypeDefault
{
public:
  /**
   * Standard definition.
   */
  typedef MeshTypeDefault  Self;
  
  /**
   * Just save all the template parameters.
   */
  typedef TPixelType  PixelType;
  enum { PointDimension = VPointDimension };
  enum { MaxTopologicalDimension = VMaxTopologicalDimension };  
  typedef TCoordRep  CoordRep;
  
  /**
   * The types to be used for identification of points, cells, and
   * boundaries.  If a corresponding container type is provided, it must
   * be indexed by this identifier type.
   */
  typedef unsigned long  PointIdentifier;
  typedef unsigned long  CellIdentifier;
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
   * Define the container type that will be used to store points.
   */
  typedef VectorContainer< PointIdentifier , Point >  PointsContainer;

  /**
   * Define the container type that will be used to store boundary links
   * back to cells.  This must conform to the STL "set" interface.
   */
  typedef std::set< CellIdentifier >            UsingCellsContainer;
  
  /**
   * The information needed for a cell type is now defined, so we can
   * define the cell type.
   */
  typedef MakeCellType                           CellType;
  typedef CellInterface< PixelType , CellType >  Cell;
  
  /**
   * Define the container types that will be used to store:
   * Cells, CellLinks, PointData, CellData, Boundaries, and BoundaryData.
   *
   * The CellLinks container should be a container of PointCellLinksContainer,
   * which should be a container conforming to the STL "set" interface.
   */
  typedef VectorContainer< CellIdentifier , Cell::Pointer >
  CellsContainer;
  typedef std::set< CellIdentifier >
  PointCellLinksContainer;
  typedef VectorContainer< PointIdentifier , PointCellLinksContainer >
  CellLinksContainer;
  typedef VectorContainer< PointIdentifier , PixelType >
  PointDataContainer;
  typedef VectorContainer< CellIdentifier , PixelType >
  CellDataContainer;
  typedef VectorContainer< BoundaryIdentifier , Cell::Pointer >
  BoundariesContainer;
  typedef VectorContainer< BoundaryIdentifier , PixelType >
  BoundaryDataContainer;
};

} // namespace itk

#endif
