/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeshType.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkMeshType_h
#define __itkMeshType_h

/**
 * itkMeshType is a simple structure that holds type information for a mesh
 * and its cells.
 */

template <
  /**
   * Geometrical dimension of space.
   */
  int VPointDimension = 3,

  /**
   * Max topological dimension of a cell that can be inserted into this mesh.
   */
  int VMaxTopologicalDimension = 3,
  
  /**
   * Numerical type to store each coordinate value.
   */
  typename TCoordRep = double,

  /**
   * The type to be used for identification of a cell.  If a cell container
   * is provided to the mesh, it must be indexed by this type.
   */
  typename TCellIdentifier  = unsigned long,
  
  /**
   * The type to be used for identification of a point.  If a point container
   * is provided to the mesh, it must be indexed by this type.
   */
  typename TPointIdentifier = unsigned long,
  
  /**
   * The type to be used for identification of a boundary.  If any boundary
   * containers are provided to the mesh, they must be indexed by this type.
   */
  typename TBoundaryIdentifier = unsigned long,

  /**
   * A type that can be used to identifiy individual boundary features on
   * the cells.  Since this will probably be an index into a static array,
   * this will probably never change from an integer setting.
   */
  typename TCellFeatureIdentifier = unsigned long
  >
class itkMeshType
{
public:
  /**
   * Just save all the template parameters.
   */
  enum { PointDimension = VPointDimension };
  enum { MaxTopologicalDimension = VMaxTopologicalDimension };  
  typedef TCoordRep            	    CoordRep;
  typedef TCellIdentifier      	    CellIdentifier;
  typedef TPointIdentifier     	    PointIdentifier;
  typedef TBoundaryIdentifier  	    BoundaryIdentifier;
  typedef TCellFeatureIdentifier    CellFeatureIdentifier;
};

typedef itkMeshType<> itkMeshTypeDefault;

#endif
