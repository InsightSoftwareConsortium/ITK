/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDefaultDynamicMeshTraits.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDefaultDynamicMeshTraits_h
#define __itkDefaultDynamicMeshTraits_h

#include "itkCellInterface.h"
#include "itkMapContainer.h"
#include "itkPoint.h"
#include <set>

namespace itk
{

/** \class DefaultDynamicMeshTraits
 * DefaultDynamicMeshTraits is a simple structure that holds type information
 * for a mesh and its cells.  It is used to avoid the passing of many
 * template parameters while still enjoying the benefits of generic
 * programming.
 *
 * Unlike DefaultStaticMeshTraits, this version of the MeshTraits structure
 * is designed to create Mesh instances that will have many insert and delete
 * operations done on them.
 *
 * Template parameters for DefaultDynamicMeshTraits:
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
 * 
 * \ingroup MeshObjects 
 */
template <
  typename TPixelType,
  unsigned int VPointDimension = 3,
  unsigned int VMaxTopologicalDimension = VPointDimension,
  typename TCoordRep = float,
  typename TInterpolationWeight = float,
  typename TCellPixelType = TPixelType
  >
class DefaultDynamicMeshTraits
{
public:
  /** Standard class typedefs. */
  typedef DefaultDynamicMeshTraits  Self;
  
  /** Just save all the template parameters. */
  typedef TPixelType      PixelType;
  typedef TCellPixelType  CellPixelType;
  typedef TCoordRep  CoordRepType;
  typedef TInterpolationWeight  InterpolationWeightType;
    
  /** Just save all the template parameters. */
  itkStaticConstMacro(PointDimension, unsigned int, VPointDimension);
  itkStaticConstMacro(MaxTopologicalDimension, unsigned int,
                      VMaxTopologicalDimension);  
  
  /** The type to be used to identify a point.  This should be the index type
   * to the PointsContainer. */
  typedef unsigned long  PointIdentifier;

  /** The type to be used to identify a cell.  This should be the index type
   * to the CellsContainer. */
  typedef unsigned long  CellIdentifier;

  /** A type that can be used to identifiy individual boundary features on
   * the cells.  Since this will probably be an index into a static array,
   * this will probably never change from an integer setting. */
  typedef unsigned long  CellFeatureIdentifier;
  
  /** The type of point used by the mesh.  This should never change from
   * this setting, regardless of the mesh type. */
  typedef Point< CoordRepType, VPointDimension >  PointType;

  /** The container type for use in storing points.  It must conform to
   * the IndexedContainerInterface. */
  typedef MapContainer< PointIdentifier , PointType >  PointsContainer;

  /** The container type that will be used to store boundary links
   * back to cells.  This must conform to the STL "set" interface. */
  typedef std::set< CellIdentifier >            UsingCellsContainer;
  
  /** The information needed for a cell type is now defined, so we can
   * define the cell type. */
  typedef itkMakeCellTraitsMacro                           CellTraits;
  
  /** The interface to cells to be used by the mesh.
   * This should not be changed. */
  typedef CellInterface< CellPixelType , CellTraits >  CellType;
  typedef typename CellType::CellAutoPointer           CellAutoPointer;
  
  /** The container type for use in storing cells.  It must conform to
   * the IndexedContainerInterface. */
  typedef MapContainer< CellIdentifier , CellType * >
        CellsContainer;
  
  /** The CellLinks container should be a container of PointCellLinksContainer,
   * which should be a container conforming to the STL "set" interface. */
  typedef std::set< CellIdentifier >
        PointCellLinksContainer;

  /** The container type for use in storing point links back to cells.]
   * It must conform to the IndexedContainerInterface. */
  typedef MapContainer< PointIdentifier , PointCellLinksContainer >
        CellLinksContainer;

  /** The container type for use in storing point data.  It must conform to
   * the IndexedContainerInterface. */
  typedef MapContainer< PointIdentifier , PixelType >
        PointDataContainer;

  /** The container type for use in storing cell data.  It must conform to
   * the IndexedContainerInterface. */
  typedef MapContainer< CellIdentifier , CellPixelType >
        CellDataContainer;

};

} // end namespace itk

#endif
