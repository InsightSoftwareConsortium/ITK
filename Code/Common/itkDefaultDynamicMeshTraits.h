/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDefaultDynamicMeshTraits.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkDefaultDynamicMeshTraits_h
#define __itkDefaultDynamicMeshTraits_h

#include "itkCellInterface.h"
#include "itkMapContainer.h"
#include <set>

namespace itk
{

/** \class DefaultDynamicMeshTraits
 * DefaultDynamicMeshTraits is a simple structure that holds type information
 * for a mesh and its cells.  It is used to avoid the passing of many
 * template parameters while still enjoying the benefits of generic
 * programming.
 *
 * Unlike DefaultStaticMeshTraits, this version of the MeshTraits structure is designed
 * to create Mesh instances that will have many insert and delete operations
 * done on them.
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
 */
  
template <
  typename TPixelType,
  int VPointDimension = 3,
  int VMaxTopologicalDimension = VPointDimension,
  typename TCoordRep = float,
  typename TInterpolationWeight = float
  >
class DefaultDynamicMeshTraits
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef DefaultDynamicMeshTraits  Self;
  
  /**
   * Just save all the template parameters.
   */
  typedef TPixelType  PixelType;
  enum { PointDimension = VPointDimension };
  enum { MaxTopologicalDimension = VMaxTopologicalDimension };  
  typedef TCoordRep  CoordRepType;
  typedef TInterpolationWeight  InterpolationWeightType;
  
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
  typedef Point< CoordRepType, PointDimension >  PointType;

  /**
   * The container type for use in storing points.  It must conform to
   * the IndexedContainer interface.
   */
  typedef MapContainer< PointIdentifier , PointType >  PointsContainer;

  /**
   * The container type that will be used to store boundary links
   * back to cells.  This must conform to the STL "set" interface.
   */
  typedef std::set< CellIdentifier >            UsingCellsContainer;
  
  /**
   * The information needed for a cell type is now defined, so we can
   * define the cell type.
   */
  typedef MakeCellTraitsMacro                           CellTraits;
  
  /**
   * The interface to cells to be used by the mesh.
   * This should not be changed.
   */
  typedef CellInterface< PixelType , CellTraits >  Cell;
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
