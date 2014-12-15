/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkDefaultDynamicMeshTraits_h
#define itkDefaultDynamicMeshTraits_h

#include "itkCellInterface.h"
#include "itkMapContainer.h"
#include "itkPoint.h"
#include "itkIntTypes.h"
#include <set>

namespace itk
{
/** \class DefaultDynamicMeshTraits
 *  \brief A simple structure that holds type information for a mesh and its cells.
 *
 * DefaultDynamicMeshTraits is a simple structure that holds type information
 * for a mesh and its cells.  It is used to avoid the passing of many
 * template parameters while still enjoying the benefits of generic
 * programming.
 *
 * Unlike DefaultStaticMeshTraits, this version of the MeshTraits structure
 * is designed to create Mesh instances that will have many insert and delete
 * operations done on them.
 *
 * \tparam TPixelType The type stored as data for an entity (cell, point,
 * or boundary).
 *
 * \tparam VPointDimension Geometric dimension of space.
 *
 * \tparam VMaxTopologicalDimension Max topological dimension of a cell
 * that can be inserted into this mesh.
 *
 * \tparam TCoordRep Numerical type to store each coordinate value.
 *
 * \tparam TInterpolationWeight Numerical type to store interpolation
 * weights.
 *
 * \ingroup MeshObjects
 * \ingroup ITKCommon
 */
template<
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
  typedef DefaultDynamicMeshTraits Self;

  /** Just save all the template parameters. */
  typedef TPixelType           PixelType;
  typedef TCellPixelType       CellPixelType;
  typedef TCoordRep            CoordRepType;
  typedef TInterpolationWeight InterpolationWeightType;

  /** Just save all the template parameters. */
  itkStaticConstMacro(PointDimension, unsigned int, VPointDimension);
  itkStaticConstMacro(MaxTopologicalDimension, unsigned int,
                      VMaxTopologicalDimension);

  /** The type to be used to identify a point.  This should be the index type
   * to the PointsContainer. */
  typedef IdentifierType PointIdentifier;

  /** The type to be used to identify a cell.  This should be the index type
   * to the CellsContainer. */
  typedef IdentifierType CellIdentifier;

  /** A type that can be used to identifiy individual boundary features on
   * the cells.  Since this will probably be an index into a static array,
   * this will probably never change from an integer setting. */
  typedef IdentifierType CellFeatureIdentifier;

  /** The type of point used by the mesh. */
  typedef Point< CoordRepType, VPointDimension > PointType;

  /** The type of point used for hashing.  This should never change from
   * this setting, regardless of the mesh type. */
  typedef Point< CoordRepType, VPointDimension > PointHashType;

  /** The container type for use in storing points.  It must conform to
   * the IndexedContainerInterface. */
  typedef MapContainer< PointIdentifier, PointType > PointsContainer;

  /** The container type that will be used to store boundary links
   * back to cells.  This must conform to the STL "set" interface. */
  typedef std::set< CellIdentifier > UsingCellsContainer;

  /** The information needed for a cell type is now defined, so we can
   * define the cell type. */
  typedef itkMakeCellTraitsMacro CellTraits;

  /** The interface to cells to be used by the mesh.
   * This should not be changed. */
  typedef CellInterface< CellPixelType, CellTraits > CellType;
  typedef typename CellType::CellAutoPointer         CellAutoPointer;

  /** The container type for use in storing cells.  It must conform to
   * the IndexedContainerInterface. */
  typedef MapContainer< CellIdentifier, CellType * > CellsContainer;

  /** The CellLinks container should be a container of PointCellLinksContainer,
   * which should be a container conforming to the STL "set" interface. */
  typedef std::set< CellIdentifier > PointCellLinksContainer;

  /** The container type for use in storing point links back to cells.]
   * It must conform to the IndexedContainerInterface. */
  typedef MapContainer<
    PointIdentifier, PointCellLinksContainer >        CellLinksContainer;

  /** The container type for use in storing point data.  It must conform to
   * the IndexedContainerInterface. */
  typedef MapContainer< PointIdentifier, PixelType > PointDataContainer;

  /** The container type for use in storing cell data.  It must conform to
   * the IndexedContainerInterface. */
  typedef MapContainer<
    CellIdentifier, CellPixelType >                   CellDataContainer;
};
} // end namespace itk

#endif
