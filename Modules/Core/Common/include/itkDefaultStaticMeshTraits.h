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
#ifndef itkDefaultStaticMeshTraits_h
#define itkDefaultStaticMeshTraits_h

#include "itkCellInterface.h"
#include "itkVectorContainer.h"
#include "itkPoint.h"
#include "itkIntTypes.h"
#include <set>

namespace itk
{
/** \class DefaultStaticMeshTraits
 * DefaultStaticMeshTraits is a simple structure that holds type information
 * for a mesh and its cells.  It is used to avoid the passing of many
 * template parameters while still enjoying the benefits of generic
 * programming.
 *
 * Template parameters for DefaultStaticMeshTraits:
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
 *    Numerical type with which to represent each coordinate value.
 *
 * TInterpolationWeight =
 *    Numerical type to store interpolation weights.
 *
 * \ingroup MeshObjects
 * \ingroup ITKCommon
 */
template <typename TPixelType,
          unsigned int VPointDimension = 3,
          unsigned int VMaxTopologicalDimension = VPointDimension,
          typename TCoordRep = float,
          typename TInterpolationWeight = float,
          typename TCellPixelType = TPixelType>
class ITK_TEMPLATE_EXPORT DefaultStaticMeshTraits
{
public:
  /** Standard class type aliases. */
  using Self = DefaultStaticMeshTraits;

  /** Just save all the template parameters. */
  using PixelType = TPixelType;
  using CellPixelType = TCellPixelType;
  using CoordRepType = TCoordRep;
  using InterpolationWeightType = TInterpolationWeight;

  /** Just save all the template parameters. */
  static constexpr unsigned int PointDimension = VPointDimension;
  static constexpr unsigned int MaxTopologicalDimension = VMaxTopologicalDimension;

  /** The type to be used to identify a point.  This should be the index type
   * to the PointsContainer. */
  using PointIdentifier = IdentifierType;

  /** The type to be used to identify a cell.  This should be the index type
   * to the CellsContainer. */
  using CellIdentifier = IdentifierType;

  /** A type that can be used to identifiy individual boundary features on
   * the cells.  Since this will probably be an index into a static array,
   * this will probably never change from an integer setting. */
  using CellFeatureIdentifier = IdentifierType;

  /** The type of point used by the mesh. */
  using PointType = Point<CoordRepType, VPointDimension>;

  /** The type of point used for hashing.  This should never change from
   * this setting, regardless of the mesh type. */
  using PointHashType = Point<CoordRepType, VPointDimension>;

  /** The container type for use in storing points.  It must conform to
   * the IndexedContainer interface. */
  using PointsContainer = VectorContainer<PointIdentifier, PointType>;

  /** The container type that will be used to store boundary links
   * back to cells.  This must conform to the STL "set" interface. */
  using UsingCellsContainer = std::set<CellIdentifier>;

  /** The information needed for a cell type is now defined, so we can
   * define the cell type. We use a macro defined in itkCellInterface. */
  using CellTraits = itkMakeCellTraitsMacro;

  /** The interface to cells to be used by the mesh.
   * This should not be changed. */
  using CellType = CellInterface<CellPixelType, CellTraits>;
  using CellRawPointer = typename CellType::CellRawPointer;
  using CellAutoPointer = typename CellType::CellAutoPointer;

  /** The container type for use in storing cells.  It must conform to
   * the IndexedContainer interface. */
  using CellsContainer = VectorContainer<CellIdentifier, CellType *>;

  /** The CellLinks container should be a container of PointCellLinksContainer,
   * which should be a container conforming to the STL "set" interface. */
  using PointCellLinksContainer = std::set<CellIdentifier>;

  /** The container type for use in storing point links back to cells.
   * It must conform to the IndexedContainer interface. */
  using CellLinksContainer = VectorContainer<PointIdentifier, PointCellLinksContainer>;

  /** The container type for use in storing point data.  It must conform to
   * the IndexedContainer interface. */
  using PointDataContainer = VectorContainer<PointIdentifier, PixelType>;

  /** The container type for use in storing cell data.  It must conform to
   * the IndexedContainer interface. */
  using CellDataContainer = VectorContainer<CellIdentifier, CellPixelType>;
};
} // end namespace itk

#endif
