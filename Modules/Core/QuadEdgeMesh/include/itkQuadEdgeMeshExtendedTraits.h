/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkQuadEdgeMeshExtendedTraits_h
#define itkQuadEdgeMeshExtendedTraits_h

#include "itkCellInterface.h"
#include "itkQuadEdgeCellTraitsInfo.h"
#include <set>

namespace itk
{
/**
 * \class QuadEdgeMeshExtendedTraits
 *
 * \brief Extended traits for a QuadEdgeMesh.
 *
 * QuadEdgeMeshExtendedTraits is a simple structure that holds type information
 * for a QuadEdgeMesh and its cells. It is used to avoid the passing
 * of many template parameters while still enjoying the benefits of generic
 * programming.
 *
 * \tparam TCoordRep
 *    Numerical type with which to represent each coordinate value.
 *
 * \tparam VPointDimension
 *    Geometric dimension of space.
 *
 * \tparam VMaxTopologicalDimension
 *    Max topological dimension of a cell that can be inserted into this mesh.
 *
 * \tparam TPixelType
 *    The type stored as data for vertices.
 *
 * \tparam TPData
 *    The type stored as data for the primal edges.
 *
 * \tparam TDData
 *    The type stored as data for the dual edges.
 *
 * \tparam TCellPixelType
 *     The type associated with every cell.
 *
 * \ingroup ITKQuadEdgeMesh
 */
template <typename TPixelType = float,
          unsigned int VPointDimension = 3,
          unsigned int VMaxTopologicalDimension = VPointDimension,
          typename TCoordRep = float,
          typename TInterpolationWeightType = float,
          typename TCellPixelType = TPixelType,
          typename TPData = bool,
          typename TDData = bool>
class QuadEdgeMeshExtendedTraits
{
public:
  using Self = QuadEdgeMeshExtendedTraits;
  /** Save the template parameters. */
  using CoordRepType = TCoordRep;
  using PixelType = TPixelType;
  using PrimalDataType = TPData;
  using DualDataType = TDData;
  using CellPixelType = TCellPixelType;

  /** Save all the template parameters. */
  static constexpr unsigned int PointDimension = VPointDimension;
  static constexpr unsigned int MaxTopologicalDimension = VPointDimension;

  using InterpolationWeightType = TInterpolationWeightType;

  /** The type to be used to identify a point.  This should be the index type
   * to the PointsContainer. */
  using PointIdentifier = IdentifierType;

  /** The type to be used to identify a cell.  This should be the index type
   * to the CellsContainer. */
  using CellIdentifier = IdentifierType;

  /** A type that can be used to identify individual boundary features on
   * the cells.  Since this will probably be an index into a static array,
   * this will probably never change from an integer setting. */
  using CellFeatureIdentifier = IdentifierType;

  /** The container type that will be used to store boundary links
   * back to cells.  This must conform to the STL "set" interface. */
  using UsingCellsContainer = std::set<CellIdentifier>;

  /** The CellLinks container should be a container of PointCellLinksContainer,
   * which should be a container conforming to the STL "set" interface. */
  using PointCellLinksContainer = std::set<CellIdentifier>;

  /** Quad edge type alias. */
  using QEPrimal = GeometricalQuadEdge<PointIdentifier, CellIdentifier, PrimalDataType, DualDataType>;
  using QEDual = typename QEPrimal::DualType;
  using VertexRefType = typename QEPrimal::OriginRefType;
  using FaceRefType = typename QEPrimal::DualOriginRefType;

  /** The type of point used by the mesh. This should never change from
   * this setting, regardless of the mesh type. Points have an entry
   * in the Onext ring */
  using PointType = QuadEdgeMeshPoint<CoordRepType, VPointDimension, QEPrimal>;

  using PointHashType = Point<CoordRepType, VPointDimension>;

  /** The container type for use in storing points. It must conform to
   * the IndexedContainer interface. */
  using PointsContainer = MapContainer<PointIdentifier, PointType>;

  /** Standard itk cell interface. */
  using CellTraits = QuadEdgeMeshCellTraitsInfo<VPointDimension,
                                                CoordRepType,
                                                InterpolationWeightType,
                                                PointIdentifier,
                                                CellIdentifier,
                                                CellFeatureIdentifier,
                                                PointType,
                                                PointsContainer,
                                                UsingCellsContainer,
                                                QEPrimal>;

  /** The interface to cells to be used by the mesh. */
  using CellType = CellInterface<CellPixelType, CellTraits>;
  using CellAutoPointer = typename CellType::CellAutoPointer;

  /** Containers types. */
  using CellLinksContainer = MapContainer<PointIdentifier, PointCellLinksContainer>;
  using CellsContainer = MapContainer<CellIdentifier, CellType *>;
  using PointDataContainer = MapContainer<PointIdentifier, PixelType>;
  using CellDataContainer = MapContainer<CellIdentifier, CellPixelType>;

  /** Other useful types. */
  using VectorType = typename PointType::VectorType;
};
} // end namespace itk

#endif
