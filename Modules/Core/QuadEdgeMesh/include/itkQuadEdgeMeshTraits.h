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
#ifndef itkQuadEdgeMeshTraits_h
#define itkQuadEdgeMeshTraits_h

#include <set>
#include "itkCellInterface.h"
#include "itkQuadEdgeCellTraitsInfo.h"

namespace itk
{
/**
 *\class QuadEdgeMeshTraits
 *  \brief Class holding the traits of the QuadEdgeMesh.
 *
 *  This class is a variant of the MeshTraits that adds the traits
 *  defined in the QuadEdgeMeshCellTraitsInfo class.
 *
 * \author Alexandre Gouaillard, Leonardo Florez-Valencia, Eric Boix
 *
 * This implementation was contributed as a paper to the Insight Journal
 * https://hdl.handle.net/1926/306
 *
 *  \sa DefaultDynamicMeshTraits
 *  \sa DefaultStaticMeshTraits
 * \ingroup ITKQuadEdgeMesh
 */
template <typename TPixel,
          unsigned int VPointDimension,
          typename TPData,
          typename TDData,
          typename TCoordRep = float,
          typename TInterpolationWeight = float>
class QuadEdgeMeshTraits
{
public:
  /** Basic types for a mesh trait class. */
  using Self = QuadEdgeMeshTraits;
  using PixelType = TPixel;
  using CellPixelType = TPixel;
  using CoordRepType = TCoordRep;
  using InterpolationWeightType = TInterpolationWeight;

  static constexpr unsigned int PointDimension = VPointDimension;
  static constexpr unsigned int MaxTopologicalDimension = VPointDimension;

  using PointIdentifier = ::itk::IdentifierType;
  using CellIdentifier = ::itk::IdentifierType;

  using CellFeatureIdentifier = unsigned char; // made small in purpose

  using UsingCellsContainer = std::set<CellIdentifier>;
  using PointCellLinksContainer = std::set<CellIdentifier>;

  /** Quad edge type alias. */
  using PrimalDataType = TPData;
  using DualDataType = TDData;
  using QEPrimal = GeometricalQuadEdge<PointIdentifier, CellIdentifier, PrimalDataType, DualDataType>;
  // using QEType = QEPrimal;
  using QEDual = typename QEPrimal::DualType;
  // FOR LEO using QEType = typename QEPrimal::Superclass;
  // FOR LEO using QEDual = typename QEPrimal::Dual;
  using VertexRefType = typename QEPrimal::OriginRefType;
  using FaceRefType = typename QEPrimal::DualOriginRefType;

  /** The type of point used for hashing.  This should never change from
   * this setting, regardless of the mesh type. */
  using PointHashType = Point<CoordRepType, VPointDimension>;

  /** Points have an entry in the Onext ring */
  using PointType = QuadEdgeMeshPoint<CoordRepType, VPointDimension, QEPrimal>;
  using PointsContainer = MapContainer<PointIdentifier, PointType>;

  /** Standard cell interface. */
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
} // namespace itk

#endif
