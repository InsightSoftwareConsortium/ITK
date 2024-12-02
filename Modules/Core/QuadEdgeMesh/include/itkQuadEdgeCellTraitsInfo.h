/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkQuadEdgeCellTraitsInfo_h
#define itkQuadEdgeCellTraitsInfo_h

#include "itkQuadEdgeMeshPoint.h"
#include "itkMapContainer.h"
#include "itkIntTypes.h"
#include <set>

namespace itk
{
/**
 * \class QuadEdgeMeshCellTraitsInfo
 *  \brief Helper class holding the traits of QuadEdge cells.
 *
 * \author Alexandre Gouaillard, Leonardo Florez-Valencia, Eric Boix
 *
 * This implementation was contributed as a paper to the Insight Journal
 * https://doi.org/10.54294/4mx7kk
 *
 * \ingroup ITKQuadEdgeMesh
 */
template <int VPointDimension,
          typename TCoordinate = float,
          typename TInterpolationWeight = float,
          typename TPointIdentifier = IdentifierType,
          typename TCellIdentifier = IdentifierType,
          typename TCellFeatureIdentifier = unsigned char,
          typename TPoint = QuadEdgeMeshPoint<TCoordinate, VPointDimension>,
          typename TPointsContainer = MapContainer<TPointIdentifier, TPoint>,
          typename TUsingCellsContainer = std::set<TPointIdentifier>,
          typename TQE = GeometricalQuadEdge<unsigned long, unsigned long, bool, bool, true>>
class QuadEdgeMeshCellTraitsInfo
{
public:
  static constexpr unsigned int PointDimension = VPointDimension;
  using CoordinateType = TCoordinate;
#ifndef ITK_FUTURE_LEGACY_REMOVE
  using CoordRepType ITK_FUTURE_DEPRECATED(
    "ITK 6 discourages using `CoordRepType`. Please use `CoordinateType` instead!") = CoordinateType;
#endif
  using InterpolationWeightType = TInterpolationWeight;
  using PointIdentifier = TPointIdentifier;
  using CellIdentifier = TCellIdentifier;
  using CellFeatureIdentifier = TCellFeatureIdentifier;
  using PointType = TPoint;
  using PointsContainer = TPointsContainer;
  using UsingCellsContainer = TUsingCellsContainer;

  /** Iterator types. */
  using PointIdIterator = PointIdentifier *;
  using PointIdConstIterator = const PointIdentifier *;
  using QuadEdgeType = TQE;
  using PointIdInternalIterator = typename TQE::IteratorGeom;
  using PointIdInternalConstIterator = typename TQE::ConstIteratorGeom;
};
} // namespace itk
#endif
