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
template<
  typename TPixelType = float,
  unsigned int VPointDimension = 3,
  unsigned int VMaxTopologicalDimension = VPointDimension,
  typename TCoordRep = float,
  typename TInterpolationWeightType = float,
  typename TCellPixelType = TPixelType,
  typename TPData = bool,
  typename TDData = bool
  >
class QuadEdgeMeshExtendedTraits
{
public:
  typedef QuadEdgeMeshExtendedTraits Self;
  /** Save the template parameters. */
  typedef TCoordRep      CoordRepType;
  typedef TPixelType     PixelType;
  typedef TPData         PrimalDataType;
  typedef TDData         DualDataType;
  typedef TCellPixelType CellPixelType;

  /** Save all the template parameters. */
  itkStaticConstMacro(PointDimension, unsigned int, VPointDimension);
  itkStaticConstMacro(MaxTopologicalDimension, unsigned int,
                      VPointDimension);

  typedef TInterpolationWeightType InterpolationWeightType;

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

  /** The container type that will be used to store boundary links
   * back to cells.  This must conform to the STL "set" interface. */
  typedef std::set< CellIdentifier > UsingCellsContainer;

  /** The CellLinks container should be a container of PointCellLinksContainer,
   * which should be a container conforming to the STL "set" interface. */
  typedef std::set< CellIdentifier > PointCellLinksContainer;

  /** Quad edge typedefs. */
  typedef GeometricalQuadEdge< PointIdentifier, CellIdentifier, PrimalDataType, DualDataType > QEPrimal;
  typedef typename QEPrimal::DualType                                                          QEDual;
  typedef typename QEPrimal::OriginRefType                                                     VertexRefType;
  typedef typename QEPrimal::DualOriginRefType                                                 FaceRefType;

  /** The type of point used by the mesh. This should never change from
   * this setting, regardless of the mesh type. Points have an entry
   * in the Onext ring */
  typedef QuadEdgeMeshPoint<
    CoordRepType, VPointDimension, QEPrimal >                 PointType;

  typedef Point< CoordRepType, VPointDimension > PointHashType;

  /** The container type for use in storing points. It must conform to
   * the IndexedContainer interface. */
  typedef MapContainer< PointIdentifier, PointType > PointsContainer;

  /** Standard itk cell interface. */
  typedef QuadEdgeMeshCellTraitsInfo<
    VPointDimension, CoordRepType,
    InterpolationWeightType, PointIdentifier,
    CellIdentifier,          CellFeatureIdentifier,
    PointType,               PointsContainer,
    UsingCellsContainer,     QEPrimal >                       CellTraits;

  /** The interface to cells to be used by the mesh. */
  typedef CellInterface< CellPixelType, CellTraits > CellType;
  typedef typename CellType::CellAutoPointer         CellAutoPointer;

  /** Containers types. */
  typedef MapContainer< PointIdentifier, PointCellLinksContainer > CellLinksContainer;
  typedef MapContainer< CellIdentifier, CellType * >               CellsContainer;
  typedef MapContainer< PointIdentifier, PixelType >               PointDataContainer;
  typedef MapContainer< CellIdentifier, CellPixelType >            CellDataContainer;

  /** Other useful types. */
  typedef typename PointType::VectorType VectorType;
};
} // enamespace

#endif

// eof - itkQuadEdgeMeshExtendedTraits.h
