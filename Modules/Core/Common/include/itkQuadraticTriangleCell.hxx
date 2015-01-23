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
#ifndef itkQuadraticTriangleCell_hxx
#define itkQuadraticTriangleCell_hxx
#include "itkQuadraticTriangleCell.h"

namespace itk
{
/**
 * Standard CellInterface:
 */
template< typename TCellInterface >
void
QuadraticTriangleCell< TCellInterface >
::MakeCopy(CellAutoPointer & cellPointer) const
{
  cellPointer.TakeOwnership(new Self);
  cellPointer->SetPointIds( this->GetPointIds() );
}

/**
 * Standard CellInterface:
 * Get the topological dimension of this cell.
 */
template< typename TCellInterface >
unsigned int
QuadraticTriangleCell< TCellInterface >
::GetDimension(void) const
{
  return Self::CellDimension;
}

/**
 * Standard CellInterface:
 * Get the number of points required to define the cell.
 */
template< typename TCellInterface >
unsigned int
QuadraticTriangleCell< TCellInterface >
::GetNumberOfPoints(void) const
{
  return Self::NumberOfPoints;
}

/**
 * Standard CellInterface:
 * Get the number of boundary features of the given dimension.
 */
template< typename TCellInterface >
typename QuadraticTriangleCell< TCellInterface >::CellFeatureCount
QuadraticTriangleCell< TCellInterface >
::GetNumberOfBoundaryFeatures(int dimension) const
{
  switch ( dimension )
    {
    case 0:
      return GetNumberOfVertices();
    case 1:
      return GetNumberOfEdges();
    default:
      return 0;
    }
}

/**
 * Standard CellInterface:
 * Get the boundary feature of the given dimension specified by the given
 * cell feature Id.
 * The Id can range from 0 to GetNumberOfBoundaryFeatures(dimension)-1.
 */
template< typename TCellInterface >
bool
QuadraticTriangleCell< TCellInterface >
::GetBoundaryFeature(int dimension, CellFeatureIdentifier featureId,
                     CellAutoPointer & cellPointer)
{
  switch ( dimension )
    {
    case 0:
      {
      VertexAutoPointer vertexPointer;
      if ( this->GetVertex(featureId, vertexPointer) )
        {
        TransferAutoPointer(cellPointer, vertexPointer);
        return true;
        }
      break;
      }
    case 1:
      {
      EdgeAutoPointer edgePointer;
      if ( this->GetEdge(featureId, edgePointer) )
        {
        TransferAutoPointer(cellPointer, edgePointer);
        return true;
        }
      break;
      }
    default:
      break; // just fall through
    }
  cellPointer.Reset();
  return false;
}

/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the given
 * iterator can be incremented and safely de-referenced enough times to
 * get all the point ids needed by the cell.
 */
template< typename TCellInterface >
void
QuadraticTriangleCell< TCellInterface >
::SetPointIds(PointIdConstIterator first)
{
  PointIdConstIterator ii(first);

  for ( unsigned int i = 0; i < Self::NumberOfPoints; ++i )
    {
    m_PointIds[i] = *ii++;
    }
}

/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the range
 * of iterators [first, last) contains the correct number of points needed to
 * define the cell.  The position *last is NOT referenced, so it can safely
 * be one beyond the end of an array or other container.
 */
template< typename TCellInterface >
void
QuadraticTriangleCell< TCellInterface >
::SetPointIds(PointIdConstIterator first, PointIdConstIterator last)
{
  int                  localId = 0;
  PointIdConstIterator ii(first);

  while ( ii != last )
    {
    m_PointIds[localId++] = *ii++;
    }
}

/**
 * Standard CellInterface:
 * Set an individual point identifier in the cell.
 */
template< typename TCellInterface >
void
QuadraticTriangleCell< TCellInterface >
::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}

/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template< typename TCellInterface >
typename QuadraticTriangleCell< TCellInterface >::PointIdIterator
QuadraticTriangleCell< TCellInterface >
::PointIdsBegin(void)
{
  return &m_PointIds[0];
}

/**
 * Standard CellInterface:
 * Get a const begin iterator to the list of point identifiers used
 * by the cell.
 */
template< typename TCellInterface >
typename QuadraticTriangleCell< TCellInterface >::PointIdConstIterator
QuadraticTriangleCell< TCellInterface >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}

/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */
template< typename TCellInterface >
typename QuadraticTriangleCell< TCellInterface >::PointIdIterator
QuadraticTriangleCell< TCellInterface >
::PointIdsEnd(void)
{
  return &m_PointIds[Self::NumberOfPoints - 1] + 1;
}

/**
 * Standard CellInterface:
 * Get a const end iterator to the list of point identifiers used
 * by the cell.
 */
template< typename TCellInterface >
typename QuadraticTriangleCell< TCellInterface >::PointIdConstIterator
QuadraticTriangleCell< TCellInterface >
::PointIdsEnd(void) const
{
  return &m_PointIds[Self::NumberOfPoints - 1] + 1;
}

/**
 * Triangle-specific:
 * Get the number of vertices defining the triangle.
 */
template< typename TCellInterface >
typename QuadraticTriangleCell< TCellInterface >::CellFeatureCount
QuadraticTriangleCell< TCellInterface >
::GetNumberOfVertices(void) const
{
  return Self::NumberOfVertices;
}

/**
 * Triangle-specific:
 * Get the number of edges defined for the triangle.
 */
template< typename TCellInterface >
typename QuadraticTriangleCell< TCellInterface >::CellFeatureCount
QuadraticTriangleCell< TCellInterface >
::GetNumberOfEdges(void) const
{
  return Self::NumberOfEdges;
}

/**
 * Triangle-specific:
 * Get the vertex specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfVertices()-1.
 */
template< typename TCellInterface >
bool
QuadraticTriangleCell< TCellInterface >
::GetVertex(CellFeatureIdentifier vertexId, VertexAutoPointer & vertexPointer)
{
  VertexType *vert = new VertexType;

  vert->SetPointId(0, m_PointIds[vertexId]);
  vertexPointer.TakeOwnership(vert);
  return true;
}

/**
 * Triangle-specific:
 * Get the edge specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfEdges()-1.
 */
template< typename TCellInterface >
bool
QuadraticTriangleCell< TCellInterface >
::GetEdge(CellFeatureIdentifier edgeId, EdgeAutoPointer & edgePointer)
{
  EdgeType *edge = new EdgeType;

  for ( unsigned int i = 0; i < EdgeType::NumberOfPoints; ++i )
    {
    edge->SetPointId(i, m_PointIds[m_Edges[edgeId][i]]);
    }
  edgePointer.TakeOwnership(edge);
  return true;
}

/** Given the parametric coordinates of a point in the cell
 *  determine the value of its Shape Functions
 *  returned through an itkArray<InterpolationWeightType>).  */
template< typename TCellInterface >
void
QuadraticTriangleCell< TCellInterface >
::EvaluateShapeFunctions(
  const ParametricCoordArrayType & parametricCoordinates,
  ShapeFunctionsArrayType  & weights) const
{
  if ( parametricCoordinates.size() != 3 )
    {
    itkGenericExceptionMacro(<< "QuadraticTriangleCell expect three coordinates");
    }

  const double L1 = parametricCoordinates[0];
  const double L2 = parametricCoordinates[1];
  const double L3 = parametricCoordinates[2];

  weights = ShapeFunctionsArrayType(6);

  weights[0] = L1 * ( 2.0 * L1 - 1.0 );
  weights[1] = L2 * ( 2.0 * L2 - 1.0 );
  weights[2] = L3 * ( 2.0 * L3 - 1.0 );
  weights[3] = 4.0 * L1 * L3;
  weights[4] = 4.0 * L1 * L2;
  weights[5] = 4.0 * L2 * L3;
}
} // end namespace itk

#endif
