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
#ifndef itkVertexCell_hxx
#define itkVertexCell_hxx
#include "itkVertexCell.h"

namespace itk
{
/** Standard CellInterface: */
template< typename TCellInterface >
void
VertexCell< TCellInterface >
::MakeCopy(CellAutoPointer & cellPointer) const
{
  cellPointer.TakeOwnership(new Self);
  cellPointer->SetPointIds( this->GetPointIds() );
}

/** Standard CellInterface:
 *  Get the topological dimension of this cell. */
template< typename TCellInterface >
unsigned int
VertexCell< TCellInterface >
::GetDimension(void) const
{
  return Self::CellDimension;
}

/** Standard CellInterface:
 *  Get the number of points required to define the cell. */
template< typename TCellInterface >
unsigned int
VertexCell< TCellInterface >
::GetNumberOfPoints(void) const
{
  return Self::NumberOfPoints;
}

/** Standard CellInterface:
 *  A vertex has no boundary entities of any dimension. */
template< typename TCellInterface >
typename VertexCell< TCellInterface >::CellFeatureCount
VertexCell< TCellInterface >
::GetNumberOfBoundaryFeatures(int) const
{
  return 0;
}

/** Standard CellInterface:
 *  A vertex has no boundary entities.  Just return null. */
template< typename TCellInterface >
bool
VertexCell< TCellInterface >
::GetBoundaryFeature(int, CellFeatureIdentifier, CellAutoPointer & cellAPtr)
{
  cellAPtr.Reset();
  return false;
}

/** Standard CellInterface:
 *  Set the point id list used by the cell.  It is assumed that the given
 *  iterator can be incremented and safely de-referenced enough times to
 *  get all the point ids needed by the cell. */
template< typename TCellInterface >
void
VertexCell< TCellInterface >
::SetPointIds(PointIdConstIterator first)
{
  PointIdConstIterator ii(first);

  for ( unsigned int i = 0; i < Self::NumberOfPoints; ++i )
    {
    m_PointIds[i] = *ii++;
    }
}

/** Standard CellInterface:
 *  Set the point id list used by the cell.  It is assumed that the range
 *  of iterators [first, last) contains the correct number of points needed to
 *  define the cell.  The position *last is NOT referenced, so it can safely
 *  be one beyond the end of an array or other container. */
template< typename TCellInterface >
void
VertexCell< TCellInterface >
::SetPointIds(PointIdConstIterator first, PointIdConstIterator last)
{
  int                  localId = 0;
  PointIdConstIterator ii(first);

  while ( ii != last )
    {
    m_PointIds[localId++] = *ii++;
    }
}

/** Standard CellInterface:
 *  Set an individual point identifier in the cell. */
template< typename TCellInterface >
void
VertexCell< TCellInterface >
::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}

/** Standard CellInterface:
 *  Get a begin iterator to the list of point identifiers used by the
 *  cell. */
template< typename TCellInterface >
typename VertexCell< TCellInterface >::PointIdIterator
VertexCell< TCellInterface >
::PointIdsBegin(void)
{
  return &m_PointIds[0];
}

/** Standard CellInterface:
 *  Get a const begin iterator to the list of point identifiers used
 *  by the cell. */
template< typename TCellInterface >
typename VertexCell< TCellInterface >::PointIdConstIterator
VertexCell< TCellInterface >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}

/** Standard CellInterface:
 *  Get an end iterator to the list of point identifiers used by the cell. */
template< typename TCellInterface >
typename VertexCell< TCellInterface >::PointIdIterator
VertexCell< TCellInterface >
::PointIdsEnd(void)
{
  return &m_PointIds[Self::NumberOfPoints - 1] + 1;
}

/** Standard CellInterface:
 *  Get a const end iterator to the list of point identifiers used
 *  by the cell. */
template< typename TCellInterface >
typename VertexCell< TCellInterface >::PointIdConstIterator
VertexCell< TCellInterface >
::PointIdsEnd(void) const
{
  return &m_PointIds[Self::NumberOfPoints - 1] + 1;
}

/** Vertex-specific:
 *  Set the identifier of the point defining the vertex. */
template< typename TCellInterface >
void
VertexCell< TCellInterface >
::SetPointId(PointIdentifier ptId)
{
  m_PointIds[0] = ptId;
}

/** Vertex-specific:
 *  Get the identifier of the point defining the vertex. */
template< typename TCellInterface >
typename VertexCell< TCellInterface >::PointIdentifier
VertexCell< TCellInterface >
::GetPointId(void)
{
  return m_PointIds[0];
}

/** Evaluate the position of a given point */
template< typename TCellInterface >
bool
VertexCell< TCellInterface >
::EvaluatePosition(CoordRepType *x,
                   PointsContainer *points,
                   CoordRepType *closestPoint,
                   CoordRepType pcoord[2],
                   double *minDist2,
                   InterpolationWeightType *weights)
{
  PointType X = points->GetElement(m_PointIds[0]);

  if ( closestPoint )
    {
    for ( unsigned int i = 0; i < PointDimension; i++ )
      {
      closestPoint[i] = X[i];
      }
    }

  double dist2 = 0;
    {
    for ( unsigned int i = 0; i < PointDimension; i++ )
      {
      dist2 += ( X[i] - x[i] ) * ( X[i] - x[i] );
      }
    }

  if ( minDist2 )
    {
    *minDist2 = dist2;
    }

  if ( weights )
    {
    weights[0] = 1.0;
    }

  if ( dist2 == 0.0 )
    {
    if ( pcoord )
      {
      pcoord[0] = 0.0;
      }
    return true;
    }
  else
    {
    if ( pcoord )
      {
      pcoord[0] = -10.0;
      }
    return 0;
    }
}
} // end namespace itk

#endif
