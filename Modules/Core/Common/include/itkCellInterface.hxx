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
#ifndef __itkCellInterface_hxx
#define __itkCellInterface_hxx

#include "itkCellInterface.h"

namespace itk
{
/** Given the parametric coordinates of a point in the cell
 * (pCoords[CellDimension]), get the closest cell boundary feature of
 * topological dimension CellDimension-1.  If the "inside" pointer is not
 * NULL, the flag is set to indicate whether the point is inside the cell. */
template< typename TPixelType, typename TCellTraits >
bool
CellInterface< TPixelType, TCellTraits >
::GetClosestBoundary(CoordRepType[], bool *, CellAutoPointer &)
{
  return false;
}

/** Given the geometric coordinates of a point (coord[PointDimension]),
 * return whether it is inside the cell.  Also perform the following
 * calculations, if the corresponding result pointers are not NULL:
 *
 *  - Find the closest point in or on the cell to the given point
 *     (Returns through pointer to array: closestPoint[PointDimension]).
 *
 *  - Get the cell's parametric coordinates for the given point
 *     (Returns through pointer to array: pCoords[CellDimension]).
 *
 *  - Get the square of the distance between the point and the cell
 *     (this is the distance from the point to the closest point,
 *      returned through "dist2" pointer).
 *
 *
 *  - Get the interpolation weights for the cell
 *     (Returns through pointer to array: weights[NumberOfPoints]). */
template< typename TPixelType, typename TCellTraits >
bool
CellInterface< TPixelType, TCellTraits >
::EvaluatePosition(CoordRepType *,
                              PointsContainer *,
                              CoordRepType *,
                              CoordRepType[],
                              double *,
                              InterpolationWeightType *)
{
  return bool();
}

/** Given the parametric coordinates of a point in the cell
 *  determine the value of its Shape Functions
 *  returned through an itkArray<InterpolationWeightType>).  */
template< typename TPixelType, typename TCellTraits >
void
CellInterface< TPixelType, TCellTraits >
::EvaluateShapeFunctions(
  const ParametricCoordArrayType &,
  ShapeFunctionsArrayType  &) const
{

}

/** Intersect the cell with a line given by an origin (origin[PointDimension])
  * and direction (direction[PointDimension]).  The intersection point
  * found will be within the given tolerance of the real intersection.
  * Get the following results if the corresponding pointers are not NULL:
  *
  *  - The intersection point's geometric coordinates (returned through
  *     pointer to array: coords[PointDimension]).
  *
  *  - The line's parametric coordinate of the intersection point
  *     (returned through "t" pointer).
  *
  *  - The cell's parametric coordinates of the intersection point
  *     (returned through pointer to array: pCoords[CellDimension]).
  *
  * Returns whether an intersection exists within the given tolerance. */
template< typename TPixelType, typename TCellTraits >
bool
CellInterface< TPixelType, TCellTraits >
::IntersectWithLine(CoordRepType[PointDimension],
                               CoordRepType[PointDimension],
                               CoordRepType,
                               CoordRepType[PointDimension],
                               CoordRepType *,
                               CoordRepType[])
{
  return bool();
}

/** Compute cell bounding box and store in the user-provided array.
 * Array is ordered (xmin, xmax,  ymin, ymax, ....).  A pointer to the
 * array is returned for convenience.  This allows code like:
 * "CoordRep* bounds = cell->GetBoundingBox(new CoordRep[6]);". */
template< typename TPixelType, typename TCellTraits >
typename CellInterface< TPixelType, TCellTraits >::CoordRepType *
CellInterface< TPixelType, TCellTraits >
::GetBoundingBox(CoordRepType[PointDimension * 2])
{
  return NULL;
}

/** Compute the square of the diagonal length of the bounding box. */
template< typename TPixelType, typename TCellTraits >
typename CellInterface< TPixelType, TCellTraits >::CoordRepType
CellInterface< TPixelType, TCellTraits >
::GetBoundingBoxDiagonalLength2(void)
{
  return NULL;
}

/** Intersect the given bounding box (bounds[PointDimension*2]) with a line
 * given by an origin (origin[PointDimension]) and direction
 * (direction[PointDimension]). Get the following results if the
 * corresponding pointers are not NULL:
 *
 *  - The intersection point's geometric coordinates (returned through
 *     pointer to array: coords[PointDimension]).
 *
 *  - The line's parametric coordinate of the intersection point
 *     (returned through "t" pointer).
 *
 * Returns whether an intersection exists. */
template< typename TPixelType, typename TCellTraits >
bool
CellInterface< TPixelType, TCellTraits >
::IntersectBoundingBoxWithLine(CoordRepType[PointDimension * 2],
                                          CoordRepType[PointDimension],
                                          CoordRepType[PointDimension],
                                          CoordRepType[PointDimension],
                                          CoordRepType *)
{
  return bool();
}

/**
 * Get the interpolation order of the cell.  Usually linear.
 */
template< typename TPixelType, typename TCellTraits >
unsigned int
CellInterface< TPixelType, TCellTraits >
::GetInterpolationOrder(void) const
{
  return 1;
}

/**
 * Get the point id list used by the cell in a form suitable to pass to
 * SetPointIds(first) on another cell.  This is equivalent to
 * PointIdsBegin() const.
 */
template< typename TPixelType, typename TCellTraits >
typename CellInterface< TPixelType, TCellTraits >::PointIdConstIterator
CellInterface< TPixelType, TCellTraits >
::GetPointIds(void) const
{
  return this->PointIdsBegin();
}

template< typename TPixelType, typename TCellTraits >
typename CellInterface< TPixelType, TCellTraits >::PointIdentifierContainerType
CellInterface< TPixelType, TCellTraits >
::GetPointIdsContainer() const
{
  PointIdentifierContainerType res;
  res.SetSize( this->GetNumberOfPoints() );
  int i = 0;
  PointIdConstIterator it = this->PointIdsBegin();
  PointIdConstIterator end = this->PointIdsEnd();
  while( it != end )
    {
    res[i] = *it;
    ++i;
    ++it;
    }
  return res;
}

template< typename TPixelType, typename TCellTraits >
void
CellInterface< TPixelType, TCellTraits >
::SetPointIdsContainer( const PointIdentifierContainerType & container )
{
  for( unsigned int i=0; i<container.Size(); i++ )
    {
    this->SetPointId( i, container[i] );
    }
}

/**
 * Return true if the UsingCellsContainer m_UsingCells is nonempty,
 * false otherwise.  The container m_UsingCells is meant to contain a
 * list of all the cells that have this one as part of their boundary.
 * Boundary data is not automatically recorded upon mesh creation.  If
 * the boundary information has not been computed, this method always
 * returns false.
 */
template< typename TPixelType, typename TCellTraits >
bool
CellInterface< TPixelType, TCellTraits >
::IsExplicitBoundary(void)
{
  return !m_UsingCells.empty();
}

/**
 * Register the fact that this cell is a part of the boundary of the
 * cell \a cellId, by adding \a cellId to the UsingCellsContainer.
 */
template< typename TPixelType, typename TCellTraits >
void
CellInterface< TPixelType, TCellTraits >
::AddUsingCell(CellIdentifier cellId)
{
  m_UsingCells.insert(cellId);
}

/**
 * Remove a cell from the UsingCellsContainer.
 */
template< typename TPixelType, typename TCellTraits >
void
CellInterface< TPixelType, TCellTraits >
::RemoveUsingCell(CellIdentifier cellId)
{
  m_UsingCells.erase(cellId);
}

/**
 * Test if a cell is in the UsingCellsContainer.  A result of \c true
 * indicates that this cell is part of the boundary of the cell \c
 * cellId, assuming that boundary information has been recorded.
 */
template< typename TPixelType, typename TCellTraits >
bool
CellInterface< TPixelType, TCellTraits >
::IsUsingCell(CellIdentifier cellId)
{
  return ( m_UsingCells.count(cellId) > 0 );
}

/**
 * Get the number of cells in the UsingCellsContainer.
 */
template< typename TPixelType, typename TCellTraits >
unsigned int
CellInterface< TPixelType, TCellTraits >
::GetNumberOfUsingCells(void)
{
  return static_cast< unsigned int >( m_UsingCells.size() );
}

#if !defined( CABLE_CONFIGURATION )

/**
 * Get a begin iterator for the UsingCellsContainer.
 */
template< typename TPixelType, typename TCellTraits >
typename CellInterface< TPixelType, TCellTraits >::UsingCellsContainerIterator
CellInterface< TPixelType, TCellTraits >
::UsingCellsBegin(void)
{
  return m_UsingCells.begin();
}

/**
 * Get an end iterator for the UsingCellsContainer.
 */
template< typename TPixelType, typename TCellTraits >
typename CellInterface< TPixelType, TCellTraits >::UsingCellsContainerIterator
CellInterface< TPixelType, TCellTraits >
::UsingCellsEnd(void)
{
  return m_UsingCells.end();
}

template< typename TPixelType, typename TCellTraits >
CellInterface< TPixelType, TCellTraits >
::CellInterface()
{

}

template< typename TPixelType, typename TCellTraits >
CellInterface< TPixelType, TCellTraits >
::~CellInterface()
{

}

#endif
} // end namespace itk

#endif
