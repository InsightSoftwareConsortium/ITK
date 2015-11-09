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
#ifndef itkCellInterface_hxx
#define itkCellInterface_hxx

#include "itkCellInterface.h"

namespace itk
{
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

#if !defined( ITK_WRAPPING_PARSER )

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

#endif
} // end namespace itk

#endif
