/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCellInterface.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkCellInterface_txx
#define _itkCellInterface_txx
#include "itkCellInterface.h"

namespace itk
{
/**
 * Get the interpolation order of the cell.  Usually linear.
 */
template <typename TPixelType, typename TCellTraits>
unsigned int
CellInterface< TPixelType , TCellTraits >
::GetInterpolationOrder(void) const
{
  return 1;
}
  

/**
 * Get the point id list used by the cell in a form suitable to pass to
 * SetPointIds(first) on another cell.  This is equivalent to
 * PointIdsBegin() const.
 */
template <typename TPixelType, typename TCellTraits>
typename CellInterface< TPixelType , TCellTraits >::PointIdConstIterator
CellInterface< TPixelType , TCellTraits >
::GetPointIds(void) const
{
  return this->PointIdsBegin();
}
 
  
/**
 * Return true if the UsingCellsContainer m_UsingCells is nonempty,
 * false otherwise.  The container m_UsingCells is meant to contain a
 * list of all the cells that have this one as part of their boundary.
 * Boundary data is not automatically recorded upon mesh creation.  If
 * the boundary information has not been computed, this method always
 * returns false.
 */
template <typename TPixelType, typename TCellTraits>
bool
CellInterface< TPixelType , TCellTraits >
::IsBoundary(void)
{
  return !m_UsingCells.empty();
}


/**
 * Register the fact that this cell is a part of the boundary of the
 * cell \c cellId, by adding \a cellId to the UsingCellsContainer.
 */
template <typename TPixelType, typename TCellTraits>
void
CellInterface< TPixelType , TCellTraits >
::AddUsingCell(CellIdentifier cellId)
{
  m_UsingCells.insert(cellId);
}


/**
 * Remove a cell from the UsingCellsContainer.
 */
template <typename TPixelType, typename TCellTraits>
void
CellInterface< TPixelType , TCellTraits >
::RemoveUsingCell(CellIdentifier cellId)
{
  m_UsingCells.erase(cellId);
}

/**
 * Test if a cell is in the UsingCellsContainer.  A result of \c true
 * indicates that this cell is part of the boundary of the cell \c
 * cellId, assuming that boundary information has been recorded.
 */
template <typename TPixelType, typename TCellTraits>
bool
CellInterface< TPixelType , TCellTraits >
::IsUsingCell(CellIdentifier cellId)
{
  return (m_UsingCells.count(cellId) > 0);
}


/**
 * Get the number of cells in the UsingCellsContainer.
 */
template <typename TPixelType, typename TCellTraits>
unsigned int
CellInterface< TPixelType , TCellTraits >
::GetNumberOfUsingCells(void)
{
  return static_cast<unsigned int>( m_UsingCells.size() );
}


/**
 * Get a begin iterator for the UsingCellsContainer.
 */
template <typename TPixelType, typename TCellTraits>
typename CellInterface< TPixelType , TCellTraits >::UsingCellsContainerIterator
CellInterface< TPixelType , TCellTraits >
::UsingCellsBegin(void)
{
  return m_UsingCells.begin();
}


/**
 * Get an end iterator for the UsingCellsContainer.
 */
template <typename TPixelType, typename TCellTraits>
typename CellInterface< TPixelType , TCellTraits >::UsingCellsContainerIterator
CellInterface< TPixelType , TCellTraits >
::UsingCellsEnd(void)
{
  return m_UsingCells.end();
}

} // end namespace itk

#endif
