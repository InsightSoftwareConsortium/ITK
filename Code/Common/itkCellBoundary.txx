/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCellBoundary.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkCellBoundary_txx
#define _itkCellBoundary_txx
#include "itkCellBoundary.h"

namespace itk
{

/**
 * This is the boundary wrapper, so of course we are a boundary!
 */
template <typename TCell>
bool
CellBoundary<TCell>
::IsBoundary(void)
{
  return true;
}


/**
 * Add a cell to the UsingCellsContainer.
 */
template <typename TCell>
void
CellBoundary<TCell>
::AddUsingCell(CellIdentifier cellId)
{
  m_UsingCells.insert(cellId);
}


/**
 * Remove a cell from the UsingCellsContainer.
 */
template <typename TCell>
void
CellBoundary<TCell>
::RemoveUsingCell(CellIdentifier cellId)
{
  m_UsingCells.erase(cellId);
}


/**
 * Test if a cell is in the UsingCellsContainer.
 */
template <typename TCell>
bool
CellBoundary<TCell>
::IsUsingCell(CellIdentifier cellId)
{
  return (m_UsingCells.count(cellId) > 0);
}


/**
 * Get the number of cells in the UsingCellsContainer.
 */
template <typename TCell>
unsigned int
CellBoundary<TCell>
::GetNumberOfUsingCells(void)
{
  return static_cast<unsigned int>( m_UsingCells.size() );
}


/**
 * Get a begin iterator for the UsingCellsContainer.
 */
template <typename TCell>
typename CellBoundary<TCell>::UsingCellsContainerIterator
CellBoundary<TCell>
::UsingCellsBegin(void)
{
  return m_UsingCells.begin();
}


/**
 * Get an end iterator for the UsingCellsContainer.
 */
template <typename TCell>
typename CellBoundary<TCell>::UsingCellsContainerIterator
CellBoundary<TCell>
::UsingCellsEnd(void)
{
  return m_UsingCells.end();
}

} // end namespace itk

#endif
