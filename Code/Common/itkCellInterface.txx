/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCellInterface.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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
int
CellInterface< TPixelType , TCellTraits >
::GetInterpolationOrder(void)
{
  return 1;
}
  

/**
 * Get the point id list used by the cell in a form suitable to pass to
 * SetPointIds(first) on another cell.  This is equivalent to
 * PointIdsBegin() const.
 */
template <typename TPixelType, typename TCellTraits>
CellInterface< TPixelType , TCellTraits >::PointIdConstIterator
CellInterface< TPixelType , TCellTraits >
::GetPointIds(void) const
{
  return this->PointIdsBegin();
}
 
  
/**
 * By default, a cell is not a boundary.
 */
template <typename TPixelType, typename TCellTraits>
bool
CellInterface< TPixelType , TCellTraits >
::IsBoundary(void)
{
  return false;
}


/**
 * This is only part of the boundary interface.  The call is ignored.
 */
template <typename TPixelType, typename TCellTraits>
void
CellInterface< TPixelType , TCellTraits >
::AddUsingCell(CellIdentifier)
{
}


/**
 * This is only part of the boundary interface.  The call is ignored.
 */
template <typename TPixelType, typename TCellTraits>
void
CellInterface< TPixelType , TCellTraits >
::RemoveUsingCell(CellIdentifier)
{
}


/**
 * By default, the cell is not a boundary, so it has no using cells.
 * This will always return false for a cell.
 */
template <typename TPixelType, typename TCellTraits>
bool
CellInterface< TPixelType , TCellTraits >
::IsUsingCell(CellIdentifier)
{
  return false;
}


/**
 * This is only part of the boundary interface.  The call is ignored.
 */
template <typename TPixelType, typename TCellTraits>
int
CellInterface< TPixelType , TCellTraits >
::GetNumUsingCells(void)
{
  return 0;
}


/**
 * This is only part of the boundary interface.  The call is ignored.
 */
template <typename TPixelType, typename TCellTraits>
CellInterface< TPixelType , TCellTraits >::UsingCellsContainerIterator
CellInterface< TPixelType , TCellTraits >
::UsingCellsBegin(void)
{
  return UsingCellsContainerIterator();
}


/**
 * This is only part of the boundary interface.  The call is ignored.
 */
template <typename TPixelType, typename TCellTraits>
CellInterface< TPixelType , TCellTraits >::UsingCellsContainerIterator
CellInterface< TPixelType , TCellTraits >
::UsingCellsEnd(void)
{
  return UsingCellsContainerIterator();
}

} // end namespace itk

#endif
