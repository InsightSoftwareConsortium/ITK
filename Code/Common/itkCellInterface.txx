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
// #include "itkCellInterface.h"

namespace itk
{
/**
 * Get the interpolation order of the cell.  Usually linear.
 */
template <typename TPixelType, typename TCellType>
int
CellInterface< TPixelType , TCellType >
::GetInterpolationOrder(void)
{
  return 1;
}
  

/**
 * Get the point id list used by the cell in a form suitable to pass to
 * SetPointIds(first) on another cell.  This is equivalent to
 * PointIdsBegin() const.
 */
template <typename TPixelType, typename TCellType>
CellInterface< TPixelType , TCellType >::PointIdConstIterator
CellInterface< TPixelType , TCellType >
::GetPointIds(void) const
{
  return this->PointIdsBegin();
}
 
  
/**
 * By default, a cell is not a boundary.
 */
template <typename TPixelType, typename TCellType>
bool
CellInterface< TPixelType , TCellType >
::IsBoundary(void)
{
  return false;
}


/**
 * This is only part of the boundary interface.  The call is ignored.
 */
template <typename TPixelType, typename TCellType>
void
CellInterface< TPixelType , TCellType >
::AddUsingCell(CellIdentifier)
{
}


/**
 * This is only part of the boundary interface.  The call is ignored.
 */
template <typename TPixelType, typename TCellType>
void
CellInterface< TPixelType , TCellType >
::RemoveUsingCell(CellIdentifier)
{
}


/**
 * By default, the cell is not a boundary, so it has no using cells.
 * This will always return false for a cell.
 */
template <typename TPixelType, typename TCellType>
bool
CellInterface< TPixelType , TCellType >
::IsUsingCell(CellIdentifier)
{
  return false;
}


/**
 * This is only part of the boundary interface.  The call is ignored.
 */
template <typename TPixelType, typename TCellType>
int
CellInterface< TPixelType , TCellType >
::GetNumUsingCells(void)
{
  return 0;
}


/**
 * This is only part of the boundary interface.  The call is ignored.
 */
template <typename TPixelType, typename TCellType>
CellInterface< TPixelType , TCellType >::UsingCellsContainerIterator
CellInterface< TPixelType , TCellType >
::UsingCellsBegin(void)
{
  return UsingCellsContainerIterator();
}


/**
 * This is only part of the boundary interface.  The call is ignored.
 */
template <typename TPixelType, typename TCellType>
CellInterface< TPixelType , TCellType >::UsingCellsContainerIterator
CellInterface< TPixelType , TCellType >
::UsingCellsEnd(void)
{
  return UsingCellsContainerIterator();
}

} // end namespace itk
