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
 * This is only part of the boundary interface.  Just ignore the call.
 */
template <typename TPixelType, typename TCellType>
void
CellInterface< TPixelType , TCellType >
::AddUsingCell(CellIdentifier)
{
}


/**
 * This is only part of the boundary interface.  Just ignore the call.
 */
template <typename TPixelType, typename TCellType>
void
CellInterface< TPixelType , TCellType >
::RemoveUsingCell(CellIdentifier)
{
}


/**
 * By default, the cell is not a boundary, so it has no using cells.
 */
template <typename TPixelType, typename TCellType>
bool
CellInterface< TPixelType , TCellType >
::IsUsingCell(CellIdentifier)
{
  return false;
}


/**
 * This is only part of the boundary interface.  Just ignore the call.
 */
template <typename TPixelType, typename TCellType>
int
CellInterface< TPixelType , TCellType >
::GetNumUsingCells(void)
{
  return 0;
}


/**
 * This is only part of the boundary interface.  Just ignore the call.
 */
template <typename TPixelType, typename TCellType>
CellInterface< TPixelType , TCellType >::UsingCellsContainerIterator
CellInterface< TPixelType , TCellType >
::UsingCellsBegin(void)
{
  return UsingCellsContainerIterator();
}


/**
 * This is only part of the boundary interface.  Just ignore the call.
 */
template <typename TPixelType, typename TCellType>
CellInterface< TPixelType , TCellType >::UsingCellsContainerIterator
CellInterface< TPixelType , TCellType >
::UsingCellsEnd(void)
{
  return UsingCellsContainerIterator();
}


/**
 * Constructor:  Useless for now.
 */
template <typename TPixelType, typename TCellType>
CellInterface< TPixelType , TCellType >
::CellInterface()
{
}

} // namespace itk
