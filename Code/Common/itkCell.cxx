/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCell.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkCell.h"


/**
 * By default, a cell is not a boundary.
 */
template <typename TPixelType, typename TCellType>
bool
itkCell< TPixelType , TCellType >
::IsBoundary(void)
{
  return false;
}


/**
 * This is only part of the boundary interface.  Just ignore the call.
 */
template <typename TPixelType, typename TCellType>
void
itkCell< TPixelType , TCellType >
::AddUsingCell(CellIdentifier)
{
}


/**
 * This is only part of the boundary interface.  Just ignore the call.
 */
template <typename TPixelType, typename TCellType>
void
itkCell< TPixelType , TCellType >
::RemoveUsingCell(CellIdentifier)
{
}


/**
 * By default, the cell is not a boundary, so it has no using cells.
 */
template <typename TPixelType, typename TCellType>
bool
itkCell< TPixelType , TCellType >
::IsUsingCell(CellIdentifier)
{
  return false;
}


/**
 * This is only part of the boundary interface.  Just ignore the call.
 */
template <typename TPixelType, typename TCellType>
int
itkCell< TPixelType , TCellType >
::GetNumUsingCells(void)
{
  return 0;
}


/**
 * This is only part of the boundary interface.  Just ignore the call.
 */
template <typename TPixelType, typename TCellType>
itkCell< TPixelType , TCellType >::UsingCellsContainerIterator
itkCell< TPixelType , TCellType >
::UsingCellsBegin(void)
{
  return UsingCellsContainerIterator();
}


/**
 * This is only part of the boundary interface.  Just ignore the call.
 */
template <typename TPixelType, typename TCellType>
itkCell< TPixelType , TCellType >::UsingCellsContainerIterator
itkCell< TPixelType , TCellType >
::UsingCellsEnd(void)
{
  return UsingCellsContainerIterator();
}


/**
 * Increase the reference count (mark as used by another object).
 */
template <typename TPixelType, typename TCellType>
void
itkCell< TPixelType , TCellType >
::Register(void)
{
  m_ReferenceCount++;
  if(m_ReferenceCount <= 0)
    {
    delete this;
    }
}


/**
 * Decrease the reference count (release by another object).
 * If there are no references left, delete ourself.
 */
template <typename TPixelType, typename TCellType>
void
itkCell< TPixelType , TCellType >
::UnRegister(void)
{
  --m_ReferenceCount;
  if(m_ReferenceCount <= 0)
    {
    // invoke the delete method
//    if(m_DeleteMethod != NULL)
//      {
//      (*m_DeleteMethod)(this);
//      }
    delete this;
    }
}


/**
 * Constructor.
 * Just initialize the reference count.
 */
template <typename TPixelType, typename TCellType>
itkCell< TPixelType , TCellType >
::itkCell(): m_ReferenceCount(0)
{
}

