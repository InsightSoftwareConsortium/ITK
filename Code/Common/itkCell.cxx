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
template <typename TPixelType, typename TMeshType>
bool
itkCell< TPixelType , TMeshType >
::IsBoundary(void)
{
  return false;
}


/**
 * This is only part of the boundary interface.  Just ignore the call.
 */
template <typename TPixelType, typename TMeshType>
void
itkCell< TPixelType , TMeshType >
::AddUsingCell(CellIdentifier)
{
}


/**
 * This is only part of the boundary interface.  Just ignore the call.
 */
template <typename TPixelType, typename TMeshType>
void
itkCell< TPixelType , TMeshType >
::RemoveUsingCell(CellIdentifier)
{
}


/**
 * By default, the cell is not a boundary, so it has no using cells.
 */
template <typename TPixelType, typename TMeshType>
bool
itkCell< TPixelType , TMeshType >
::IsUsingCell(CellIdentifier)
{
  return false;
}


/**
 * This is only part of the boundary interface.  Just ignore the call.
 */
template <typename TPixelType, typename TMeshType>
int
itkCell< TPixelType , TMeshType >
::GetNumUsingCells(void)
{
  return 0;
}


/**
 * This is only part of the boundary interface.  Just ignore the call.
 */
template <typename TPixelType, typename TMeshType>
itkCell< TPixelType , TMeshType >::UsingCellsContainer::iterator
itkCell< TPixelType , TMeshType >
::UsingCellsBegin(void)
{
  return UsingCellsContainer::iterator();
}


/**
 * This is only part of the boundary interface.  Just ignore the call.
 */
template <typename TPixelType, typename TMeshType>
itkCell< TPixelType , TMeshType >::UsingCellsContainer::iterator
itkCell< TPixelType , TMeshType >
::UsingCellsEnd(void)
{
  return UsingCellsContainer::iterator();
}


/**
 * Increase the reference count (mark as used by another object).
 */
template <typename TPixelType, typename TMeshType>
void
itkCell< TPixelType , TMeshType >
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
template <typename TPixelType, typename TMeshType>
void
itkCell< TPixelType , TMeshType >
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
