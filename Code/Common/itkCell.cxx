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
 * Use this to set all the points in the cell.  It is assumed that the
 * range [first, last) is exactly the size needed for the cell type on which
 * the operation is done.  The position *last is NOT referenced, so it
 * can safely be one beyond the end of an array.
 */
template <typename TPixelType, typename TMeshType>
void
itkCell< TPixelType , TMeshType >
::SetCellPoints(const PointIdentifier* first, const PointIdentifier* last)
{
  int localId=0;
  const PointIdentifier *ii = first;
  
  while(ii != last)
    m_PointIds[localId++] = *ii++;
}


/**
 * Use this to set an individual point identifier in the cell.
 */
template <typename TPixelType, typename TMeshType>
void
itkCell< TPixelType , TMeshType >
::SetCellPoint(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Used internally by a cell to get the geometric position of a point.
 */
template <typename TPixelType, typename TMeshType>
bool
itkCell< TPixelType , TMeshType >
::GetPointPosition(Mesh* mesh, int localId, Point* point)
{
  return mesh->m_Points->GetElementIfIndexExists(m_PointIds[localId], point);
}

