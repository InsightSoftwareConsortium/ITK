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
itkCell< TPixelType , TMeshType >::Point
itkCell< TPixelType , TMeshType >
::GetPointPosition(Mesh* mesh, int localId)
{
  return (*(mesh->m_PointIds))[m_PointIds[localId]];
}

