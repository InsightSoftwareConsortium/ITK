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


template <typename TPixelType, typename TMeshType>
itkCell< TPixelType , TMeshType >::Point
itkCell< TPixelType , TMeshType >
::GetPointPosition(Mesh* mesh, int localID)
{
  return (*(mesh->m_Points))[m_Points[localID]];
}


template <typename TPixelType, typename TMeshType>
void
itkCell< TPixelType , TMeshType >
::SetCellPoint(CellFeatureID featureId, PointIdentifier ptId)
{
  m_Points[featureId] = ptId;
}

