/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVertexCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkVertexCell.h"


/**
 *
 */
template <typename TPixelType, typename TMeshType>
itkVertexCell< TPixelType , TMeshType >::Pointer
itkVertexCell< TPixelType , TMeshType >
::New(void)
{
  return new Self;
}


/**
 *
 */
template <typename TPixelType, typename TMeshType>
itkVertexCell< TPixelType , TMeshType >::CellFeatureID
itkVertexCell< TPixelType , TMeshType >
::GetNumberOfBoundaryEntities(void)
{
  return 1;
}


/**
 * Standard itkCell API:
 * Set the cell's internal point list to the list of identifiers provided.
 */
template <typename TPixelType, typename TMeshType>
void
itkVertexCell< TPixelType , TMeshType >
::SetCellPoints(PointIdentifier *ptList)
{
  m_Points[0] = ptList[0];
}


/**
 * Vertex-specific:
 * Get the identifier of the point defining the vertex.
 */
template <typename TPixelType, typename TMeshType>
itkVertexCell< TPixelType , TMeshType >::PointIdentifier
itkVertexCell< TPixelType , TMeshType >
::GetCellPoint(void)
{
  return m_Points[0];
}

