/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLineCell.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// #include "itkLineCell.h"

/**
 *
 */
template <typename TPixelType, typename TMeshType>
itkLineCell< TPixelType , TMeshType >::Pointer
itkLineCell< TPixelType , TMeshType >
::New(void)
{
  return new Self;
}


/**
 *
 */
template <typename TPixelType, typename TMeshType>
itkLineCell< TPixelType , TMeshType >::CellFeatureID
itkLineCell< TPixelType , TMeshType >
::GetNumberOfBoundaryEntities(void)
{
  return 2;
}


/**
 * Standard itkCell API:
 * Set the cell's internal point list to the list of identifiers provided.
 */
template <typename TPixelType, typename TMeshType>
void
itkLineCell< TPixelType , TMeshType >
::SetCellPoints(PointIdentifier *ptList)
{
  m_Points[0] = ptList[0];
  m_Points[1] = ptList[1];
}


/**
 * Line-specific:
 * Get the number of vertices for this cell.
 */
template <typename TPixelType, typename TMeshType>
itkLineCell< TPixelType , TMeshType >::CellFeatureID
itkLineCell< TPixelType , TMeshType >
::GetNumberOfVertices(void)
{
  return 2;
}


/**
 * Line-specific:
 * Get the vertex specified by the given cell feature ID.
 */
template <typename TPixelType, typename TMeshType>
itkLineCell< TPixelType , TMeshType >::Vertex::Pointer
itkLineCell< TPixelType , TMeshType >
::GetCellVertex(CellFeatureID vertexId)
{
  Vertex::Pointer vert(Vertex::New());
  vert->SetCellPoint(0, m_Points[vertexId]);
  
  return vert;  
}

