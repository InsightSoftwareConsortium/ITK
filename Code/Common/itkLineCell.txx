/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLineCell.txx
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
 * Get the number of boundary entities of the given dimension.
 */
template <typename TPixelType, typename TMeshType>
itkLineCell< TPixelType , TMeshType >::CellFeatureCount
itkLineCell< TPixelType , TMeshType >
::GetNumberOfBoundaryEntities(int dimension)
{
  switch (dimension)
    {
    case 0: return GetNumberOfVertices();
    default: return 0;
    }
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
  for(int i=0; i < NumberOfPoints ; ++i)
    m_PointIds[i] = ptList[i];
}


/**
 * Line-specific:
 * Get the number of vertices for this cell.
 */
template <typename TPixelType, typename TMeshType>
itkLineCell< TPixelType , TMeshType >::CellFeatureCount
itkLineCell< TPixelType , TMeshType >
::GetNumberOfVertices(void)
{
  return NumberOfPoints;
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
  vert->SetCellPoint(0, m_PointIds[vertexId]);
  
  return vert;  
}

