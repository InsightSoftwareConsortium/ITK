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
 * Get the number of boundary entities of the given dimension.
 */
template <typename TPixelType, typename TMeshType>
itkVertexCell< TPixelType , TMeshType >::CellFeatureCount
itkVertexCell< TPixelType , TMeshType >
::GetNumberOfBoundaryEntities(int dimension)
{
  switch (dimension)
    {
    case 0: return NumberOfPoints;
    default: return 0;
    }
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
  for(int i=0; i < NumberOfPoints ; ++i)
    m_PointIds[i] = ptList[i];
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
  return m_PointIds[0];
}

