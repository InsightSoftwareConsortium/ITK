/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVertexCell.cxx
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
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TMeshType>
int
itkVertexCell< TPixelType , TMeshType >
::GetCellDimension(void)
{
  return CellDimension;
}


/**
 * A vertex has no boundary entities of any dimension.
 */
template <typename TPixelType, typename TMeshType>
itkVertexCell< TPixelType , TMeshType >::CellFeatureCount
itkVertexCell< TPixelType , TMeshType >
::GetNumberOfBoundaryFeatures(int)
{
  return 0;
}


/**
 * A vertex has no boundary entities.  Just return null.
 */
template <typename TPixelType, typename TMeshType>
itkVertexCell< TPixelType , TMeshType >::Cell::Pointer
itkVertexCell< TPixelType , TMeshType >
::GetBoundaryFeature(int, CellFeatureId)
{
  return Cell::Pointer(NULL);
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

