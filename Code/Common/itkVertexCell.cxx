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
template <typename TPixelType, typename TCellType>
itkVertexCell< TPixelType , TCellType >::Pointer
itkVertexCell< TPixelType , TCellType >
::New(void)
{
  return new Self;
}


/**
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TCellType>
int
itkVertexCell< TPixelType , TCellType >
::GetCellDimension(void)
{
  return CellDimension;
}


/**
 * A vertex has no boundary entities of any dimension.
 */
template <typename TPixelType, typename TCellType>
itkVertexCell< TPixelType , TCellType >::CellFeatureCount
itkVertexCell< TPixelType , TCellType >
::GetNumberOfBoundaryFeatures(int)
{
  return 0;
}


/**
 * A vertex has no boundary entities.  Just return null.
 */
template <typename TPixelType, typename TCellType>
itkVertexCell< TPixelType , TCellType >::Cell::Pointer
itkVertexCell< TPixelType , TCellType >
::GetBoundaryFeature(int, CellFeatureIdentifier)
{
  return Cell::Pointer(NULL);
}


/**
 * Standard itkCell API:
 * Set the cell's internal point list to the list of identifiers provided.
 */
template <typename TPixelType, typename TCellType>
void
itkVertexCell< TPixelType , TCellType >
::SetCellPoints(const PointIdentifier *ptList)
{
  for(int i=0; i < NumberOfPoints ; ++i)
    m_PointIds[i] = ptList[i];
}


/**
 * Standard itkCell API:
 * Use this to set all the points in the cell.  It is assumed that the
 * range [first, last) is exactly the size needed for this cell type.
 * The position *last is NOT referenced, so it can safely be one beyond
 * the end of an array.
 */
template <typename TPixelType, typename TCellType>
void
itkVertexCell< TPixelType , TCellType >
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
template <typename TPixelType, typename TCellType>
void
itkVertexCell< TPixelType , TCellType >
::SetCellPoint(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Vertex-specific:
 * Get the identifier of the point defining the vertex.
 */
template <typename TPixelType, typename TCellType>
itkVertexCell< TPixelType , TCellType >::PointIdentifier
itkVertexCell< TPixelType , TCellType >
::GetCellPoint(void)
{
  return m_PointIds[0];
}

