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
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TMeshType>
int
itkLineCell< TPixelType , TMeshType >
::GetCellDimension(void)
{
  return CellDimension;
}


/**
 * Get the number of boundary entities of the given dimension.
 */
template <typename TPixelType, typename TMeshType>
itkLineCell< TPixelType , TMeshType >::CellFeatureCount
itkLineCell< TPixelType , TMeshType >
::GetNumberOfBoundaryFeatures(int dimension)
{
  switch (dimension)
    {
    case 0: return GetNumberOfVertices();
    default: return 0;
    }
}


/**
 * Get the boundary feature of the given dimension specified by the given
 * cell feature Id.
 */
template <typename TPixelType, typename TMeshType>
itkLineCell< TPixelType , TMeshType >::Cell::Pointer
itkLineCell< TPixelType , TMeshType >
::GetBoundaryFeature(int dimension, CellFeatureIdentifier featureId)
{
  switch (dimension)
    {
    case 0: return Cell::Pointer(GetCellVertex(featureId));
    default: return Cell::Pointer(NULL);
    }
}


/**
 * Standard itkCell API:
 * Set the cell's internal point list to the list of identifiers provided.
 */
template <typename TPixelType, typename TMeshType>
void
itkLineCell< TPixelType , TMeshType >
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
template <typename TPixelType, typename TMeshType>
void
itkLineCell< TPixelType , TMeshType >
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
itkLineCell< TPixelType , TMeshType >
::SetCellPoint(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
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
 * Get the vertex specified by the given cell feature Id.
 */
template <typename TPixelType, typename TMeshType>
itkLineCell< TPixelType , TMeshType >::Vertex::Pointer
itkLineCell< TPixelType , TMeshType >
::GetCellVertex(CellFeatureIdentifier vertexId)
{
  Vertex::Pointer vert(Vertex::New());
  vert->SetCellPoint(0, m_PointIds[vertexId]);
  
  return vert;  
}

