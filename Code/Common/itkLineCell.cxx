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

namespace itk
{

/**
 *
 */
template <typename TPixelType, typename TCellType>
LineCell< TPixelType , TCellType >::Pointer
LineCell< TPixelType , TCellType >
::New(void)
{
  return new Self;
}


/**
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TCellType>
int
LineCell< TPixelType , TCellType >
::GetCellDimension(void)
{
  return CellDimension;
}


/**
 * Get the number of boundary entities of the given dimension.
 */
template <typename TPixelType, typename TCellType>
LineCell< TPixelType , TCellType >::CellFeatureCount
LineCell< TPixelType , TCellType >
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
template <typename TPixelType, typename TCellType>
LineCell< TPixelType , TCellType >::Cell::Pointer
LineCell< TPixelType , TCellType >
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
template <typename TPixelType, typename TCellType>
void
LineCell< TPixelType , TCellType >
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
LineCell< TPixelType , TCellType >
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
LineCell< TPixelType , TCellType >
::SetCellPoint(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Provide iterator begin and end for the point identifier array.  These
 * are just pointers to the beginning and one past the end.
 */
template <typename TPixelType, typename TCelltype>
LineCell< TPixelType , TCelltype >::PointIterator
LineCell< TPixelType , TCelltype >
::PointIdsBegin(void)
{
  return &m_PointIds[0];
}

template <typename TPixelType, typename TCelltype>
LineCell< TPixelType , TCelltype >::PointConstIterator
LineCell< TPixelType , TCelltype >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}

template <typename TPixelType, typename TCelltype>
LineCell< TPixelType , TCelltype >::PointIterator
LineCell< TPixelType , TCelltype >
::PointIdsEnd(void)
{
  return &m_PointIds[NumberOfPoints];
}

template <typename TPixelType, typename TCelltype>
LineCell< TPixelType , TCelltype >::PointConstIterator
LineCell< TPixelType , TCelltype >
::PointIdsEnd(void) const
{
  return &m_PointIds[NumberOfPoints];
}


/**
 * Line-specific:
 * Get the number of vertices for this cell.
 */
template <typename TPixelType, typename TCellType>
LineCell< TPixelType , TCellType >::CellFeatureCount
LineCell< TPixelType , TCellType >
::GetNumberOfVertices(void)
{
  return NumberOfPoints;
}


/**
 * Line-specific:
 * Get the vertex specified by the given cell feature Id.
 */
template <typename TPixelType, typename TCellType>
LineCell< TPixelType , TCellType >::Vertex::Pointer
LineCell< TPixelType , TCellType >
::GetCellVertex(CellFeatureIdentifier vertexId)
{
  Vertex::Pointer vert(Vertex::New());
  vert->SetCellPoint(0, m_PointIds[vertexId]);
  
  return vert;  
}


/**
 * Object factory for the boundary version of this cell type.
 */
template <typename TPixelType, typename TCellType>
LineBoundary< TPixelType , TCellType >::Pointer
LineBoundary< TPixelType , TCellType >
::New(void)
{
  return new Self;
}

} // namespace itk
