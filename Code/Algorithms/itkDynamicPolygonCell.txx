/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDynamicPolygonCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkDynamicPolygonCell_txx
#define _itkDynamicPolygonCell_txx


namespace itk
{


/**
 * Standard CellInterface:
 */
template <typename TPixelType, typename TCellTraits>
DynamicPolygonCell< TPixelType , TCellTraits >::CellPointer
DynamicPolygonCell< TPixelType , TCellTraits >
::MakeCopy(void)
{

  CellPointer newCell(Self::New());
  newCell->SetPointIds(this->PointIdsBegin(),this->PointIdsEnd());
  return newCell;
}

  
/**
 * Standard CellInterface:
 * Get the topological dimension of this cell.
 */
template <typename TPixelType, typename TCellTraits>
int
DynamicPolygonCell< TPixelType , TCellTraits >
::GetDimension(void)
{
  return Self::CellDimension;
}


/**
 * Standard CellInterface:
 * Get the number of points required to define the cell.
 */
template <typename TPixelType, typename TCellTraits>
int
DynamicPolygonCell< TPixelType , TCellTraits >
::GetNumberOfPoints(void)
{
  return m_PointIds.size();
}  


/**
 * Standard CellInterface:
 * Get the number of boundary features of the given dimension.
 */
template <typename TPixelType, typename TCellTraits>
DynamicPolygonCell< TPixelType , TCellTraits >::CellFeatureCount
DynamicPolygonCell< TPixelType , TCellTraits >
::GetNumberOfBoundaryFeatures(int dimension)
{
  switch (dimension)
    {
    case 0: return GetNumberOfVertices();
    case 1: return GetNumberOfEdges();
    default: return 0;
    }
}


/**
 * Standard CellInterface:
 * Get the boundary feature of the given dimension specified by the given
 * cell feature Id.
 * The Id can range from 0 to GetNumberOfBoundaryFeatures(dimension)-1.
 */
template <typename TPixelType, typename TCellTraits>
DynamicPolygonCell< TPixelType , TCellTraits >::CellPointer
DynamicPolygonCell< TPixelType , TCellTraits >
::GetBoundaryFeature(int dimension, CellFeatureIdentifier featureId)
{
  switch (dimension)
    {
    case 0: return CellPointer(GetVertex(featureId));
    case 1: return CellPointer(GetEdge(featureId));
    default: return CellPointer(NULL);
    }
}


/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the given
 * iterator can be incremented and safely de-referenced enough times to 
 * get all the point ids needed by the cell.
 */ 
template <typename TPixelType, typename TCellTraits>
void
DynamicPolygonCell< TPixelType , TCellTraits >
::SetPointIds(int dummy, int num, PointIdConstIterator first)
{
  PointIdConstIterator ii(first);
  m_PointIds.clear();
  for(int i=0; i < num ; ++i)
    {
    m_PointIds.push_back(*ii++);
    }
  m_NumberOfPoints = num;
  BuildEdges();
}

/**
 * after input the points in order, generate the edge connections
 */
template <typename TPixelType, typename TCellTraits>
void
DynamicPolygonCell< TPixelType , TCellTraits >
::BuildEdges(void){
  m_Edges.clear();
  m_Edges.resize(m_NumberOfPoints);
  for(int i = 1;i < m_NumberOfPoints; i++){
    m_Edges[i][0]=i-1;
    m_Edges[i][1]=i;
  }
  m_Edges[m_NumberOfPoints-1][0]=m_NumberOfPoints-1;
  m_Edges[m_NumberOfPoints-1][1]=0;
  m_NumberOfEdges=m_NumberOfPoints;
}

/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the given
 * iterator can be incremented and safely de-referenced enough times to 
 * get all the point ids needed by the cell.
 */ 
template <typename TPixelType, typename TCellTraits>
void
DynamicPolygonCell< TPixelType , TCellTraits >
::SetPointIds(PointIdConstIterator first)
{
}

/** 
 * Add one points to the points list
 */
template <typename TPixelType, typename TCellTraits>
void
DynamicPolygonCell< TPixelType , TCellTraits >
::AddPointId(PointIdentifier ptID)
{
  m_PointIds.push_back(ptID);
  m_NumberOfPoints++;
}

/**
 * clear all the point and edge informations
 */
template <typename TPixelType, typename TCellTraits>
void
DynamicPolygonCell< TPixelType , TCellTraits >
::clearPoints(void)
{
  m_NumberOfPoints=0;
  m_NumberOfEdges=0;
  m_PointIds.clear();
  m_Edges.clear();
}

/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the range
 * of iterators [first, last) contains the correct number of points needed to
 * define the cell.  The position *last is NOT referenced, so it can safely
 * be one beyond the end of an array or other container.
 */ 
template <typename TPixelType, typename TCellTraits>
void
DynamicPolygonCell< TPixelType , TCellTraits >
::SetPointIds(PointIdConstIterator first, PointIdConstIterator last)
{
  m_PointIds.erase(m_PointIds.begin(), m_PointIds.end());
  m_PointIds.insert(m_PointIds.begin(), first, last);
  m_NumberOfPoints = m_PointIds.size();
  BuildEdges();
}

/**
 * Standard CellInterface:
 * Set an individual point identifier in the cell.
 */ 
template <typename TPixelType, typename TCellTraits>
void
DynamicPolygonCell< TPixelType , TCellTraits >
::SetPointId(int localId, PointIdentifier ptId)
{
  if(m_PointIds.size() < (unsigned int)(localId + 1)) {
    m_PointIds.resize( localId + 1 );
  }
  m_PointIds[localId] = ptId;
  m_NumberOfPoints = m_PointIds.size();
}


/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TPixelType, typename TCellTraits>
DynamicPolygonCell< TPixelType , TCellTraits >::PointIdIterator
DynamicPolygonCell< TPixelType , TCellTraits >
::PointIdsBegin(void)
{
  return &*(m_PointIds.begin());
}


/**
 * Standard CellInterface:
 * Get a const begin iterator to the list of point identifiers used
 * by the cell.
 */ 
template <typename TPixelType, typename TCellTraits>
DynamicPolygonCell< TPixelType , TCellTraits >::PointIdConstIterator
DynamicPolygonCell< TPixelType , TCellTraits >
::PointIdsBegin(void) const
{
  return &*(m_PointIds.begin());
}


/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */ 
template <typename TPixelType, typename TCellTraits>
DynamicPolygonCell< TPixelType , TCellTraits >::PointIdIterator
DynamicPolygonCell< TPixelType , TCellTraits >
::PointIdsEnd(void)
{
  return &*(m_PointIds.end());
}


/**
 * Standard CellInterface:
 * Get a const end iterator to the list of point identifiers used
 * by the cell.
 */ 
template <typename TPixelType, typename TCellTraits>
DynamicPolygonCell< TPixelType , TCellTraits >::PointIdConstIterator
DynamicPolygonCell< TPixelType , TCellTraits >
::PointIdsEnd(void) const
{
  return &*(m_PointIds.end());
}


/**
 * DynamicPolygon-specific:
 * Get the number of vertices defining the DynamicPolygon.
 */
template <typename TPixelType, typename TCellTraits>
DynamicPolygonCell< TPixelType , TCellTraits >::CellFeatureCount
DynamicPolygonCell< TPixelType , TCellTraits >
::GetNumberOfVertices(void)
{
  return m_PointIds.size();
}

/**
 * DynamicPolygon-specific:
 * Get the number of edges defined for the DynamicPolygon.
 */
template <typename TPixelType, typename TCellTraits>
DynamicPolygonCell< TPixelType , TCellTraits >::CellFeatureCount
DynamicPolygonCell< TPixelType , TCellTraits >
::GetNumberOfEdges(void)
{
  return m_Edges.size();
}

/**
 * DynamicPolygon-specific:
 * Get the vertex specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfVertices()-1.
 */ 
template <typename TPixelType, typename TCellTraits>
DynamicPolygonCell< TPixelType , TCellTraits >::VertexPointer
DynamicPolygonCell< TPixelType , TCellTraits >
::GetVertex(CellFeatureIdentifier vertexId)
{
  VertexPointer vert(Vertex::New());
  vert->SetPointId(0, m_PointIds[vertexId]);
  
  return vert;
}


/**
 * DynamicPolygon-specific:
 * Get the edge specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfEdges()-1.
 */ 
template <typename TPixelType, typename TCellTraits>
DynamicPolygonCell< TPixelType , TCellTraits >::EdgePointer
DynamicPolygonCell< TPixelType , TCellTraits >
::GetEdge(CellFeatureIdentifier edgeId)
{
  EdgePointer edge(Edge::New());
  unsigned int max_pointId = GetNumberOfPoints() - 1;

  if( edgeId < max_pointId ){
    edge->SetPointId(0, m_PointIds[edgeId]);
    edge->SetPointId(1, m_PointIds[edgeId+1]);
  }
  else if( edgeId == max_pointId ) {
    edge->SetPointId(0, m_PointIds[max_pointId] );
    edge->SetPointId(1, m_PointIds[0] );
  }
  return edge;
}


} // end namespace itk

#endif
