/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolygonCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPolygonCell_txx
#define _itkPolygonCell_txx
#include "itkPolygonCell.h"


namespace itk
{

/**
 * Standard CellInterface:
 */
template <typename TCellInterface>
void
PolygonCell< TCellInterface >
::MakeCopy(CellAutoPointer & cellPointer) const
{
  cellPointer.TakeOwnership( new Self );
  cellPointer->SetPointIds(this->GetPointIds());
}


  
/**
 * Standard CellInterface:
 * Get the topological dimension of this cell.
 */
template <typename TCellInterface>
unsigned int
PolygonCell< TCellInterface >
::GetDimension(void) const
{
  return Self::CellDimension;
}


/**
 * Standard CellInterface:
 * Get the number of points required to define the cell.
 */
template <typename TCellInterface>
unsigned int
PolygonCell< TCellInterface >
::GetNumberOfPoints(void) const
{
  return m_PointIds.size();
}  


/**
 * Standard CellInterface:
 * Get the number of boundary features of the given dimension.
 */
template <typename TCellInterface>
typename PolygonCell< TCellInterface >::CellFeatureCount
PolygonCell< TCellInterface >
::GetNumberOfBoundaryFeatures(int dimension) const
{
  switch (dimension)
    {
    case 0: return this->GetNumberOfVertices();
    case 1: return this->GetNumberOfEdges();
    default: return 0;
    }
}


/**
 * Standard CellInterface:
 * Get the boundary feature of the given dimension specified by the given
 * cell feature Id.
 * The Id can range from 0 to GetNumberOfBoundaryFeatures(dimension)-1.
 */
template <typename TCellInterface>
bool
PolygonCell< TCellInterface >
::GetBoundaryFeature(int dimension, CellFeatureIdentifier featureId,
                      CellAutoPointer& cellPointer )
{
  switch (dimension)
    {
    case 0: 
      {
      VertexAutoPointer vertexPointer;
      if( this->GetVertex(featureId,vertexPointer) )
        {
        TransferAutoPointer(cellPointer,vertexPointer);
        return true;
        }
      else
        {
        cellPointer.Reset();
        return false;
        }
      break;
      }
    case 1: 
      {
      EdgeAutoPointer edgePointer;
      if( this->GetEdge(featureId,edgePointer) )
        {
        TransferAutoPointer(cellPointer,edgePointer);
        return true;
        }
      else
        {
        cellPointer.Reset();
        return false;
        }
      break;
      }

    default: 
      {
      cellPointer.Reset();
      return false;
      }
    }
  return false;
}



/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the given
 * iterator can be incremented and safely de-referenced enough times to 
 * get all the point ids needed by the cell.
 */ 
template <typename TCellInterface>
void
PolygonCell< TCellInterface >
::SetPointIds(int dummy, int num, PointIdConstIterator first)
{
  PointIdConstIterator ii(first);
  m_PointIds.clear();
  for(int i=0; i < num ; ++i)
    {
    m_PointIds.push_back(*ii++);
    }
  this->BuildEdges();
}

/**
 * after input the points in order, generate the edge connections
 */
template <typename TCellInterface>
void
PolygonCell< TCellInterface >
::BuildEdges(void)
{
  if( m_PointIds.size() > 0 )
    {
    m_Edges.resize(m_PointIds.size());
    const unsigned int numberOfPoints = m_PointIds.size();
    for(unsigned int i = 1; i < numberOfPoints; i++)
      {
      m_Edges[i-1][0]=i-1;
      m_Edges[i-1][1]=i;
      }
    m_Edges[m_PointIds.size()-1][0]=m_PointIds.size()-1;
    m_Edges[m_PointIds.size()-1][1]=0;
    }
  else
    {
    m_Edges.clear();
    }
}

/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the given
 * iterator can be incremented and safely de-referenced enough times to 
 * get all the point ids needed by the cell.
 */ 
template <typename TCellInterface>
void
PolygonCell< TCellInterface >
::SetPointIds(PointIdConstIterator first)
{
}

/** 
 * Add one points to the points list
 */
template <typename TCellInterface>
void
PolygonCell< TCellInterface >
::AddPointId(PointIdentifier ptID)
{
  m_PointIds.push_back(ptID);
}

/**
 * clear all the point and edge informations
 */
template <typename TCellInterface>
void
PolygonCell< TCellInterface >
::ClearPoints(void)
{
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
template <typename TCellInterface>
void
PolygonCell< TCellInterface >
::SetPointIds(PointIdConstIterator first, PointIdConstIterator last)
{
  m_PointIds.erase(m_PointIds.begin(), m_PointIds.end());
  m_PointIds.insert(m_PointIds.begin(), first, last);
  this->BuildEdges();
}

/**
 * Standard CellInterface:
 * Set an individual point identifier in the cell.
 */ 
template <typename TCellInterface>
void
PolygonCell< TCellInterface >
::SetPointId(int localId, PointIdentifier ptId)
{
  if(m_PointIds.size() < (unsigned int)(localId + 1)) {
    m_PointIds.resize( localId + 1 );
  }
  m_PointIds[localId] = ptId;
}


/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TCellInterface>
typename PolygonCell< TCellInterface >::PointIdIterator
PolygonCell< TCellInterface >
::PointIdsBegin(void)
{
  return &*(m_PointIds.begin());
}


/**
 * Standard CellInterface:
 * Get a const begin iterator to the list of point identifiers used
 * by the cell.
 */ 
template <typename TCellInterface>
typename PolygonCell< TCellInterface >::PointIdConstIterator
PolygonCell< TCellInterface >
::PointIdsBegin(void) const
{
  return &*(m_PointIds.begin());
}


/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */ 
template <typename TCellInterface>
typename PolygonCell< TCellInterface >::PointIdIterator
PolygonCell< TCellInterface >
::PointIdsEnd(void)
{
  return &*(m_PointIds.end());
}


/**
 * Standard CellInterface:
 * Get a const end iterator to the list of point identifiers used
 * by the cell.
 */ 
template <typename TCellInterface>
typename PolygonCell< TCellInterface >::PointIdConstIterator
PolygonCell< TCellInterface >
::PointIdsEnd(void) const
{
  return &*(m_PointIds.end());
}


/**
 * Polygon-specific:
 * Get the number of vertices defining the Polygon.
 */
template <typename TCellInterface>
typename PolygonCell< TCellInterface >::CellFeatureCount
PolygonCell< TCellInterface >
::GetNumberOfVertices(void) const
{
  return m_PointIds.size();
}

/**
 * Polygon-specific:
 * Get the number of edges defined for the Polygon.
 */
template <typename TCellInterface>
typename PolygonCell< TCellInterface >::CellFeatureCount
PolygonCell< TCellInterface >
::GetNumberOfEdges(void) const
{
  return m_Edges.size();
}

/**
 * Polygon-specific:
 * Get the vertex specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfVertices()-1.
 */ 
template <typename TCellInterface>
bool
PolygonCell< TCellInterface >
::GetVertex(CellFeatureIdentifier vertexId,VertexAutoPointer & vertexPointer )
{
  VertexType * vert = new VertexType;
  vert->SetPointId(0, m_PointIds[vertexId]);
  vertexPointer.TakeOwnership( vert );
  return true;  
}

/**
 * Polygon-specific:
 * Get the edge specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfEdges()-1.
 */ 
template <typename TCellInterface>
bool
PolygonCell< TCellInterface >
::GetEdge(CellFeatureIdentifier edgeId, EdgeAutoPointer & edgePointer )
{
  EdgeType * edge = new EdgeType;
  unsigned int max_pointId = this->GetNumberOfPoints() - 1;
  if( edgeId < max_pointId ){
    edge->SetPointId(0, m_PointIds[edgeId]);
    edge->SetPointId(1, m_PointIds[edgeId+1]);
  }
  else if( edgeId == max_pointId ) {
    edge->SetPointId(0, m_PointIds[max_pointId] );
    edge->SetPointId(1, m_PointIds[0] );
  }
  edgePointer.TakeOwnership( edge ); 
  return true;
}

} // end namespace itk

#endif
