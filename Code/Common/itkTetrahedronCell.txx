/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTetrahedronCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkTetrahedronCell_txx
#define _itkTetrahedronCell_txx
#include "itkTetrahedronCell.h"
#include "itkTriangleCell.h"
#include "vnl/algo/vnl_determinant.h"

namespace itk
{

/**
 * Standard CellInterface:
 */
template <typename TCellInterface>
void
TetrahedronCell< TCellInterface >
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
TetrahedronCell< TCellInterface >
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
TetrahedronCell< TCellInterface >
::GetNumberOfPoints(void) const
{
  return Self::NumberOfPoints;
}  


/**
 * Standard CellInterface:
 * Get the number of boundary features of the given dimension.
 */
template <typename TCellInterface>
typename TetrahedronCell< TCellInterface >::CellFeatureCount
TetrahedronCell< TCellInterface >
::GetNumberOfBoundaryFeatures(int dimension) const
{
  switch (dimension)
    {
    case 0: return GetNumberOfVertices();
    case 1: return GetNumberOfEdges();
    case 2: return GetNumberOfFaces();
    default: return 0;
    }
}


template <typename TCellInterface>
bool
TetrahedronCell< TCellInterface >
::EvaluatePosition(CoordRepType* x,
                                PointsContainer* points,
                                CoordRepType* closestPoint,
                                CoordRepType pcoord[3],
                                double* minDist2,
                                InterpolationWeightType* weights)
{
  unsigned int i;
  double rhs[PointDimension], c1[PointDimension], c2[PointDimension], c3[PointDimension];
  double det, p4;

  CoordRepType pcoords[3];
  pcoords[0] = pcoords[1] = pcoords[2] = 0.0;

  if(!points)
    {
    return false;
    }

  PointType pt1 = points->GetElement(0);
  PointType pt2 = points->GetElement(1);
  PointType pt3 = points->GetElement(2);
  PointType pt4 = points->GetElement(3);

  for (i=0; i<PointDimension; i++)
    {
    rhs[i] = x[i] - pt4[i];
    c1[i] = pt1[i] - pt4[i];
    c2[i] = pt2[i] - pt4[i];
    c3[i] = pt3[i] - pt4[i];
    }

  // Create a vnl_matrix so that the determinant can be computed 
  // for any PointDimension
  vnl_matrix_fixed<CoordRepType,3,PointDimension> mat;
  for(i=0;i<PointDimension;i++)
    {
    mat.put(0,i,c1[i]);
    mat.put(1,i,c2[i]);
    mat.put(2,i,c3[i]);
    }

  if ( (det = vnl_determinant(mat)) == 0.0 )
    {
    return false;
    }

  
  for(i=0;i<PointDimension;i++)
    {
    mat.put(0,i,rhs[i]);
    mat.put(1,i,c2[i]);
    mat.put(2,i,c3[i]);
    }

  pcoords[0] = vnl_determinant(mat) / det;

  for(i=0;i<PointDimension;i++)
    {
    mat.put(0,i,c1[i]);
    mat.put(1,i,rhs[i]);
    mat.put(2,i,c3[i]);
    }

  pcoords[1] = vnl_determinant(mat) / det;
  
  for(i=0;i<PointDimension;i++)
    {
    mat.put(0,i,c1[i]);
    mat.put(1,i,c2[i]);
    mat.put(2,i,rhs[i]);
    }

  pcoords[2] = vnl_determinant(mat) / det;

  p4 = 1.0 - pcoords[0] - pcoords[1] - pcoords[2];

  if(weights)
    {
    weights[0] = p4;
    weights[1] = pcoords[0];
    weights[2] = pcoords[1];
    weights[3] = pcoords[2];
    }

  if(pcoord)
    {
    pcoord[0] = pcoords[0]; 
    pcoord[1] = pcoords[1];
    pcoord[2] = pcoords[2];
    }

  if ( pcoords[0] >= -0.001 && pcoords[0] <= 1.001 &&
  pcoords[1] >= -0.001 && pcoords[1] <= 1.001 &&
  pcoords[2] >= -0.001 && pcoords[2] <= 1.001 && p4 >= -0.001 && p4 <= 1.001 )
    {
    if (closestPoint)
      {
      for(unsigned int i=0;i <PointDimension;i++)
        {
        closestPoint[i] = x[i]; 
        }
      if(minDist2)
        {
        *minDist2 = 0.0; //inside tetra
        }
      }
    return true; 
    }
  else
    { //could easily be sped up using parametric localization - next release
    double dist2;
    CoordRepType  closest[PointDimension], pc[3];

    if (closestPoint)
      {
      FaceAutoPointer triangle;
      *minDist2 = NumericTraits< double >::max();
      for (i=0; i<4; i++)
        {
        this->GetFace (i,triangle);
        triangle->EvaluatePosition(x,points,closest,pc,&dist2,NULL);
        
        if ( dist2 < *minDist2 )
          {
          closestPoint[0] = closest[0]; 
          closestPoint[1] = closest[1]; 
          closestPoint[2] = closest[2];
          *minDist2 = dist2;
          }
        }
      }
    //Just fall through to default return false;
    }
    return false;
}


/**
 * Standard CellInterface:
 * Get the boundary feature of the given dimension specified by the given
 * cell feature Id.
 * The Id can range from 0 to GetNumberOfBoundaryFeatures(dimension)-1.
 */
template <typename TCellInterface>
bool
TetrahedronCell< TCellInterface >
::GetBoundaryFeature(int dimension, CellFeatureIdentifier featureId, 
                     CellAutoPointer & cellPointer )
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
    case 2: 
    {
    FaceAutoPointer facePointer;
    if( this->GetFace(featureId,facePointer) )
      {
      TransferAutoPointer(cellPointer,facePointer);
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
TetrahedronCell< TCellInterface >
::SetPointIds(PointIdConstIterator first)
{
  PointIdConstIterator ii(first);
  for(unsigned int i=0; i < Self::NumberOfPoints ; ++i)
    {
    m_PointIds[i] = *ii++;
    }
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
TetrahedronCell< TCellInterface >
::SetPointIds(PointIdConstIterator first, PointIdConstIterator last)
{
  int localId=0;
  PointIdConstIterator ii(first);
  
  while(ii != last)
    {
    m_PointIds[localId++] = *ii++;
    }
}


/**
 * Standard CellInterface:
 * Set an individual point identifier in the cell.
 */
template <typename TCellInterface>
void
TetrahedronCell< TCellInterface >
::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TCellInterface>
typename TetrahedronCell< TCellInterface >::PointIdIterator
TetrahedronCell< TCellInterface >
::PointIdsBegin(void)
{
  return &m_PointIds[0];
}


/**
 * Standard CellInterface:
 * Get a const begin iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TCellInterface>
typename TetrahedronCell< TCellInterface >::PointIdConstIterator
TetrahedronCell< TCellInterface >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}


/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */
template <typename TCellInterface>
typename TetrahedronCell< TCellInterface >::PointIdIterator
TetrahedronCell< TCellInterface >
::PointIdsEnd(void)
{
  return &m_PointIds[Self::NumberOfPoints];
}


/**
 * Standard CellInterface:
 * Get a const end iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TCellInterface>
typename TetrahedronCell< TCellInterface >::PointIdConstIterator
TetrahedronCell< TCellInterface >
::PointIdsEnd(void) const
{
  return &m_PointIds[Self::NumberOfPoints];
}


/**
 * Tetrahedron-specific:
 * Get the number of vertices defining the tetrahedron.
 */
template <typename TCellInterface>
typename TetrahedronCell< TCellInterface >::CellFeatureCount
TetrahedronCell< TCellInterface >
::GetNumberOfVertices(void) const
{
  return Self::NumberOfVertices;
}


/**
 * Tetrahedron-specific:
 * Get the number of edges defined for the tetrahedron.
 */
template <typename TCellInterface>
typename TetrahedronCell< TCellInterface >::CellFeatureCount
TetrahedronCell< TCellInterface >
::GetNumberOfEdges(void) const
{
  return Self::NumberOfEdges;
}


/**
 * Tetrahedron-specific:
 * Get the number of faces defined for the tetrahedron.
 */
template <typename TCellInterface>
typename TetrahedronCell< TCellInterface >::CellFeatureCount
TetrahedronCell< TCellInterface >
::GetNumberOfFaces(void) const
{
  return Self::NumberOfFaces;
}


/**
 * Tetrahedron-specific:
 * Get the vertex specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfVertices()-1.
 */
template <typename TCellInterface>
bool
TetrahedronCell< TCellInterface >
::GetVertex(CellFeatureIdentifier vertexId,VertexAutoPointer & vertexPointer )
{
  VertexType * vert = new VertexType;
  vert->SetPointId(0, m_PointIds[vertexId]);
  vertexPointer.TakeOwnership( vert ); 
  return true;
}


/**
 * Tetrahedron-specific:
 * Get the edge specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfEdges()-1.
 */
template <typename TCellInterface>
bool
TetrahedronCell< TCellInterface >
::GetEdge(CellFeatureIdentifier edgeId, EdgeAutoPointer & edgePointer )
{
  EdgeType * edge = new EdgeType;
  for(int i=0; i < EdgeType::NumberOfPoints; ++i)
    {
    edge->SetPointId(i, m_PointIds[ m_Edges[edgeId][i] ]);
    }
  edgePointer.TakeOwnership( edge ); 
  return true;
}



/**
 * Tetrahedron-specific:
 * Get the face specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfFaces()-1.
 */
template <typename TCellInterface>
bool
TetrahedronCell< TCellInterface >
::GetFace(CellFeatureIdentifier faceId, FaceAutoPointer & facePointer )
{
  FaceType * face = new FaceType;
  for(unsigned int i=0; i < FaceType::NumberOfPoints; ++i)
    {
    face->SetPointId(i, m_PointIds[ m_Faces[faceId][i] ]);
    }
  facePointer.TakeOwnership( face ); 
  return true;
}


} // end namespace itk

#endif
