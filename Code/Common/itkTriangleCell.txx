/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTriangleCell.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkTriangleCell_txx
#define _itkTriangleCell_txx
#include "itkTriangleCell.h"
#include "vnl/algo/vnl_determinant.h"

namespace itk
{

/**
 * Standard CellInterface:
 */
template <typename TCellInterface>
void
TriangleCell< TCellInterface >
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
TriangleCell< TCellInterface >
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
TriangleCell< TCellInterface >
::GetNumberOfPoints(void) const
{
  return Self::NumberOfPoints;
}  


/**
 * Standard CellInterface:
 * Get the number of boundary features of the given dimension.
 */
template <typename TCellInterface>
typename TriangleCell< TCellInterface >::CellFeatureCount
TriangleCell< TCellInterface >
::GetNumberOfBoundaryFeatures(int dimension) const
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
template <typename TCellInterface>
bool
TriangleCell< TCellInterface >
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
TriangleCell< TCellInterface >
::SetPointIds(PointIdConstIterator first)
{
  PointIdConstIterator ii(first);
  for(unsigned int i=0; i < NumberOfPoints ; ++i, ++ii)
    {
    m_PointIds[i] = *ii;
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
TriangleCell< TCellInterface >
::SetPointIds(PointIdConstIterator first, PointIdConstIterator last)
{
  int localId=0;
  PointIdConstIterator ii(first);
  
  while( ( ii != last ) && ( localId < NumberOfPoints ) )
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
TriangleCell< TCellInterface >
::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}


/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template <typename TCellInterface>
typename TriangleCell< TCellInterface >::PointIdIterator
TriangleCell< TCellInterface >
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
typename TriangleCell< TCellInterface >::PointIdConstIterator
TriangleCell< TCellInterface >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}


/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */
template <typename TCellInterface>
typename TriangleCell< TCellInterface >::PointIdIterator
TriangleCell< TCellInterface >
::PointIdsEnd(void)
{
  return &m_PointIds[Self::NumberOfPoints-1] + 1;
}


/**
 * Standard CellInterface:
 * Get a const end iterator to the list of point identifiers used
 * by the cell.
 */
template <typename TCellInterface>
typename TriangleCell< TCellInterface >::PointIdConstIterator
TriangleCell< TCellInterface >
::PointIdsEnd(void) const
{
  return &m_PointIds[Self::NumberOfPoints-1] + 1;
}


/**
 * Triangle-specific:
 * Get the number of vertices defining the triangle.
 */
template <typename TCellInterface>
typename TriangleCell< TCellInterface >::CellFeatureCount
TriangleCell< TCellInterface >
::GetNumberOfVertices(void) const
{
  return Self::NumberOfVertices;
}


/**
 * Triangle-specific:
 * Get the number of edges defined for the triangle.
 */
template <typename TCellInterface>
typename TriangleCell< TCellInterface >::CellFeatureCount
TriangleCell< TCellInterface >
::GetNumberOfEdges(void) const
{
  return Self::NumberOfEdges;
}

/**
 * Triangle-specific:
 * Get the vertex specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfVertices()-1.
 */
template <typename TCellInterface>
bool
TriangleCell< TCellInterface >
::GetVertex(CellFeatureIdentifier vertexId,VertexAutoPointer & vertexPointer )
{
  VertexType * vert = new VertexType;
  vert->SetPointId(0, m_PointIds[vertexId]);
  vertexPointer.TakeOwnership( vert );
  return true;  
}

/**
 * Triangle-specific:
 * Get the edge specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfEdges()-1.
 */
template <typename TCellInterface>
bool
TriangleCell< TCellInterface >
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



/** Compute distance to finite line. Returns parametric coordinate t 
 *  and point location on line. */
template <typename TCellInterface>
double
TriangleCell< TCellInterface >
::DistanceToLine(PointType x, PointType p1, PointType p2, 
                              double &t, CoordRepType *closestPoint)
{
  // convert from CoordRepType * to PointType:
  PointType temp( closestPoint );
//   for (unsigned int i = 0; i < PointDimension; i++)
//     {
//     temp[i] = closestPoint[i];
//     }

  // Compute the squared distance to the line:
  const double distance2 = this->DistanceToLine (x, p1, p2, t, temp);

  // convert from PointType to CoordRepType * :
  for (unsigned int j = 0; j < PointDimension; j++)
    {
    closestPoint[j] = temp[j];
    } 

  return distance2;
}

template <typename TCellInterface>
double
TriangleCell< TCellInterface >
::DistanceToLine(PointType x, PointType p1, PointType p2, 
                              double &t, PointType &closestPoint)
{
  VectorType v21 = p2 - p1;
  //
  //   Get parametric location
  //
  double num( 0 );
  double denom( 0 );

  for(unsigned int i=0;i<PointDimension;i++)
    {
    num += static_cast<double>( v21[i]*(x[i]-p1[i]) );
    denom += static_cast<double>(v21[i]*v21[i]);
    }

  // trying to avoid an expensive fabs
  double tolerance = 1.e-05*num;
  if (tolerance < 0.0)
    {
    tolerance = -tolerance;
    }
  if ( ( -tolerance < denom ) && ( denom < tolerance )) //numerically bad!
    {
    closestPoint = p1; //arbitrary, point is (numerically) far away
    }
  //
  // If parametric coordinate is within 0<=p<=1, then the point is closest to
  // the line.  Otherwise, it's closest to a point at the end of the line.
  //
  else if ( (t=num/denom) < 0.0 )
    {
    closestPoint = p1;
    }
  else if ( t > 1.0 )
    {
    closestPoint = p2;
    }
  else
    {
    closestPoint = p1 + v21 * t;
//     for(i=0;i<PointDimension;i++)
//       {
//       closest[i] = p1[i] + t*p21[i];
//       }
    }
    
//   for(i=0;i<PointDimension;i++)
//     {
//     closestPoint[i] = closest[i]; 
//     }

  return static_cast< double >( closestPoint.SquaredEuclideanDistanceTo(x) );
}

template<typename TCellInterface>
typename TriangleCell< TCellInterface >::VectorType
TriangleCell< TCellInterface >::ComputeNormal( const PointType& iP1,
  const PointType& iP2, const PointType& iP3 )
{
  VectorType oN;
  
  if(PointDimension != 3)
    {
    itkWarningMacro("TriangleCell::ComputeNormal() only works with 3D points");
    std::cout << "TriangleCell::ComputeNormal() only works with 3D points"
      <<std::endl;
    oN.Fill( 0. );
    return oN;
    }

  VectorType v23 = iP3 - iP2;
  VectorType v21 = iP1 - iP2;

  oN = CrossProduct( v23, v21 );
  oN.Normalize();

  return oN;
}

template<typename TCellInterface>
typename TriangleCell< TCellInterface >::VectorType
TriangleCell< TCellInterface >::ComputeNormal( PointsContainer* iPoints )
{
  PointType p0 = iPoints->GetElement( m_PointIds[0] );
  PointType p1 = iPoints->GetElement( m_PointIds[1] );
  PointType p2 = iPoints->GetElement( m_PointIds[2] );

  return ComputeNormal( p0, p1, p2 );
}

template<typename TCellInterface>
typename TriangleCell< TCellInterface >::CoordRepType
TriangleCell< TCellInterface >::ComputeArea( PointsContainer* iPoints )
{
  PointType p[3];

  for( unsigned int i = 0; i < NumberOfPoints; ++i )
    p[i] = iPoints->GetElement( m_PointIds[i] );

  CoordRepType a = p[1].EuclideanDistanceTo( p[2] );
  CoordRepType b = p[0].EuclideanDistanceTo( p[2] );
  CoordRepType c = p[1].EuclideanDistanceTo( p[0] );

  CoordRepType s = 0.5 * ( a + b + c );
  return vcl_sqrt( s * ( s - a ) * ( s - b ) * ( s - c ) );
}

template <typename TCellInterface>
typename TriangleCell< TCellInterface >::PointType
TriangleCell< TCellInterface >::ComputeBarycenter(
  CoordRepType* iWeights, PointsContainer* iPoints )
{
  PointType p[3];
  CoordRepType sum_weights(0.);
  unsigned int i(0);

  for( ; i < 3; i++ )
    {
    sum_weights += iWeights[i];
    p[i] = iPoints->GetElement( m_PointIds[i] );
    }

  PointType oP;

  if( sum_weights != 0. )
    {
    oP.Fill( 0. );
    for( i = 0; i < 3; i++ )
      oP += p[i].GetVectorFromOrigin() * iWeights[i] / sum_weights;
    }
  else
    {
    oP = p[0];
    }
  return oP;
}

template <typename TCellInterface>
typename TriangleCell< TCellInterface >::PointType
TriangleCell< TCellInterface >::ComputeCenterOfGravity(
  PointsContainer* iPoints )
{
  std::vector< CoordRepType > weights( 3, 1./3. );
  return ComputeBarycenter( weights.begin(), iPoints );
}

template <typename TCellInterface>
typename TriangleCell< TCellInterface >::PointType
TriangleCell< TCellInterface >::ComputeCircumCenter(
  PointsContainer* iPoints )
{
  std::vector< CoordRepType > weights( 3, 0. );

  PointType p[3];
  unsigned int i(0);

  for( unsigned int i = 0; i < 3; i++ )
    {
    p[i] = iPoints->GetElement( m_PointIds[i] );
    }

  CoordRepType a = p[1].SquaredEuclideanDistanceTo( p[2] );
  CoordRepType b = p[0].SquaredEuclideanDistanceTo( p[2] );
  CoordRepType c = p[1].SquaredEuclideanDistanceTo( p[0] );

  weights[0] = a * ( b + c - a );
  weights[1] = b * ( c + a - b );
  weights[2] = c * ( a + b - c );

  CoordRepType sum_weights = weights[0] + weights[1] + weights[2];
  
  if( sum_weights != 0. )
    {
    PointType oP;
    oP.Fill( 0. );

    for( unsigned int i = 0; i < 3; i++ )
      oP += p[i].GetVectorFromOrigin() * weights[i] / sum_weights;

    return oP;
    }
  else
    return p[0];
}


/** Evaluate the position of a given point inside the cell 
 *  This only works in 3D since cross product is not defined
 *  for higher dimensions */
template <typename TCellInterface>
bool
TriangleCell< TCellInterface >
::EvaluatePosition(CoordRepType* x,
                   PointsContainer* points,
                   CoordRepType* closestPoint,
                   CoordRepType pcoord[3],
                   double* minDist2,
                   InterpolationWeightType* weights)
{
 
  if(PointDimension != 3)
    {
    itkWarningMacro(
      "TriangleCell::EvaluatePosition() only works with 3D points");
    std::cout << "TriangleCell::EvaluatePosition() only works with 3D points"
      << std::endl;
    return false;
    }


  unsigned int i, j;
  double fabsn;
  double rhs[2], c1[2], c2[2];
  double det;
  double maxComponent;
  unsigned int idx=0, indices[2];
  double dist2Point, dist2Line1, dist2Line2;
  PointType closest; 
  PointType closestPoint1, closestPoint2, cp;
  CoordRepType pcoords[3];
  PointType X( x );

  if(!points)
    {
    return false;
    }
  
  // Get normal for triangle, only the normal direction is needed, i.e. the
  // normal need not be normalized (unit length)
  //
  PointType pt1 = points->GetElement(m_PointIds[0]);
  PointType pt2 = points->GetElement(m_PointIds[1]);
  PointType pt3 = points->GetElement(m_PointIds[2]);


  // This is the solution for 3D points
//   double ax, ay, az, bx, by, bz;

  // order is important!!! maintain consistency with triangle vertex order
  VectorType n = ComputeNormal( pt1, pt2, pt3 );

/*  ax = pt3[0] - pt2[0]; ay = pt3[1] - pt2[1]; az = pt3[2] - pt2[2];
  bx = pt1[0] - pt2[0]; by = pt1[1] - pt2[1]; bz = pt1[2] - pt2[2];

  n[0] = (ay * bz - az * by);
  n[1] = (az * bx - ax * bz);
  n[2] = (ax * by - ay * bx);*/
 
  // Project point to plane
//   double t( 0. ), n2( 0. );
  VectorType xo = X - pt1;
  double t = static_cast< double >( xo * n );
//   for(i=0;i<PointDimension;i++)
//     {
//     xo[i] = x[i] - pt1[i];
//     }
// 
//   for(i=0;i<PointDimension;i++)
//     {
//     t += static_cast< double >( n[i]*xo[i] );
//     n2 += static_cast< double >( n[i]*n[i] );
//     }
// 
  if(( n[0] != 0.) && ( n[1] != 0. ) && ( n[2] != 0. ) )
    {
    cp = X - n * t;
//     for(i=0;i<PointDimension;i++)
//       {
//       cp[i] = x[i] - t * n[i]/n2;
//       }
    }
  else
    {
    cp = X;
//     for(i=0;i<PointDimension;i++)
//       {
//       cp[i] = x[i];
//       }
    }  

  // Construct matrices.  Since we have over determined system, need to find
  // which 2 out of 3 equations to use to develop equations. (Any 2 should 
  // work since we've projected point to plane.)
  //
  for (maxComponent=0.0, i=0; i<3; i++)
    {
    // trying to avoid an expensive call to vcl_fabs()
    fabsn = ( n[i] < 0. ) ? -n[i] : n[i];
    if (fabsn > maxComponent)
      {
      maxComponent = fabsn;
      idx = i;
      }
    }

  for (j=0, i=0; i<3; i++)  
    {
    if ( i != idx )
      {
      indices[j++] = i;
      }
    }
  
  for (i=0; i<2; i++)
    {
    rhs[i] = static_cast< double >( cp[indices[i]] - pt3[indices[i]] );
    c1[i] = static_cast< double >( pt1[indices[i]] - pt3[indices[i]] );
    c2[i] = static_cast< double >( pt2[indices[i]] - pt3[indices[i]] );
    }

  
  if ( (det = c1[0]*c2[1] - c2[0]*c1[1]) == 0.0 )
    {
    pcoords[0] = pcoords[1] = pcoords[2] = 0.0;
    if(pcoord)
      {
      pcoord[0] = pcoords[0]; 
      pcoord[1] = pcoords[1];
      pcoord[2] = pcoords[2];
      }
    return false;
    }

  const double _t1 = rhs[0]*c2[1] - c2[0]*rhs[1];
  const double _t2 = c1[0]*rhs[1] - rhs[0]*c1[1];
  pcoords[0] = _t1 / det;
  pcoords[1] = _t2 / det;
  pcoords[2] = (det - (_t1 + _t2))/det;

  // Okay, now find closest point to element
  //
  if(weights)
    {
    weights[0] = pcoords[2];
    weights[1] = pcoords[0];
    weights[2] = pcoords[1];
    }

  // Zero with epsilon
  const double zwe = -NumericTraits<double>::min();
  // One with epsilon
  const double owe = 1.0 + NumericTraits<double>::min();

  if ( pcoords[0] >= zwe  && pcoords[0] <= owe &&
       pcoords[1] >= zwe  && pcoords[1] <= owe &&
       pcoords[2] >= zwe  && pcoords[2] <= owe )
    {
    //projection distance
    if (closestPoint)
      { // Compute the Distance 2 Between Points
      *minDist2 = 0;
      for(i=0;i<PointDimension;i++)
        {
        const double val = cp[i] - x[i];
        *minDist2 += val * val;
        closestPoint[i] = cp[i];
        }
      }

    if(pcoord)
      {
      pcoord[0] = pcoords[0]; 
      pcoord[1] = pcoords[1];
      pcoord[2] = pcoords[2];
      }
    return true;
    }
  else
    {
    if (closestPoint)
      {
      if ( pcoords[0] < 0.0 && pcoords[1] < 0.0 )
        {
        dist2Point = 0;
        for(i=0;i<PointDimension;i++)
          {
          const double value = x[i] - pt3[i];
          dist2Point += value * value;
          }
        dist2Line1 = this->DistanceToLine(x,pt1,pt3,t,closestPoint1);
        dist2Line2 = this->DistanceToLine(x,pt3,pt2,t,closestPoint2);
        if (dist2Point < dist2Line1)
          {
          *minDist2 = dist2Point;
          closest = pt3;
          }
        else
          {
          *minDist2 = dist2Line1;
          closest = closestPoint1;
          }
        if (dist2Line2 < *minDist2)
          {
          *minDist2 = dist2Line2;
          closest = closestPoint2;
          }
        for (i=0; i<3; i++)
          {
          closestPoint[i] = closest[i];
          }
        }
      else if ( pcoords[1] < 0.0 && pcoords[2] < 0.0 )
        {
        dist2Point = 0;
        for(i=0;i<PointDimension;i++)
          {
          dist2Point += x[i]-pt1[i]*x[i]-pt1[i];
          }
        dist2Line1 = this->DistanceToLine(x,pt1,pt3,t,closestPoint1);
        dist2Line2 = this->DistanceToLine(x,pt1,pt2,t,closestPoint2);
        if (dist2Point < dist2Line1)
          {
          *minDist2 = dist2Point;
          closest = pt1;
          }
        else
          {
          *minDist2 = dist2Line1;
          closest = closestPoint1;
          }
        if (dist2Line2 < *minDist2)
          {
          *minDist2 = dist2Line2;
          closest = closestPoint2;
          }
        for (i=0; i<3; i++)
          {
          closestPoint[i] = closest[i];
          }
        }
      else if ( pcoords[0] < 0.0 && pcoords[2] < 0.0 )
        {
        dist2Point = 0;
        for(i=0;i<PointDimension;i++)
          {
          dist2Point += (x[i]-pt2[i])*(x[i]-pt2[i]);
          }
        dist2Line1 = this->DistanceToLine(x,pt2,pt3,t,closestPoint1);
        dist2Line2 = this->DistanceToLine(x,pt1,pt2,t,closestPoint2);
        if (dist2Point < dist2Line1)
          {
          *minDist2 = dist2Point;
          closest = pt2;
          }
        else
          {
          *minDist2 = dist2Line1;
          closest = closestPoint1;
          }
        if (dist2Line2 < *minDist2)
          {
          *minDist2 = dist2Line2;
          closest = closestPoint2;
          }
        for (i=0; i<3; i++)
          {
          closestPoint[i] = closest[i];
          }
        }
      else if ( pcoords[0] < 0.0 )
        {
        *minDist2 = this->DistanceToLine(x,pt2,pt3,t,closestPoint);
        }
      else if ( pcoords[1] < 0.0 )
        {
        *minDist2 = this->DistanceToLine(x,pt1,pt3,t,closestPoint);
        }
      else if ( pcoords[2] < 0.0 )
        {
        *minDist2 = this->DistanceToLine(x,pt1,pt2,t,closestPoint);
        }
      }
    if(pcoord)
      {
      pcoord[0] = pcoords[0]; 
      pcoord[1] = pcoords[1];
      pcoord[2] = pcoords[2];
      }
    //Just fall through to default return false;
    }
    return false; //Default case that should never be reached.
}


} // end namespace itk

#endif
