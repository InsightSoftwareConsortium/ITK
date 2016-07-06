/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkQuadrilateralCell_hxx
#define itkQuadrilateralCell_hxx
#include "itkQuadrilateralCell.h"
#include "vnl/algo/vnl_determinant.h"

namespace itk
{

/**
 * Standard CellInterface:
 */
template< typename TCellInterface >
void
QuadrilateralCell< TCellInterface >
::MakeCopy(CellAutoPointer & cellPointer) const
{
  cellPointer.TakeOwnership(new Self);
  cellPointer->SetPointIds( this->GetPointIds() );
}

/**
 * Standard CellInterface:
 * Get the topological dimension of this cell.
 */
template< typename TCellInterface >
unsigned int
QuadrilateralCell< TCellInterface >
::GetDimension(void) const
{
  return Self::CellDimension;
}

/**
 * Standard CellInterface:
 * Get the number of points required to define the cell.
 */
template< typename TCellInterface >
unsigned int
QuadrilateralCell< TCellInterface >
::GetNumberOfPoints(void) const
{
  return Self::NumberOfPoints;
}

/**
 * Standard CellInterface:
 * Get the number of boundary features of the given dimension.
 */
template< typename TCellInterface >
typename QuadrilateralCell< TCellInterface >::CellFeatureCount
QuadrilateralCell< TCellInterface >
::GetNumberOfBoundaryFeatures(int dimension) const
{
  switch ( dimension )
    {
    case 0:
      return GetNumberOfVertices();
    case 1:
      return GetNumberOfEdges();
    default:
      return 0;
    }
}

/**
 * Standard CellInterface:
 * Get the boundary feature of the given dimension specified by the given
 * cell feature Id.
 * The Id can range from 0 to GetNumberOfBoundaryFeatures(dimension)-1.
 */
template< typename TCellInterface >
bool
QuadrilateralCell< TCellInterface >
::GetBoundaryFeature(int dimension, CellFeatureIdentifier featureId, CellAutoPointer & cellPointer)
{
  switch ( dimension )
    {
    case 0:
      {
      VertexAutoPointer vertexPointer;
      if ( this->GetVertex(featureId, vertexPointer) )
        {
        TransferAutoPointer(cellPointer, vertexPointer);
        return true;
        }
      break;
      }
    case 1:
      {
      EdgeAutoPointer edgePointer;
      if ( this->GetEdge(featureId, edgePointer) )
        {
        TransferAutoPointer(cellPointer, edgePointer);
        return true;
        }
      break;
      }
    default:
      break; //just fall through and return false;
    }
  cellPointer.Reset();
  return false;
}

/**
 * Standard CellInterface:
 * Set the point id list used by the cell.  It is assumed that the given
 * iterator can be incremented and safely de-referenced enough times to
 * get all the point ids needed by the cell.
 */
template< typename TCellInterface >
void
QuadrilateralCell< TCellInterface >
::SetPointIds(PointIdConstIterator first)
{
  PointIdConstIterator ii(first);

  for ( unsigned int i = 0; i < Self::NumberOfPoints; ++i )
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
template< typename TCellInterface >
void
QuadrilateralCell< TCellInterface >
::SetPointIds(PointIdConstIterator first, PointIdConstIterator last)
{
  int                  localId = 0;
  PointIdConstIterator ii(first);

  while ( ii != last )
    {
    m_PointIds[localId++] = *ii++;
    }
}

/**
 * Standard CellInterface:
 * Set an individual point identifier in the cell.
 */
template< typename TCellInterface >
void
QuadrilateralCell< TCellInterface >
::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}

/**
 * Standard CellInterface:
 * Get a begin iterator to the list of point identifiers used by the cell.
 */
template< typename TCellInterface >
typename QuadrilateralCell< TCellInterface >::PointIdIterator
QuadrilateralCell< TCellInterface >
::PointIdsBegin(void)
{
  return &m_PointIds[0];
}

/**
 * Standard CellInterface:
 * Get a const begin iterator to the list of point identifiers used
 * by the cell.
 */
template< typename TCellInterface >
typename QuadrilateralCell< TCellInterface >::PointIdConstIterator
QuadrilateralCell< TCellInterface >
::PointIdsBegin(void) const
{
  return &m_PointIds[0];
}

/**
 * Standard CellInterface:
 * Get an end iterator to the list of point identifiers used by the cell.
 */
template< typename TCellInterface >
typename QuadrilateralCell< TCellInterface >::PointIdIterator
QuadrilateralCell< TCellInterface >
::PointIdsEnd(void)
{
  return &m_PointIds[Self::NumberOfPoints - 1] + 1;
}

/**
 * Standard CellInterface:
 * Get a const end iterator to the list of point identifiers used
 * by the cell.
 */
template< typename TCellInterface >
typename QuadrilateralCell< TCellInterface >::PointIdConstIterator
QuadrilateralCell< TCellInterface >
::PointIdsEnd(void) const
{
  return &m_PointIds[Self::NumberOfPoints - 1] + 1;
}

/**
 * Quadrilateral-specific:
 * Get the number of vertices defining the quadrilateral.
 */
template< typename TCellInterface >
typename QuadrilateralCell< TCellInterface >::CellFeatureCount
QuadrilateralCell< TCellInterface >
::GetNumberOfVertices(void) const
{
  return NumberOfVertices;
}

/**
 * Quadrilateral-specific:
 * Get the number of edges defined for the quadrilateral.
 */
template< typename TCellInterface >
typename QuadrilateralCell< TCellInterface >::CellFeatureCount
QuadrilateralCell< TCellInterface >
::GetNumberOfEdges(void) const
{
  return Self::NumberOfEdges;
}

/**
 * Quadrilateral-specific:
 * Get the vertex specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfVertices()-1.
 */
template< typename TCellInterface >
bool
QuadrilateralCell< TCellInterface >
::GetVertex(CellFeatureIdentifier vertexId, VertexAutoPointer & vertexPointer)
{
  VertexType *vert = new VertexType;

  vert->SetPointId(0, m_PointIds[vertexId]);
  vertexPointer.TakeOwnership(vert);
  return true;
}

/**
 * Quadrilateral-specific:
 * Get the edge specified by the given cell feature Id.
 * The Id can range from 0 to GetNumberOfEdges()-1.
 */
template< typename TCellInterface >
bool
QuadrilateralCell< TCellInterface >
::GetEdge(CellFeatureIdentifier edgeId, EdgeAutoPointer & edgePointer)
{
  EdgeType *edge = new EdgeType;

  for ( unsigned int i = 0; i < EdgeType::NumberOfPoints; ++i )
    {
    edge->SetPointId(i, m_PointIds[m_Edges[edgeId][i]]);
    }
  edgePointer.TakeOwnership(edge);
  return true;
}

/** Evaluate the position inside the cell */
template< typename TCellInterface >
bool
QuadrilateralCell< TCellInterface >
::EvaluatePosition(CoordRepType *x,
                   PointsContainer *points,
                   CoordRepType *closestPoint,
                   CoordRepType pcoord[CellDimension],
                   double *dist2,
                   InterpolationWeightType *weight)
{
  static ITK_CONSTEXPR_VAR int    ITK_QUAD_MAX_ITERATION = 10;
  static ITK_CONSTEXPR_VAR double ITK_QUAD_CONVERGED = 1.e-03;
  static ITK_CONSTEXPR_VAR double ITK_DIVERGED = 1.e6;

  int                     iteration, converged;
  double                  params[CellDimension];
  double                  fcol[CellDimension];
  double                  rcol[CellDimension];
  double                  scol[CellDimension];
  double                  d;
  PointType               pt;
  CoordRepType            derivs[NumberOfDerivatives];
  InterpolationWeightType weights[NumberOfPoints];

  //  set initial position for Newton's method
  int          subId = 0;
  CoordRepType pcoords[CellDimension];

  pcoords[0] = pcoords[1] = params[0] = params[1] = 0.5;

  // NOTE: Point x is here assumed to lie on the plane of Quad.  Otherwise, (FIXME)
  //   - Get normal for quadrilateral, using its 3 corners
  //   - Project point x onto Quad plane using this normal
  // See vtkQuad for this:  ComputeNormal (this, pt1, pt2, pt3, n);  vtkPlane::ProjectPoint(x,pt1,n,cp);

  //  enter iteration loop
  for ( iteration = converged = 0;
        !converged && ( iteration < ITK_QUAD_MAX_ITERATION ); iteration++ )
    {
    //  calculate element interpolation functions and derivatives
    this->InterpolationFunctions(pcoords, weights);
    this->InterpolationDerivs(pcoords, derivs);

    //  calculate newton functions
    for ( unsigned int i = 0; i < CellDimension; ++i )
      {
      fcol[i] = rcol[i] = scol[i] = 0.0;
      }
    for ( unsigned int i = 0; i < NumberOfPoints; ++i )
      {
      pt = points->GetElement(m_PointIds[i]);
      // using the projection normal n, one can choose which 2 axes to use out of 3
      // any 2 should work, so (not having n) we use [x,y] (also assuming 2D use of QuadCell)
      // if we compute n, one can use the closest two indices as in vtkQuad
      for ( unsigned int j = 0; j < CellDimension; ++j )
        {
        fcol[j] += pt[j] * weights[i];
        rcol[j] += pt[j] * derivs[i];
        scol[j] += pt[j] * derivs[i + NumberOfPoints];
        }
      }

    for ( unsigned int i = 0; i < CellDimension; ++i )
      {
      fcol[i] -= x[i];
      }

    //  compute determinants and generate improvements
    vnl_matrix_fixed< CoordRepType, CellDimension, CellDimension > mat;
    for ( unsigned int i = 0; i < CellDimension; ++i )
      {
      mat.put(0, i, rcol[i]);
      mat.put(1, i, scol[i]);
      }

    d = vnl_determinant(mat);
    //d=vtkMath::Determinant2x2(rcol,scol);
    if ( std::abs(d) < 1.e-20 )
      {
      return false;
      }

    vnl_matrix_fixed< CoordRepType, CellDimension, CellDimension > mat1;
    for ( unsigned int i = 0; i < CellDimension; ++i )
      {
      mat1.put(0, i, fcol[i]);
      mat1.put(1, i, scol[i]);
      }

    vnl_matrix_fixed< CoordRepType, CellDimension, CellDimension > mat2;
    for ( unsigned int i = 0; i < CellDimension; ++i )
      {
      mat2.put(0, i, rcol[i]);
      mat2.put(1, i, fcol[i]);
      }

    pcoords[0] = params[0] - vnl_determinant(mat1) / d;
    pcoords[1] = params[1] - vnl_determinant(mat2) / d;

    if ( pcoord )
      {
      pcoord[0] = pcoords[0];
      pcoord[1] = pcoords[1];
      }

    //  check for convergence
    if ( ( ( std::abs(pcoords[0] - params[0]) ) < ITK_QUAD_CONVERGED )
         && ( ( std::abs(pcoords[1] - params[1]) ) < ITK_QUAD_CONVERGED ) )
      {
      converged = 1;
      }

    // Test for bad divergence (S.Hirschberg 11.12.2001)
    else if ( ( std::abs(pcoords[0]) > ITK_DIVERGED )
              || ( std::abs(pcoords[1]) > ITK_DIVERGED ) )
      {
      return -1;
      }

    //  if not converged, repeat
    else
      {
      params[0] = pcoords[0];
      params[1] = pcoords[1];
      }
    }

  //  if not converged, set the parametric coordinates to arbitrary values
  //  outside of element
  if ( !converged )
    {
    return false;
    }

  this->InterpolationFunctions(pcoords, weights);

  if ( weight )
    {
    for ( unsigned int i = 0; i < NumberOfPoints; ++i )
      {
      weight[i] = weights[i];
      }
    }

  if ( pcoords[0] >= -0.001 && pcoords[0] <= 1.001
       && pcoords[1] >= -0.001 && pcoords[1] <= 1.001 )
    {
    if ( closestPoint )
      {
      closestPoint[0] = x[0];
      closestPoint[1] = x[1];
      *dist2 = 0.0; //inside quadrilateral
      }
    return true;
    }
  else
    {
    CoordRepType pc[CellDimension], w[NumberOfPoints];
    if ( closestPoint )
      {
      for ( unsigned int i = 0; i < CellDimension; ++i ) //only approximate ??
        {
        if ( pcoords[i] < 0.0 )
          {
          pc[i] = 0.0;
          }
        else if ( pcoords[i] > 1.0 )
          {
          pc[i] = 1.0;
          }
        else
          {
          pc[i] = pcoords[i];
          }
        }
      this->EvaluateLocation(subId, points, pc, closestPoint, (InterpolationWeightType *)w);

      *dist2 = 0;
      for ( unsigned int i = 0; i < CellDimension; ++i )
        {
        *dist2 += ( closestPoint[i] - x[i] ) * ( closestPoint[i] - x[i] );
        }
      }
    return false;
    }
}

/** Compute iso-parametric interpolation functions */
template< typename TCellInterface >
void
QuadrilateralCell< TCellInterface >
::InterpolationFunctions(const CoordRepType pointCoords[CellDimension], InterpolationWeightType weights[NumberOfPoints])
{
  const double rm = 1. - pointCoords[0];
  const double sm = 1. - pointCoords[1];

  weights[0] = rm * sm;
  weights[1] = pointCoords[0] * sm;
  weights[2] = pointCoords[0] * pointCoords[1];
  weights[3] = rm * pointCoords[1];
}

/** Compute iso-parametric interpolation functions */
template< typename TCellInterface >
void
QuadrilateralCell< TCellInterface >
::InterpolationDerivs(const CoordRepType pointCoords[CellDimension], CoordRepType derivs[NumberOfDerivatives])
{
  const double rm = 1. - pointCoords[0];
  const double sm = 1. - pointCoords[1];

  // r-derivatives
  derivs[0] = -sm;
  derivs[1] = sm;
  derivs[2] = pointCoords[1];
  derivs[3] = -pointCoords[1];
  // s-derivatives
  derivs[4] = -rm;
  derivs[5] = -pointCoords[0];
  derivs[6] = pointCoords[0];
  derivs[7] = rm;
}

/** Evaluate the location inside the cell */
template< typename TCellInterface >
void
QuadrilateralCell< TCellInterface >
::EvaluateLocation(int & itkNotUsed(subId), const PointsContainer *points, const CoordRepType pointCoords[PointDimension],
                   CoordRepType x[PointDimension], InterpolationWeightType *weights)
{
  this->InterpolationFunctions(pointCoords, weights);

  for ( unsigned int ii = 0; ii < PointDimension; ++ii )
    {
    x[ii] = NumericTraits< CoordRepType >::ZeroValue();
    }

  for ( unsigned int ii = 0; ii < NumberOfPoints; ++ii )
    {
    const PointType & point = points->GetElement(m_PointIds[ii]);

    for ( unsigned int jj = 0; jj < PointDimension; ++jj )
      {
      x[jj] += point[jj] * weights[ii];
      }
    }
}

} // end namespace itk

#endif
