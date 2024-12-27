/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkTriangleCell_hxx
#define itkTriangleCell_hxx
#include "vnl/algo/vnl_determinant.h"

#include <algorithm> // For copy_n.
#include <cmath>     // For abs.

namespace itk
{

template <typename TCellInterface>
void
TriangleCell<TCellInterface>::MakeCopy(CellAutoPointer & cellPointer) const
{
  cellPointer.TakeOwnership(new Self);
  cellPointer->SetPointIds(this->GetPointIds());
}

template <typename TCellInterface>
unsigned int
TriangleCell<TCellInterface>::GetDimension() const
{
  return Self::CellDimension;
}

template <typename TCellInterface>
unsigned int
TriangleCell<TCellInterface>::GetNumberOfPoints() const
{
  return Self::NumberOfPoints;
}

template <typename TCellInterface>
auto
TriangleCell<TCellInterface>::GetNumberOfBoundaryFeatures(int dimension) const -> CellFeatureCount
{
  switch (dimension)
  {
    case 0:
      return GetNumberOfVertices();
    case 1:
      return GetNumberOfEdges();
    default:
      return 0;
  }
}

template <typename TCellInterface>
bool
TriangleCell<TCellInterface>::GetBoundaryFeature(int                   dimension,
                                                 CellFeatureIdentifier featureId,
                                                 CellAutoPointer &     cellPointer)
{
  switch (dimension)
  {
    case 0:
    {
      VertexAutoPointer vertexPointer;
      if (this->GetVertex(featureId, vertexPointer))
      {
        TransferAutoPointer(cellPointer, vertexPointer);
        return true;
      }
      break;
    }
    case 1:
    {
      EdgeAutoPointer edgePointer;
      if (this->GetEdge(featureId, edgePointer))
      {
        TransferAutoPointer(cellPointer, edgePointer);
        return true;
      }
      break;
    }
    default:
      break; // just fall through and return false
  }
  cellPointer.Reset();
  return false;
}

template <typename TCellInterface>
void
TriangleCell<TCellInterface>::SetPointIds(PointIdConstIterator first)
{
  std::copy_n(first, Self::NumberOfPoints, m_PointIds.begin());
}

template <typename TCellInterface>
void
TriangleCell<TCellInterface>::SetPointIds(PointIdConstIterator first, PointIdConstIterator last)
{
  unsigned int         localId = 0;
  PointIdConstIterator ii(first);

  while ((ii != last) && (localId < NumberOfPoints))
  {
    m_PointIds[localId++] = *ii++;
  }
}

template <typename TCellInterface>
void
TriangleCell<TCellInterface>::SetPointId(int localId, PointIdentifier ptId)
{
  m_PointIds[localId] = ptId;
}

template <typename TCellInterface>
auto
TriangleCell<TCellInterface>::PointIdsBegin() -> PointIdIterator
{
  return &m_PointIds[0];
}

template <typename TCellInterface>
auto
TriangleCell<TCellInterface>::PointIdsBegin() const -> PointIdConstIterator
{
  return &m_PointIds[0];
}

template <typename TCellInterface>
auto
TriangleCell<TCellInterface>::PointIdsEnd() -> PointIdIterator
{
  return &m_PointIds[Self::NumberOfPoints - 1] + 1;
}

template <typename TCellInterface>
auto
TriangleCell<TCellInterface>::PointIdsEnd() const -> PointIdConstIterator
{
  return &m_PointIds[Self::NumberOfPoints - 1] + 1;
}

template <typename TCellInterface>
auto
TriangleCell<TCellInterface>::GetNumberOfVertices() const -> CellFeatureCount
{
  return Self::NumberOfVertices;
}

template <typename TCellInterface>
auto
TriangleCell<TCellInterface>::GetNumberOfEdges() const -> CellFeatureCount
{
  return Self::NumberOfEdges;
}

template <typename TCellInterface>
bool
TriangleCell<TCellInterface>::GetVertex(CellFeatureIdentifier vertexId, VertexAutoPointer & vertexPointer)
{
  auto * vert = new VertexType;

  vert->SetPointId(0, m_PointIds[vertexId]);
  vertexPointer.TakeOwnership(vert);
  return true;
}

template <typename TCellInterface>
bool
TriangleCell<TCellInterface>::GetEdge(CellFeatureIdentifier edgeId, EdgeAutoPointer & edgePointer)
{
  auto * edge = new EdgeType;

  for (unsigned int i = 0; i < EdgeType::NumberOfPoints; ++i)
  {
    edge->SetPointId(i, m_PointIds[m_Edges[edgeId][i]]);
  }
  edgePointer.TakeOwnership(edge);
  return true;
}

template <typename TCellInterface>
double
TriangleCell<TCellInterface>::DistanceToLine(PointType        x,
                                             PointType        p1,
                                             PointType        p2,
                                             double &         t,
                                             CoordinateType * closestPoint)
{
  // convert from CoordinateType * to PointType:
  PointType temp(closestPoint);
  //   for (unsigned int i = 0; i < PointDimension; ++i)
  //     {
  //     temp[i] = closestPoint[i];
  //     }

  // Compute the squared distance to the line:
  const double distance2 = this->DistanceToLine(x, p1, p2, t, temp);

  // convert from PointType to CoordinateType * :
  for (unsigned int j = 0; j < PointDimension; ++j)
  {
    closestPoint[j] = temp[j];
  }

  return distance2;
}

template <typename TCellInterface>
double
TriangleCell<TCellInterface>::DistanceToLine(PointType   x,
                                             PointType   p1,
                                             PointType   p2,
                                             double &    t,
                                             PointType & closestPoint)
{
  VectorType v21 = p2 - p1;

  // Get parametric location
  double num(0);
  double denom(0);

  for (unsigned int i = 0; i < PointDimension; ++i)
  {
    num += static_cast<double>(v21[i] * (x[i] - p1[i]));
    denom += static_cast<double>(v21[i] * v21[i]);
  }

  const double tolerance = std::abs(1.e-05 * num);
  if ((-tolerance < denom) && (denom < tolerance)) // numerically bad!
  {
    closestPoint = p1; // arbitrary, point is (numerically) far away
  }

  // If parametric coordinate is within 0<=p<=1, then the point is closest to
  // the line.  Otherwise, it's closest to a point at the end of the line.
  else if ((t = num / denom) < 0.0)
  {
    closestPoint = p1;
  }
  else if (t > 1.0)
  {
    closestPoint = p2;
  }
  else
  {
    closestPoint = p1 + v21 * t;
  }

  return static_cast<double>(closestPoint.SquaredEuclideanDistanceTo(x));
}

template <typename TCellInterface>
auto
TriangleCell<TCellInterface>::ComputeArea(PointsContainer * iPoints) -> CoordinateType
{
  PointType p[3];

  for (unsigned int i = 0; i < NumberOfPoints; ++i)
  {
    p[i] = iPoints->GetElement(m_PointIds[i]);
  }

  CoordinateType a = p[1].EuclideanDistanceTo(p[2]);
  CoordinateType b = p[0].EuclideanDistanceTo(p[2]);
  CoordinateType c = p[1].EuclideanDistanceTo(p[0]);

  CoordinateType s = 0.5 * (a + b + c);
  return std::sqrt(s * (s - a) * (s - b) * (s - c));
}

template <typename TCellInterface>
auto
TriangleCell<TCellInterface>::ComputeBarycenter(CoordinateType * iWeights, PointsContainer * iPoints) -> PointType
{
  PointType      p[3];
  CoordinateType sum_weights(0.);

  for (unsigned int i = 0; i < 3; ++i)
  {
    sum_weights += iWeights[i];
    p[i] = iPoints->GetElement(m_PointIds[i]);
  }

  PointType oP{};
  if (sum_weights != 0.)
  {
    for (unsigned int i = 0; i < 3; ++i)
    {
      oP += p[i].GetVectorFromOrigin() * iWeights[i] / sum_weights;
    }
  }
  else
  {
    oP = p[0];
  }
  return oP;
}

template <typename TCellInterface>
auto
TriangleCell<TCellInterface>::ComputeCenterOfGravity(PointsContainer * iPoints) -> PointType
{
  std::vector<CoordinateType> weights(3, 1. / 3.);
  return ComputeBarycenter(&weights[0], iPoints);
}

template <typename TCellInterface>
auto
TriangleCell<TCellInterface>::ComputeCircumCenter(PointsContainer * iPoints) -> PointType
{
  std::vector<CoordinateType> weights(3, 0.);

  PointType p[3];

  for (unsigned int i = 0; i < 3; ++i)
  {
    p[i] = iPoints->GetElement(m_PointIds[i]);
  }

  CoordinateType a = p[1].SquaredEuclideanDistanceTo(p[2]);
  CoordinateType b = p[0].SquaredEuclideanDistanceTo(p[2]);
  CoordinateType c = p[1].SquaredEuclideanDistanceTo(p[0]);

  weights[0] = a * (b + c - a);
  weights[1] = b * (c + a - b);
  weights[2] = c * (a + b - c);

  CoordinateType sum_weights = weights[0] + weights[1] + weights[2];

  if (sum_weights != 0.)
  {
    PointType oP{};

    for (unsigned i = 0; i < 3; ++i)
    {
      oP += p[i].GetVectorFromOrigin() * weights[i] / sum_weights;
    }

    return oP;
  }

  return p[0];
}

template <typename TCellInterface>
bool
TriangleCell<TCellInterface>::EvaluatePosition(CoordinateType *          x,
                                               PointsContainer *         points,
                                               CoordinateType *          closestPoint,
                                               CoordinateType            pcoord[],
                                               double *                  minDist2,
                                               InterpolationWeightType * weights)
{
  double          dist2Point;
  double          dist2Line1;
  double          dist2Line2;
  PointType       closest;
  PointType       closestPoint1;
  PointType       closestPoint2;
  const PointType X(x);

  if (!points)
  {
    return false;
  }

  // Get the vertexes of this triangle
  PointType pt1 = points->GetElement(m_PointIds[0]);
  PointType pt2 = points->GetElement(m_PointIds[1]);
  PointType pt3 = points->GetElement(m_PointIds[2]);

  // Compute Vectors along the edges.
  // These two vectors form a vector base for the 2D space of the triangle cell.
  const VectorType v12 = pt1 - pt2;
  const VectorType v32 = pt3 - pt2;

  // Compute Vectors in the dual vector base inside the 2D space of the triangle
  // cell.
  // u12 is orthogonal to v32
  // u32 is orthogonal to v12
  const double dotproduct = v12 * v32;
  VectorType   u12 = v12 - v32 * (dotproduct / v32.GetSquaredNorm());
  VectorType   u32 = v32 - v12 * (dotproduct / v12.GetSquaredNorm());

  // Add normalizations for making {u12,u32} a vector basis orthonormal to {v12,
  // v32}.
  u12 /= (u12 * v12);
  u32 /= (u32 * v32);

  // Project point to plane, by using the dual vector base

  // Compute components of the input point in the 2D
  // space defined by v12 and v32
  const VectorType xo = X - pt2;

  const double u12p = xo * u12;
  const double u32p = xo * u32;

  const VectorType x12 = v12 * u12p;
  const VectorType x32 = v32 * u32p;

  // The projection of point X in the plane is cp
  PointType cp = pt2 + x12 + x32;

  // Compute barycentric coordinates in the Triangle
  const double b1 = u12p;
  const double b2 = 1.0 - u12p - u32p;
  const double b3 = u32p;

  // Test if the projected point is inside the cell.

  // Zero with epsilon
  const double zwe = -NumericTraits<double>::min();

  // Since the three barycentric coordinates are interdependent
  // only three tests should be necessary. That is, we only need
  // to test against the equations of three lines (half-spaces).
  if ((b1 >= zwe) && (b2 >= zwe) && (b3 >= zwe))
  {
    // This is the case when the point is inside the triangle
    // projection distance
    if (closestPoint)
    { // Compute the Distance 2 Between Points
      *minDist2 = 0;
      for (unsigned int i = 0; i < PointDimension; ++i)
      {
        const double val = cp[i] - x[i];
        *minDist2 += val * val;
        closestPoint[i] = cp[i];
      }
    }

    if (pcoord)
    {
      pcoord[0] = b1;
      pcoord[1] = b2;
      pcoord[2] = b3;
    }

    if (weights)
    {
      weights[0] = b1;
      weights[1] = b2;
      weights[2] = b3;
    }

    return true;
  }

  if (closestPoint)
  {
    double lt; // parameter along the line (not used)
    if (b1 < 0.0 && b2 < 0.0)
    {
      dist2Point = 0;
      for (unsigned int i = 0; i < PointDimension; ++i)
      {
        dist2Point += (x[i] - pt3[i]) * (x[i] - pt3[i]);
      }
      dist2Line1 = this->DistanceToLine(x, pt1, pt3, lt, closestPoint1);
      dist2Line2 = this->DistanceToLine(x, pt3, pt2, lt, closestPoint2);
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
      unsigned int i = 0;
      for (; i < PointDimension; ++i)
      {
        closestPoint[i] = closest[i];
      }
      for (; i < 3; ++i)
      {
        closestPoint[i] = 0.;
      }
    }
    else if (b2 < 0.0 && b3 < 0.0)
    {
      dist2Point = 0;
      for (unsigned int i = 0; i < PointDimension; ++i)
      {
        dist2Point += (x[i] - pt1[i]) * (x[i] - pt1[i]);
      }
      dist2Line1 = this->DistanceToLine(x, pt1, pt3, lt, closestPoint1);
      dist2Line2 = this->DistanceToLine(x, pt1, pt2, lt, closestPoint2);
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
      unsigned int i = 0;
      for (; i < PointDimension; ++i)
      {
        closestPoint[i] = closest[i];
      }
      for (; i < 3; ++i)
      {
        closestPoint[i] = 0.;
      }
    }
    else if (b1 < 0.0 && b3 < 0.0)
    {
      dist2Point = 0;
      for (unsigned int i = 0; i < PointDimension; ++i)
      {
        dist2Point += (x[i] - pt2[i]) * (x[i] - pt2[i]);
      }
      dist2Line1 = this->DistanceToLine(x, pt2, pt3, lt, closestPoint1);
      dist2Line2 = this->DistanceToLine(x, pt1, pt2, lt, closestPoint2);
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
      unsigned int i = 0;
      for (; i < PointDimension; ++i)
      {
        closestPoint[i] = closest[i];
      }
      for (; i < 3; ++i)
      {
        closestPoint[i] = 0.;
      }
    }
    else if (b1 < 0.0)
    {
      *minDist2 = this->DistanceToLine(x, pt2, pt3, lt, closestPoint);
    }
    else if (b2 < 0.0)
    {
      *minDist2 = this->DistanceToLine(x, pt1, pt3, lt, closestPoint);
    }
    else if (b3 < 0.0)
    {
      *minDist2 = this->DistanceToLine(x, pt1, pt2, lt, closestPoint);
    }
  }
  if (pcoord)
  {
    pcoord[0] = b1;
    pcoord[1] = b2;
    pcoord[2] = b3;
  }
  // Just fall through to default return false;

  return false; // Default case that should never be reached.
}
} // end namespace itk

#endif
