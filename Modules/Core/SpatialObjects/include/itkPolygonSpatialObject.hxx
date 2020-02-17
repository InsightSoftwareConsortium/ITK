/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkPolygonSpatialObject_hxx
#define itkPolygonSpatialObject_hxx

#include "itkPolygonSpatialObject.h"
#include "itkMath.h"

namespace itk
{

template <unsigned int TDimension>
PolygonSpatialObject<TDimension>::PolygonSpatialObject()
{
  this->SetTypeName("PolygonSpatialObject");

  this->Clear();

  this->Update();
}

template <unsigned int TDimension>
void
PolygonSpatialObject<TDimension>::Clear()
{
  Superclass::Clear();

  m_IsClosed = true;
  m_OrientationInObjectSpace = -1;
  m_OrientationInObjectSpaceMTime = this->GetMyMTime();
  m_ThicknessInObjectSpace = 0.0;

  this->Modified();
}

template <unsigned int TDimension>
int
PolygonSpatialObject<TDimension>::GetOrientationInObjectSpace() const
{
  if (m_OrientationInObjectSpaceMTime == this->GetMyMTime())
  {
    return m_OrientationInObjectSpace;
  }
  m_OrientationInObjectSpaceMTime = this->GetMyMTime();

  const PolygonPointListType & points = this->GetPoints();
  auto                         it = points.begin();
  auto                         itend = points.end();
  PointType                    minPnt;
  PointType                    maxPnt;
  minPnt.Fill(NumericTraits<double>::max());
  maxPnt.Fill(NumericTraits<double>::NonpositiveMin());
  while (it != itend)
  {
    PointType curpoint = it->GetPositionInObjectSpace();
    for (unsigned int i = 0; i < TDimension; i++)
    {
      if (minPnt[i] > curpoint[i])
      {
        minPnt[i] = curpoint[i];
      }
      if (maxPnt[i] < curpoint[i])
      {
        maxPnt[i] = curpoint[i];
      }
    }
    it++;
  }
  m_OrientationInObjectSpace = -1;
  for (unsigned int i = 0; i < TDimension; i++)
  {
    if (Math::ExactlyEquals(minPnt[i], maxPnt[i]))
    {
      m_OrientationInObjectSpace = i;
      break;
    }
  }
  return m_OrientationInObjectSpace;
}

template <unsigned int TDimension>
double
PolygonSpatialObject<TDimension>::MeasureAreaInObjectSpace() const
{
  // To find the area of a planar polygon not in the x-y plane, use:
  // 2 A(P) = std::abs(N . (sum_{i=0}^{n-1} (v_i x v_{i+1})))
  // where N is a unit vector normal to the plane. The `.' represents the
  // dot product operator, the `x' represents the cross product operator,
  //        and std::abs() is the absolute value function.
  double area = 0.0;
  int    numpoints = this->GetNumberOfPoints();
  int    X = 0;
  int    Y = 1;

  if (numpoints < 3)
  {
    return 0;
  }

  if (this->GetOrientationInObjectSpace() == 0 && ObjectDimension > 2)
  {
    X = 1;
    Y = 2;
  }
  else if (this->GetOrientationInObjectSpace() == 1 && ObjectDimension > 2)
  {
    Y = 2;
  }

  const PolygonPointListType & points = this->GetPoints();
  auto                         it = points.begin();
  auto                         itend = points.end();
  PointType                    a = it->GetPositionInObjectSpace();
  PointType                    b;
  ++it;
  while (it != itend)
  {
    b = it->GetPositionInObjectSpace();
    if (a == b)
    {
      continue;
    }
    area += a[X] * b[Y] - a[Y] * b[X];
    a = b;
    it++;
  }
  if (m_IsClosed)
  {
    a = points.begin()->GetPositionInObjectSpace();
    b = points.back().GetPositionInObjectSpace();
    // closed PolygonGroup may have the first and last points the same
    if (a != b)
    {
      area += a[X] * b[Y] - a[Y] * b[X];
    }
  }
  area *= 0.5;
  return area < 0.0 ? -area : area;
}

template <unsigned int TDimension>
double
PolygonSpatialObject<TDimension>::MeasureVolumeInObjectSpace() const
{
  return m_ThicknessInObjectSpace * this->MeasureAreaInObjectSpace();
}

template <unsigned int TDimension>
double
PolygonSpatialObject<TDimension>::MeasurePerimeterInObjectSpace() const
{
  double perimeter = 0.0;
  int    numpoints = this->GetNumberOfPoints();

  if (numpoints < 3)
  {
    return 0;
  }
  const PolygonPointListType & points = this->GetPoints();

  auto it = points.begin();
  auto itend = points.end();

  PointType a = it->GetPositionInObjectSpace();
  PointType b;
  ++it;
  while (it != itend)
  {
    b = it->GetPositionInObjectSpace();
    if (a == b)
    {
      continue;
    }
    double curdistance = a.EuclideanDistanceTo(b);
    perimeter += curdistance;
    a = b;
    it++;
  }
  if (m_IsClosed)
  {
    a = points.begin()->GetPositionInObjectSpace();
    b = points.back().GetPositionInObjectSpace();
    // closed PolygonGroup may have the first and last points the same
    if (a != b)
    {
      double curdistance = a.EuclideanDistanceTo(b);
      perimeter += curdistance;
    }
  }
  return perimeter;
}

template <unsigned int TDimension>
bool
PolygonSpatialObject<TDimension>::IsInsideInObjectSpace(const PointType & point) const
{
  if (this->GetIsClosed() && this->GetMyBoundingBoxInObjectSpace()->IsInside(point))
  {
    int numpoints = this->GetNumberOfPoints();
    int X = -1;
    int Y = -1;

    if (numpoints >= 3)
    {
      for (unsigned int i = 0; i < TDimension; ++i)
      {
        if (this->GetOrientationInObjectSpace() != static_cast<int>(i))
        {
          if (X == -1)
          {
            X = i;
          }
          else
          {
            Y = i;
            break;
          }
        }
      }

      const PolygonPointListType & points = this->GetPoints();
      auto                         it = points.begin();
      auto                         itend = points.end();

      bool oddNodes = false;

      PointType node1 = it->GetPositionInObjectSpace();
      PointType node2;
      ++it;
      const double x = point[X];
      const double y = point[Y];
      while (it != itend)
      {
        node2 = it->GetPositionInObjectSpace();

        if (node1 != node2)
        {
          if ((node1[Y] < y && node2[Y] >= y) || (node2[Y] < y && node1[Y] >= y))
          {
            if (node1[X] + ((y - node1[Y]) / (node2[Y] - node1[Y])) * (node2[X] - node1[X]) < x)
            {
              oddNodes = !oddNodes;
            }
          }
          node1 = node2;
        }
        it++;
      }
      if (m_IsClosed)
      {
        node1 = points.back().GetPositionInObjectSpace();
        node2 = points.begin()->GetPositionInObjectSpace();
        // closed PolygonGroup may have the first and last points the same
        if (node1 != node2)
        {
          if ((node1[Y] <= y && node2[Y] > y) || (node2[Y] < y && node1[Y] >= y))
          {
            if (node1[X] + ((y - node1[Y]) / (node2[Y] - node1[Y])) * (node2[X] - node1[X]) < x)
            {
              oddNodes = !oddNodes;
            }
          }
        }
      }

      if (oddNodes)
      {
        return true;
      }
    }
  }
  return false;
}

/** InternalClone */
template <unsigned int TDimension>
typename LightObject::Pointer
PolygonSpatialObject<TDimension>::InternalClone() const
{
  // Default implementation just copies the parameters from
  // this to new transform.
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval = dynamic_cast<Self *>(loPtr.GetPointer());
  if (rval.IsNull())
  {
    itkExceptionMacro(<< "downcast to type " << this->GetNameOfClass() << " failed.");
  }
  rval->SetIsClosed(this->GetIsClosed());
  rval->SetThicknessInObjectSpace(this->GetThicknessInObjectSpace());

  return loPtr;
}

template <unsigned int TDimension>
void
PolygonSpatialObject<TDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "OrientationInObjectSpace: " << m_OrientationInObjectSpace << std::endl;
  os << indent << "OrientationInObjectSpace Time: " << m_OrientationInObjectSpaceMTime << std::endl;
  if (m_IsClosed)
  {
    os << indent << "IsClosed: True" << std::endl;
  }
  else
  {
    os << indent << "IsClosed: True" << std::endl;
  }
  os << indent << "ThicknessInObjectSpace: " << m_ThicknessInObjectSpace << std::endl;
}

} // namespace itk
#endif
