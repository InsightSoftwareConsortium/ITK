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
#ifndef itkContourSpatialObject_hxx
#define itkContourSpatialObject_hxx

#include "itkContourSpatialObject.h"
#include "itkNumericTraits.h"

namespace itk
{
/** Constructor */
template <unsigned int TDimension>
ContourSpatialObject<TDimension>::ContourSpatialObject()
{
  this->SetTypeName("ContourSpatialObject");

  this->Clear();

  this->Update();
}

template <unsigned int TDimension>
void
ContourSpatialObject<TDimension>::Clear()
{
  Superclass::Clear();

  this->GetProperty().SetRed(1);
  this->GetProperty().SetGreen(0);
  this->GetProperty().SetBlue(0);
  this->GetProperty().SetAlpha(1);

  m_ControlPoints.clear();

  m_InterpolationMethod = InterpolationMethodEnum::NO_INTERPOLATION;
  m_InterpolationFactor = 2.0;

  m_IsClosed = false;

  m_OrientationInObjectSpace = -1;
  m_OrientationInObjectSpaceMTime = this->GetMyMTime();

  m_AttachedToSlice = -1;

  this->Modified();
}

template <unsigned int TDimension>
int
ContourSpatialObject<TDimension>::GetOrientationInObjectSpace() const
{
  if (m_OrientationInObjectSpaceMTime == this->GetMyMTime())
  {
    return m_OrientationInObjectSpace;
  }
  m_OrientationInObjectSpaceMTime = this->GetMyMTime();

  const ContourPointListType & points = this->GetPoints();
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

/** Set the control points which are defining the contour */
template <unsigned int TDimension>
void
ContourSpatialObject<TDimension>::SetControlPoints(const ContourPointListType & points)
{
  m_ControlPoints.clear();

  typename ContourPointListType::const_iterator it;
  it = points.begin();
  while (it != points.end())
  {
    m_ControlPoints.push_back(*it);
    m_ControlPoints.back().SetSpatialObject(this);
    it++;
  }
  this->Modified();
}

/** Add a control point which is defining the contour */
template <unsigned int TDimension>
void
ContourSpatialObject<TDimension>::AddControlPoint(const ContourPointType & point)
{
  m_ControlPoints.push_back(point);
  m_ControlPoints.back().SetSpatialObject(this);
  this->Modified();
}

/** InternalClone */
template <unsigned int TDimension>
typename LightObject::Pointer
ContourSpatialObject<TDimension>::InternalClone() const
{
  // Default implementation just copies the parameters from
  // this to new transform.
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval = dynamic_cast<Self *>(loPtr.GetPointer());
  if (rval.IsNull())
  {
    itkExceptionMacro(<< "downcast to type " << this->GetNameOfClass() << " failed.");
  }
  rval->SetInterpolationMethod(this->GetInterpolationMethod());
  rval->SetInterpolationFactor(this->GetInterpolationFactor());
  rval->SetIsClosed(this->GetIsClosed());
  rval->SetAttachedToSlice(this->GetAttachedToSlice());

  rval->SetControlPoints(this->GetControlPoints());

  return loPtr;
}

/** Print the contour spatial object */
template <unsigned int TDimension>
void
ContourSpatialObject<TDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "ContourSpatialObject(" << this << ")" << std::endl;
  os << indent << "#Control Points: " << static_cast<SizeValueType>(m_ControlPoints.size()) << std::endl;
  os << indent << "Interpolation type: " << m_InterpolationMethod << std::endl;
  os << indent << "Contour closed: " << m_IsClosed << std::endl;
  os << indent << "Orientation In Object Space: " << m_OrientationInObjectSpace << std::endl;
  os << indent << "Orientation time: " << m_OrientationInObjectSpaceMTime << std::endl;
  os << indent << "Pin to slice : " << m_AttachedToSlice << std::endl;
  Superclass::PrintSelf(os, indent);
}


/** Print the contour spatial object */
template <unsigned int TDimension>
void
ContourSpatialObject<TDimension>::Update()
{
  switch (m_InterpolationMethod)
  {
    case InterpolationMethodEnum::NO_INTERPOLATION:
      this->SetPoints(m_ControlPoints);
      break;
    case InterpolationMethodEnum::EXPLICIT_INTERPOLATION:
      break;
    case InterpolationMethodEnum::BEZIER_INTERPOLATION:
      // TODO: Implement bezier interpolation
      {
        itkExceptionMacro(<< "Bezier interpolation type not yet defined.");
      }
      break;
    case InterpolationMethodEnum::LINEAR_INTERPOLATION:
      this->m_Points.clear();
      auto it = m_ControlPoints.begin();
      while (it != m_ControlPoints.end())
      {
        auto it2 = ++it;
        if (it2 == m_ControlPoints.end())
        {
          if (this->GetIsClosed())
          {
            it2 = m_ControlPoints.begin();
          }
          else
          {
            break;
          }
        }
        PointType pnt = it->GetPositionInObjectSpace();
        PointType pnt2 = it2->GetPositionInObjectSpace();
        PointType step;
        for (unsigned int d = 0; d < TDimension; ++d)
        {
          step[d] = (pnt2[d] - pnt[d]) / m_InterpolationFactor;
        }
        PointType newPoint;
        newPoint.Fill(NumericTraits<double>::max());
        for (unsigned int i = 0; i < m_InterpolationFactor; ++i)
        {
          for (unsigned int d = 0; d < TDimension; ++d)
          {
            newPoint = pnt[d] + i * step[d];
          }
        }
        typename Superclass::SpatialObjectPointType newSOPoint;
        newSOPoint = (*it);
        newSOPoint.SetSpatialObject(this);
        newSOPoint.SetPositionInObjectSpace(newPoint);
        this->m_Points.push_back(newSOPoint);
      }
      break;
  };

  // Call this last to compute MyBoundingBoxInWorldSpace
  Superclass::Update();
}

} // end namespace itk

#endif
