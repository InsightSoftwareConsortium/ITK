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
#ifndef itkEllipseSpatialObject_hxx
#define itkEllipseSpatialObject_hxx

#include "itkEllipseSpatialObject.h"

namespace itk
{
/** Constructor */
template <unsigned int TDimension>
EllipseSpatialObject<TDimension>::EllipseSpatialObject()
{
  this->SetTypeName("EllipseSpatialObject");

  this->Clear();

  this->Update();
}

template <unsigned int TDimension>
void
EllipseSpatialObject<TDimension>::Clear()
{
  Superclass::Clear();

  m_RadiusInObjectSpace.Fill(1.0);
  m_CenterInObjectSpace.Fill(0.0);

  this->Modified();
}

/** Define the radius of the circle in object space.
 * An ellipse is formed by setting the ObjectToParentTransform */
template <unsigned int TDimension>
void
EllipseSpatialObject<TDimension>::SetRadiusInObjectSpace(double radius)
{
  bool changes = false;
  for (unsigned int i = 0; i < ObjectDimension; ++i)
  {
    if (m_RadiusInObjectSpace[i] != radius)
    {
      m_RadiusInObjectSpace[i] = radius;
      changes = true;
    }
  }
  if (changes)
  {
    this->Modified();
  }
}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template <unsigned int TDimension>
bool
EllipseSpatialObject<TDimension>::IsInsideInObjectSpace(const PointType & point) const
{
  double d;
  double r = 0;
  for (unsigned int i = 0; i < TDimension; i++)
  {
    if (m_RadiusInObjectSpace[i] > 0.0)
    {
      d = point[i] - m_CenterInObjectSpace[i];
      r += (d * d) / (m_RadiusInObjectSpace[i] * m_RadiusInObjectSpace[i]);
    }
    else if (point[i] != 0.0 || m_RadiusInObjectSpace[i] < 0)
    // Deal with an ellipse with 0 or negative radius;
    {
      r = 2; // Keeps function from returning true here
      break;
    }
  }

  if (r < 1)
  {
    return true;
  }

  return false;
}

/** Compute the bounds of the ellipse */
template <unsigned int TDimension>
void
EllipseSpatialObject<TDimension>::ComputeMyBoundingBox()
{
  itkDebugMacro("Computing ellipse bounding box");

  PointType pnt1;
  PointType pnt2;
  for (unsigned int i = 0; i < TDimension; i++)
  {
    pnt1[i] = m_CenterInObjectSpace[i] - m_RadiusInObjectSpace[i];
    pnt2[i] = m_CenterInObjectSpace[i] + m_RadiusInObjectSpace[i];
  }

  this->GetModifiableMyBoundingBoxInObjectSpace()->SetMinimum(pnt1);
  this->GetModifiableMyBoundingBoxInObjectSpace()->SetMaximum(pnt1);
  this->GetModifiableMyBoundingBoxInObjectSpace()->ConsiderPoint(pnt2);
  this->GetModifiableMyBoundingBoxInObjectSpace()->ComputeBoundingBox();
}

/** InternalClone */
template <unsigned int TDimension>
typename LightObject::Pointer
EllipseSpatialObject<TDimension>::InternalClone() const
{
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval = dynamic_cast<Self *>(loPtr.GetPointer());
  if (rval.IsNull())
  {
    itkExceptionMacro(<< "Downcast to type " << this->GetNameOfClass() << " failed.");
  }
  rval->SetRadiusInObjectSpace(this->GetRadiusInObjectSpace());
  rval->SetCenterInObjectSpace(this->GetCenterInObjectSpace());

  return loPtr;
}

/** Print Self function */
template <unsigned int TDimension>
void
EllipseSpatialObject<TDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "EllipseSpatialObject(" << this << ")" << std::endl;
  Superclass::PrintSelf(os, indent);
  os << "Object Radii: " << m_RadiusInObjectSpace << std::endl;
  os << "Object Center: " << m_CenterInObjectSpace << std::endl;
}

} // end namespace itk

#endif
