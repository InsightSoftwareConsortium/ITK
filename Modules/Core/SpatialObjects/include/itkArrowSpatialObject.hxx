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
#ifndef itkArrowSpatialObject_hxx
#define itkArrowSpatialObject_hxx

#include "itkArrowSpatialObject.h"

namespace itk
{
/** Constructor */
template <unsigned int TDimension>
ArrowSpatialObject<TDimension>::ArrowSpatialObject()
{
  this->SetTypeName("ArrowSpatialObject");

  this->Clear();

  this->Update();
}

template <unsigned int TDimension>
void
ArrowSpatialObject<TDimension>::Clear()
{
  Superclass::Clear();

  this->GetProperty().SetRed(1);
  this->GetProperty().SetGreen(0);
  this->GetProperty().SetBlue(0);
  this->GetProperty().SetAlpha(1);

  m_DirectionInObjectSpace.Fill(0);
  m_DirectionInObjectSpace[0] = 1; // along the x direction by default
  m_PositionInObjectSpace.Fill(0);
  m_LengthInObjectSpace = 1;

  this->Modified();
}

/** Compute the bounding box */
template <unsigned int TDimension>
void
ArrowSpatialObject<TDimension>::ComputeMyBoundingBox()
{
  itkDebugMacro("Computing Rectangle bounding box");

  PointType pnt = this->GetPositionInObjectSpace();

  this->GetModifiableMyBoundingBoxInObjectSpace()->SetMinimum(pnt);
  this->GetModifiableMyBoundingBoxInObjectSpace()->SetMaximum(pnt);
}

/** Check if a given point is on the arrow */
template <unsigned int TDimension>
bool
ArrowSpatialObject<TDimension>::IsInsideInObjectSpace(const PointType & point) const
{
  itkDebugMacro("Checking the point [" << point << "] is on the Line");

  PointType pnt = this->GetPositionInObjectSpace();

  bool isInside = true;
  for (unsigned int i = 0; i < TDimension; i++)
  {
    if (Math::NotExactlyEquals(point[i], pnt[i]))
    {
      isInside = false;
      break;
    }
  }
  if (isInside)
  {
    return true;
  }

  return false;
}

template <unsigned int TDimension>
typename ArrowSpatialObject<TDimension>::PointType
ArrowSpatialObject<TDimension>::GetPositionInWorldSpace() const
{
  PointType pnt = this->GetPositionInObjectSpace();

  pnt = this->GetObjectToWorldTransform()->TransformPoint(pnt);

  return pnt;
}

template <unsigned int TDimension>
typename ArrowSpatialObject<TDimension>::VectorType
ArrowSpatialObject<TDimension>::GetDirectionInWorldSpace() const
{
  PointType pnt = this->GetPositionInObjectSpace();
  PointType pnt2;
  for (unsigned int i = 0; i < TDimension; i++)
  {
    pnt2[i] = pnt[i] + m_LengthInObjectSpace * m_DirectionInObjectSpace[i];
  }

  pnt = this->GetObjectToWorldTransform()->TransformPoint(pnt);
  pnt2 = this->GetObjectToWorldTransform()->TransformPoint(pnt2);

  VectorType dir = pnt2 - pnt;
  dir.Normalize();

  return dir;
}

template <unsigned int TDimension>
double
ArrowSpatialObject<TDimension>::GetLengthInWorldSpace() const
{
  PointType pnt = this->GetPositionInObjectSpace();
  PointType pnt2;
  for (unsigned int i = 0; i < TDimension; i++)
  {
    pnt2[i] = pnt[i] + m_LengthInObjectSpace * m_DirectionInObjectSpace[i];
  }

  pnt = this->GetObjectToWorldTransform()->TransformPoint(pnt);
  pnt2 = this->GetObjectToWorldTransform()->TransformPoint(pnt2);

  double len = pnt.EuclideanDistanceTo(pnt2);

  return len;
}

/** InternalClone */
template <unsigned int TDimension>
typename LightObject::Pointer
ArrowSpatialObject<TDimension>::InternalClone() const
{
  // Default implementation just copies the parameters from
  // this to new transform.
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval = dynamic_cast<Self *>(loPtr.GetPointer());
  if (rval.IsNull())
  {
    itkExceptionMacro(<< "downcast to type " << this->GetNameOfClass() << " failed.");
  }
  rval->SetDirectionInObjectSpace(this->GetDirectionInObjectSpace());
  rval->SetPositionInObjectSpace(this->GetPositionInObjectSpace());
  rval->SetLengthInObjectSpace(this->GetLengthInObjectSpace());

  return loPtr;
}

template <unsigned int TDimension>
void
ArrowSpatialObject<TDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "ArrowSpatialObject(" << this << ")" << std::endl;
  Superclass::PrintSelf(os, indent);
  os << indent << "Object Position = " << m_PositionInObjectSpace << std::endl;
  os << indent << "Object Direction = " << m_DirectionInObjectSpace << std::endl;
  os << indent << "Object Length = " << m_LengthInObjectSpace << std::endl;
}
} // end namespace itk

#endif // end itkArrowSpatialObject_hxx
