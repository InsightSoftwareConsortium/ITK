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
#ifndef itkBoxSpatialObject_hxx
#define itkBoxSpatialObject_hxx

#include "itkBoxSpatialObject.h"
#include "itkNumericTraits.h"

namespace itk
{
/** Constructor */
template <unsigned int TDimension>
BoxSpatialObject<TDimension>::BoxSpatialObject()
{
  this->SetTypeName("BoxSpatialObject");

  this->Clear();

  this->Update();
}

template <unsigned int TDimension>
void
BoxSpatialObject<TDimension>::Clear()
{
  Superclass::Clear();

  m_SizeInObjectSpace.Fill(1);
  m_PositionInObjectSpace.Fill(0);

  this->Modified();
}

/** Test whether a point is inside or outside the object */
template <unsigned int TDimension>
bool
BoxSpatialObject<TDimension>::IsInsideInObjectSpace(const PointType & point) const
{
  itkDebugMacro("Checking the point [" << point << "] is in the box");

  return this->GetMyBoundingBoxInObjectSpace()->IsInside(point);
}

/** Compute the bounds of the box */
template <unsigned int TDimension>
void
BoxSpatialObject<TDimension>::ComputeMyBoundingBox()
{
  itkDebugMacro("Computing BoxSpatialObject bounding box");

  PointType pnt1;
  PointType pnt2;
  for (unsigned int i = 0; i < TDimension; i++)
  {
    pnt1[i] = m_PositionInObjectSpace[i];
    pnt2[i] = m_PositionInObjectSpace[i] + m_SizeInObjectSpace[i];
  }

  this->GetModifiableMyBoundingBoxInObjectSpace()->SetMinimum(pnt1);
  this->GetModifiableMyBoundingBoxInObjectSpace()->SetMaximum(pnt1);
  this->GetModifiableMyBoundingBoxInObjectSpace()->ConsiderPoint(pnt2);
  this->GetModifiableMyBoundingBoxInObjectSpace()->ComputeBoundingBox();
}

/** InternalClone */
template <unsigned int TDimension>
typename LightObject::Pointer
BoxSpatialObject<TDimension>::InternalClone() const
{
  // Default implementation just copies the parameters from
  // this to new transform.
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval = dynamic_cast<Self *>(loPtr.GetPointer());
  if (rval.IsNull())
  {
    itkExceptionMacro(<< "downcast to type " << this->GetNameOfClass() << " failed.");
  }
  rval->SetSizeInObjectSpace(this->GetSizeInObjectSpace());
  rval->SetPositionInObjectSpace(this->GetPositionInObjectSpace());

  return loPtr;
}

/** Print Self function */
template <unsigned int TDimension>
void
BoxSpatialObject<TDimension>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Object Size: " << m_SizeInObjectSpace << std::endl;
  os << "Object Position: " << m_PositionInObjectSpace << std::endl;
}
} // end namespace itk

#endif
