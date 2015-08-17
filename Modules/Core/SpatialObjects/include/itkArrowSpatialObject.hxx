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
#ifndef itkArrowSpatialObject_hxx
#define itkArrowSpatialObject_hxx

#include "itkArrowSpatialObject.h"
#include "itkEuler3DTransform.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
ArrowSpatialObject< TDimension >
::ArrowSpatialObject()
{
  this->SetDimension(TDimension);
  this->SetTypeName ("ArrowSpatialObject");
  this->GetProperty()->SetRed(1);
  this->GetProperty()->SetGreen(0);
  this->GetProperty()->SetBlue(0);
  this->GetProperty()->SetAlpha(1);

  m_Direction.Fill(0);
  m_Direction[0] = 1; // along the x direction by default
  m_Position.Fill(0);
  m_Length = 1;

  this->ComputeBoundingBox();
}

/** Destructor */
template< unsigned int TDimension >
ArrowSpatialObject< TDimension >
::~ArrowSpatialObject()
{}

/** Set the length of the arrow */
template< unsigned int TDimension >
void
ArrowSpatialObject< TDimension >
::SetLength(double length)
{
  m_Length = length;
  double spacing[TDimension];
  spacing[0] = m_Length;

  for ( unsigned int i = 1; i < TDimension; i++ )
    {
    spacing[i] = 1;
    }
  this->SetSpacing(spacing);
  this->Modified();
}

/** Compute the bounding box */
template< unsigned int TDimension >
bool
ArrowSpatialObject< TDimension >
::ComputeLocalBoundingBox() const
{
  itkDebugMacro("Computing Rectangle bounding box");

  if ( this->GetBoundingBoxChildrenName().empty()
       || strstr( typeid( Self ).name(), this->GetBoundingBoxChildrenName().c_str() ) )
    {
    PointType pnt = this->GetPosition();
    PointType pnt2;
    for ( unsigned int i = 0; i < TDimension; i++ )
      {
      pnt2[i] = pnt[i] + m_Length * m_Direction[i];
      }

    pnt = this->GetIndexToWorldTransform()->TransformPoint(pnt);
    pnt2 = this->GetIndexToWorldTransform()->TransformPoint(pnt2);

    const_cast< typename Superclass::BoundingBoxType * >(
      this->GetBounds() )->SetMinimum(pnt);
    const_cast< typename Superclass::BoundingBoxType * >(
      this->GetBounds() )->SetMaximum(pnt2);
    }
  return true;
}

/** Check if a given point is on the arrow */
template< unsigned int TDimension >
bool
ArrowSpatialObject< TDimension >
::IsInside(const PointType & point, unsigned int depth, char *name) const
{
  itkDebugMacro("Checking the point [" << point << "] is on the Line");

  if ( name == ITK_NULLPTR )
    {
    if ( IsInside(point) )
      {
      return true;
      }
    }
  else if ( strstr(typeid( Self ).name(), name) )
    {
    if ( IsInside(point) )
      {
      return true;
      }
    }

  return Superclass::IsInside(point, depth, name);
}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template< unsigned int TDimension >
bool
ArrowSpatialObject< TDimension >
::IsInside(const PointType & point) const
{
  if ( !this->SetInternalInverseTransformToWorldToIndexTransform() )
    {
    return false;
    }

  PointType transformedPoint =
    this->GetInternalInverseTransform()->TransformPoint(point);

  this->ComputeLocalBoundingBox();

  if ( this->GetBounds()->IsInside(transformedPoint) )
    {
    // If the transformedPoint lies on the line between the two points
    PointType pnt = this->GetPosition();
    PointType pnt2;
    for ( unsigned int i = 0; i < TDimension; i++ )
      {
      pnt2[i] = pnt[i] + m_Length * m_Direction[i];
      }

    VectorType v = pnt2 - pnt;
    VectorType v2 = transformedPoint - pnt;

    v.Normalize();
    v2.Normalize();

    if ( Math::AlmostEquals( dot_product( v.GetVnlVector(), v2.GetVnlVector() ), NumericTraits< typename VectorType::ValueType >::OneValue() ) )
      {
      return true;
      }
    }

  return false;
}

/** Update the local transform from the position and the direction */
template< unsigned int TDimension >
void
ArrowSpatialObject< TDimension >
::UpdateTransform()
{
  //TODO: What should happen if TDimension is not equal to 3
  VectorType offset;
  for ( unsigned int i = 0; i < TDimension; i++ )
    {
    offset[i] = m_Position[i];
    }
  this->GetObjectToParentTransform()->SetOffset(offset);

  // If the given direction is not normalized we set the length of the vector
  // as the length of the arrow
  m_Length = m_Direction.GetSquaredNorm();
  if ( m_Length != 0.0 )
    {
    m_Length = std::sqrt(m_Length);
    }
  else
    {
    this->Modified();
    return;
    }

  m_Direction.Normalize();
  this->Modified();
}

template< > void ArrowSpatialObject< 3 > ::UpdateTransform();


/** Print the object */
template< unsigned int TDimension >
void
ArrowSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "ArrowSpatialObject(" << this << ")" << std::endl;
  Superclass::PrintSelf(os, indent);
  os << indent << "Position = " << m_Position << std::endl;
  os << indent << "Direction = " << m_Direction << std::endl;
  os << indent << "Length = " << m_Length << std::endl;
}
} // end namespace itk

#endif // end itkArrowSpatialObject_hxx
