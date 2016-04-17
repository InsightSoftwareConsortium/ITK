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
#ifndef itkBoxSpatialObject_hxx
#define itkBoxSpatialObject_hxx

#include "itkBoxSpatialObject.h"
#include "itkNumericTraits.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
BoxSpatialObject< TDimension >
::BoxSpatialObject()
{
  this->SetTypeName("BoxSpatialObject");
  m_Size.Fill(0);
  this->SetDimension(TDimension);
}

/** Destructor */
template< unsigned int TDimension >
BoxSpatialObject< TDimension >
::~BoxSpatialObject()
{}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template< unsigned int TDimension >
bool
BoxSpatialObject< TDimension >
::IsInside(const PointType & point) const
{
  this->ComputeLocalBoundingBox();
  if ( !this->GetBounds()->IsInside(point) )
    {
    return false;
    }

  if ( !this->SetInternalInverseTransformToWorldToIndexTransform() )
    {
    return false;
    }

  PointType transformedPoint =
    this->GetInternalInverseTransform()->TransformPoint(point);

  bool isInside = true;
  for ( unsigned int i = 0; i < TDimension; i++ )
    {
    if ( m_Size[i] )
      {
      if ( ( transformedPoint[i] > m_Size[i] ) || ( transformedPoint[i] < 0 ) )
        {
        isInside = false;
        break;
        }
      }
    else
      {
      itkExceptionMacro(<< "Size of the BoxSpatialObject must be non-zero!");
      }
    }

  return isInside;
}

/** Test if the given point is inside the box. A point on the border is
 *  considered inside. */
template< unsigned int TDimension >
bool
BoxSpatialObject< TDimension >
::IsInside(const PointType & point, unsigned int depth, char *name) const
{
  itkDebugMacro("Checking the point ["
                << point << "] is inside the AxisAlignedBox");

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

/** Compute the bounds of the box */
template< unsigned int TDimension >
bool
BoxSpatialObject< TDimension >
::ComputeLocalBoundingBox() const
{
  itkDebugMacro("Computing BoxSpatialObject bounding box");

  if ( this->GetBoundingBoxChildrenName().empty()
       || strstr( typeid( Self ).name(),
                  this->GetBoundingBoxChildrenName().c_str() ) )
    {
    // First we compute the bounding box in the index space
    typename BoundingBoxType::Pointer bb = BoundingBoxType::New();

    PointType    pntMin;
    PointType    pntMax;
    unsigned int i;
    for ( i = 0; i < TDimension; i++ )
      {
      pntMin[i] = NumericTraits< typename PointType::ValueType >::ZeroValue();
      pntMax[i] = static_cast< typename PointType::ValueType >( m_Size[i] );
      }

    bb->SetMinimum(pntMin);
    bb->SetMaximum(pntMax);
    bb->ComputeBoundingBox();

    // Next Transform the corners of the bounding box
    typedef typename BoundingBoxType::PointsContainer PointsContainer;
    const PointsContainer *corners = bb->GetCorners();
    typename PointsContainer::Pointer transformedCorners = PointsContainer::New();
    transformedCorners->Reserve(static_cast<typename PointsContainer::ElementIdentifier>( corners->size() ));

    typename PointsContainer::const_iterator it = corners->begin();
    typename PointsContainer::iterator itTrans = transformedCorners->begin();
    while ( it != corners->end() )
      {
      PointType pnt = this->GetIndexToWorldTransform()->TransformPoint(*it);
      *itTrans = pnt;
      ++it;
      ++itTrans;
      }

    // refresh the bounding box with the transformed corners
    const_cast< BoundingBoxType * >( this->GetBounds() )->SetPoints(transformedCorners);
    this->GetBounds()->ComputeBoundingBox();
    }
  return true;
}

/** Returns if the box is evaluable at one point */
template< unsigned int TDimension >
bool
BoxSpatialObject< TDimension >
::IsEvaluableAt(const PointType & point,
                unsigned int depth, char *name) const
{
  itkDebugMacro("Checking if the BoxSpatialObject is evaluable at " << point);
  return IsInside(point, depth, name);
}

/** Returns the value at one point */
template< unsigned int TDimension >
bool
BoxSpatialObject< TDimension >
::ValueAt(const PointType & point, double & value, unsigned int depth,
          char *name) const
{
  itkDebugMacro("Getting the value of the BoxSpatialObject at " << point);
  if ( IsInside(point, 0, name) )
    {
    value = this->GetDefaultInsideValue();
    return true;
    }
  else if ( Superclass::IsEvaluableAt(point, depth, name) )
    {
    Superclass::ValueAt(point, value, depth, name);
    return true;
    }
  value = this->GetDefaultOutsideValue();
  return false;
}

/** Print Self function */
template< unsigned int TDimension >
void
BoxSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Size: " << m_Size << std::endl;
}
} // end namespace itk

#endif
