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
  m_Size.Fill(1);
  m_Position.Fill(0);
}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
/** Destructor */
template< unsigned int TDimension >
void
BoxSpatialObject< TDimension >
::SetSize(const SizeType & s) const
{
  this->m_Size = s;
  this->Modified();
}

template< unsigned int TDimension >
void
BoxSpatialObject< TDimension >
::SetPosition(const PointType & p) const
{
  this->m_Position = p;
  this->Modified();
}

template< unsigned int TDimension >
bool
BoxSpatialObject< TDimension >
::IsInside(const PointType & point, unsigned int depth,
  const std::string & name) const
{
  itkDebugMacro("Checking the point [" << point << "] is in the box");

  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    if( this->GetObjectBounds()->IsInside( point ) )
      {
      PointType transformedPoint = this->GetObjectToWorldTransform()
        ->GetInverse()->TransformPoint(point);

      bool isOutside = false;
      for ( unsigned int i = 0; i < TDimension; i++ )
        {
        if ( ( transformedPoint[i] - m_Position[i] > m_Size[i] )
          || ( transformedPoint[i] - m_Position[i] < 0 ) )
          {
          isOutside = true;
          break;
          }
        }
      if( !isOutside )
        {
        return true;
        }
      }
    }

  if( depth > 0 )
    {
    return Superclass::IsInsideChildren(point, depth-1, name);
    }

  return false;
}

/** Compute the bounds of the box */
template< unsigned int TDimension >
bool
BoxSpatialObject< TDimension >
::ComputeObjectBoundingBox() const
{
  itkDebugMacro("Computing BoxSpatialObject bounding box");

  // First we compute the bounding box in the object space
  typename BoundingBoxType::Pointer bb = BoundingBoxType::New();

  PointType    pnt1;
  PointType    pnt2;
  for ( unsigned int i = 0; i < TDimension; i++ )
    {
    pnt1[i] = m_Position[i];
    pnt2[i] = m_Position[i] + m_Size[i];
    }

  bb->SetMinimum(pnt1);
  bb->SetMaximum(pnt1);
  bb->ConsiderPoint(pnt2);
  bb->ComputeBoundingBox();

  // Next Transform the corners of the bounding box
  using PointsContainer = typename BoundingBoxType::PointsContainer;
  const PointsContainer *corners = bb->GetCorners();
  typename PointsContainer::Pointer transformedCorners =
    PointsContainer::New();
  transformedCorners->Reserve(
    static_cast<typename PointsContainer::ElementIdentifier>(
      corners->size() ) );

  auto it = corners->begin();
  auto itTrans = transformedCorners->begin();
  while ( it != corners->end() )
    {
    PointType pnt = this->GetObjectToWorldTransform()->TransformPoint(*it);
    *itTrans = pnt;
    ++it;
    ++itTrans;
    }

  // refresh the bounding box with the transformed corners
  const_cast< BoundingBoxType * >( this->GetObjectBounds() )
    ->SetPoints(transformedCorners);
  this->GetObjectBounds()->ComputeBoundingBox();

  return true;
}

/** Print Self function */
template< unsigned int TDimension >
void
BoxSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Size: " << m_Size << std::endl;
  os << "Position: " << m_Position << std::endl;
}
} // end namespace itk

#endif
