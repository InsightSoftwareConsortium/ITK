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
#include "itkCylinderSpatialObject.h"

namespace itk
{
/** Constructor */
CylinderSpatialObject::CylinderSpatialObject()
{
  this->SetTypeName("CylinderSpatialObject");
  this->SetDimension(3);
  m_Radius = 1.0;
  m_Height = 1.0;
}

/** Destructor */
CylinderSpatialObject::~CylinderSpatialObject()
{}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
bool CylinderSpatialObject
::IsInside(const PointType & point) const
{
  if ( !this->SetInternalInverseTransformToWorldToIndexTransform() )
    {
    return false;
    }

  PointType transformedPoint =
    this->GetInternalInverseTransform()->TransformPoint(point);

  // Does the point lie above or below the cylinder (y-axis)?
  if ( transformedPoint[1] < -0.5 * m_Height ||
       transformedPoint[1] >  0.5 * m_Height )
    {
    return false;
    }

  // Does the point lie outside the radius of the cylinder?
  if ( Math::sqr(transformedPoint[0]) + Math::sqr(transformedPoint[2]) > Math::sqr(m_Radius) )
    {
    return false;
    }

  return true;
}

/** Test if the given point is inside the Cylinder */
bool CylinderSpatialObject
::IsInside(const PointType & point, unsigned int depth, char *name) const
{
  itkDebugMacro("Checking the point [" << point << "] is inside the Cylinder");

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

/** Compute the bounds of the Cylinder */
bool CylinderSpatialObject
::ComputeLocalBoundingBox() const
{
  itkDebugMacro("Computing tube bounding box");

  if ( this->GetBoundingBoxChildrenName().empty()
       || strstr( typeid( Self ).name(), this->GetBoundingBoxChildrenName().c_str() ) )
    {
    // First we compute the bounding box in the index space
    BoundingBoxType::Pointer bb = BoundingBoxType::New();
    PointType ptMin, ptMax;
    ptMin[0] = -m_Radius;
    ptMin[1] = -m_Height / 2;
    ptMin[2] = -m_Radius;
    ptMax[0] = m_Radius;
    ptMax[1] = m_Height / 2;
    ptMax[2] = m_Radius;
    bb->SetMinimum(ptMin);
    bb->SetMaximum(ptMax);

    // Initialize the final bounding box by setting the min and max to the
    // transformed ptMin and ptMax.
    ptMin = this->GetIndexToWorldTransform()->TransformPoint(ptMin);
    const_cast< BoundingBoxType * >( this->GetBounds() )->SetMinimum(ptMin);
    ptMax = this->GetIndexToWorldTransform()->TransformPoint(ptMax);
    const_cast< BoundingBoxType * >( this->GetBounds() )->SetMaximum(ptMax);

    // Transform all corners of the bounding box in index space to world space,
    // and make sure that the final bounding box includes these.
    const BoundingBoxType::PointsContainer *corners = bb->GetCorners();
    BoundingBoxType::PointsContainer::const_iterator
    itBB = corners->begin();
    while ( itBB != corners->end() )
      {
      PointType pnt =
        this->GetIndexToWorldTransform()->TransformPoint(*itBB);
      const_cast< BoundingBoxType * >( this->GetBounds() )->ConsiderPoint(pnt);
      ++itBB;
      }
    }
  return true;
}

/** Returns if the Cylinder os evaluable at one point */
bool CylinderSpatialObject
::IsEvaluableAt(const PointType & point, unsigned int depth, char *name) const
{
  itkDebugMacro("Checking if the Cylinder is evaluable at " << point);
  return IsInside(point, depth, name);
}

/** Returns the value at one point */
bool CylinderSpatialObject
::ValueAt(const PointType & point, double & value, unsigned int depth,
          char *name) const
{
  itkDebugMacro("Getting the value of the Cylinder at " << point);
  if ( IsInside(point, 0, name) )
    {
    value = this->GetDefaultInsideValue();
    return true;
    }
  else
    {
    if ( Superclass::IsEvaluableAt(point, depth, name) )
      {
      Superclass::ValueAt(point, value, depth, name);
      return true;
      }
    else
      {
      value = this->GetDefaultOutsideValue();
      }
    }
  return false;
}

/** Print Self function */
void CylinderSpatialObject
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Radius: " << m_Radius << std::endl;
  os << "Height: " << m_Height << std::endl;
}
} // end namespace itk
