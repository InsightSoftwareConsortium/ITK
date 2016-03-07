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
#ifndef itkGaussianSpatialObject_hxx
#define itkGaussianSpatialObject_hxx

#include <cmath>
#include "itkGaussianSpatialObject.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
GaussianSpatialObject< TDimension >
::GaussianSpatialObject()
{
  this->SetTypeName("GaussianSpatialObject");
  this->SetDimension(TDimension);
  m_Radius = 1.0;
  m_Sigma = 1.0;
  m_Maximum = 1.0;
}

/** Destructor */
template< unsigned int TDimension >
GaussianSpatialObject< TDimension >
::~GaussianSpatialObject()
{}

/** The z-score is the root mean square of the z-scores along
 *  each principal axis. */
template< unsigned int TDimension >
typename GaussianSpatialObject< TDimension >::ScalarType
GaussianSpatialObject< TDimension >
::SquaredZScore(const PointType & point) const
{
  if ( !this->SetInternalInverseTransformToWorldToIndexTransform() )
    {
    return 0;
    }

  PointType transformedPoint =
    this->GetInternalInverseTransform()->TransformPoint(point);

  ScalarType r = 0;
  for ( unsigned int i = 0; i < TDimension; i++ )
    {
    r += transformedPoint[i] * transformedPoint[i];
    }
  return r / ( m_Sigma * m_Sigma );
}

/** Test whether a point is inside or outside the object.
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth. */
template< unsigned int TDimension >
bool
GaussianSpatialObject< TDimension >
::IsInside(const PointType & point) const
{
  if ( m_Radius < itk::Math::eps )
    {
    return false;
    }

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

  double r = 0;
  for ( unsigned int i = 0; i < TDimension; i++ )
    {
    r += transformedPoint[i] * transformedPoint[i];
    }

  r /= ( m_Radius * m_Radius );

  if ( r < 1.0 )
    {
    return true;
    }

  return false;
}

/** Test if the given point is inside the boundary of the spatial
 * object. */
template< unsigned int TDimension >
bool
GaussianSpatialObject< TDimension >
::IsInside(const PointType & point, unsigned int depth, char *name) const
{
  itkDebugMacro("Checking the point [" << point
                                       << "] is inside the GaussianSpatialObject");

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

/** Compute the bounds of the Gaussian (as determined by the
 *  specified radius). */
template< unsigned int TDimension >
bool
GaussianSpatialObject< TDimension >
::ComputeLocalBoundingBox() const
{
  if ( this->GetBoundingBoxChildrenName().empty()
       || strstr( typeid( Self ).name(),
                  this->GetBoundingBoxChildrenName().c_str() ) )
    {
    // we need to set the minimum and maximum of the bounding box
    // the center is always inside the bounding box.
    PointType center;
    center.Fill(0);
    center = this->GetIndexToWorldTransform()->TransformPoint(center);
    const_cast< BoundingBoxType * >( this->GetBounds() )->SetMinimum(center);
    const_cast< BoundingBoxType * >( this->GetBounds() )->SetMaximum(center);

    // First we compute the bounding box in the index space
    typename BoundingBoxType::Pointer bb = BoundingBoxType::New();

    PointType    pntMin;
    PointType    pntMax;
    unsigned int i;
    for ( i = 0; i < TDimension; i++ )
      {
      pntMin[i] = -m_Radius;
      pntMax[i] = m_Radius;
      }

    bb->SetMinimum(pntMin);
    bb->SetMaximum(pntMax);

    bb->ComputeBoundingBox();

    typedef typename BoundingBoxType::PointsContainer PointsContainer;
    const PointsContainer *corners = bb->GetCorners();
    typename BoundingBoxType::PointsContainer::const_iterator
    it = corners->begin();
    while ( it != corners->end() )
      {
      PointType pnt = this->GetIndexToWorldTransform()->TransformPoint(*it);
      const_cast< BoundingBoxType * >( this->GetBounds() )->ConsiderPoint(pnt);
      ++it;
      }
    }
  return true;
}

/** Returns if the ellipse is evaluable at one point. */
template< unsigned int TDimension >
bool
GaussianSpatialObject< TDimension >
::IsEvaluableAt(const PointType & point,
                unsigned int depth, char *name) const
{
  itkDebugMacro("Checking if the ellipse is evaluable at " << point);
  return IsInside(point, depth, name);
}

/** Returns the value at one point. */
template< unsigned int TDimension >
bool
GaussianSpatialObject< TDimension >
::ValueAt(const PointType & point, ScalarType & value, unsigned int depth,
          char *name) const
{
  itkDebugMacro("Getting the value of the ellipse at " << point);
  if ( IsInside(point, 0, name) )
    {
    const double zsq = this->SquaredZScore(point);
    value = m_Maximum * (ScalarType)std::exp(-zsq / 2.0);
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

/** Returns the sigma=m_Radius level set of the Gaussian function, as an
 * EllipseSpatialObject. */
template< unsigned int TDimension >
typename EllipseSpatialObject< TDimension >::Pointer
GaussianSpatialObject< TDimension >
::GetEllipsoid() const
{
  typedef itk::EllipseSpatialObject< TDimension > EllipseType;
  typename EllipseType::Pointer ellipse = EllipseType::New();

  ellipse->SetRadius(m_Radius);

  ellipse->GetIndexToObjectTransform()->SetCenter(
    this->GetIndexToObjectTransform()->GetCenter() );
  ellipse->GetIndexToObjectTransform()->SetMatrix(
    this->GetIndexToObjectTransform()->GetMatrix() );
  ellipse->GetIndexToObjectTransform()->SetOffset(
    this->GetIndexToObjectTransform()->GetOffset() );

  ellipse->GetModifiableObjectToWorldTransform()->SetCenter(
    this->GetObjectToWorldTransform()->GetCenter() );
  ellipse->GetModifiableObjectToWorldTransform()->SetMatrix(
    this->GetObjectToWorldTransform()->GetMatrix() );
  ellipse->GetModifiableObjectToWorldTransform()->SetOffset(
    this->GetObjectToWorldTransform()->GetOffset() );

  ellipse->GetModifiableIndexToWorldTransform()->SetCenter(
    this->GetIndexToWorldTransform()->GetCenter() );
  ellipse->GetModifiableIndexToWorldTransform()->SetMatrix(
    this->GetIndexToWorldTransform()->GetMatrix() );
  ellipse->GetModifiableIndexToWorldTransform()->SetOffset(
    this->GetIndexToWorldTransform()->GetOffset() );

  return ellipse;
}

/** Print Self function. */
template< unsigned int TDimension >
void
GaussianSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Maximum: " << m_Maximum << std::endl;
  os << "Radius: " << m_Radius << std::endl;
  os << "Sigma: " << m_Sigma << std::endl;
}
} // end namespace itk

#endif
