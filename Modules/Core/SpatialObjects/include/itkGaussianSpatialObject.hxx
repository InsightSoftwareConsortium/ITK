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
  m_Radius = 1.0;
  m_Sigma = 1.0;
  m_Maximum = 1.0;
}

/** The z-score is the root mean square of the z-scores along
 *  each principal axis. */
template< unsigned int TDimension >
typename GaussianSpatialObject< TDimension >::ScalarType
GaussianSpatialObject< TDimension >
::SquaredZScore(const PointType & point) const
{
  PointType transformedPoint =
    this->GetObjectToWorldTransform()->GetInverseTransform()
      ->TransformPoint(point);

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
::IsInsideInWorldSpace(const PointType & point, unsigned int depth,
  const std::string & name) const
{
  if ( m_Radius < itk::Math::eps )
    {
    return false;
    }

  if ( !this->GetMyBoundingBoxInWorldSpace()->IsInsideInWorldSpace(point) )
    {
    return false;
    }

  PointType transformedPoint =
    this->GetObjectToWorldTransform()->GetInverse()->TransformPoint(point);

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

  if( depth > 0 )
    {
    return Superclass::IsInsideInWorldSpaceChildrenInWorldSpace(point, depth-1, name);
    }

  return false;
}

/** Compute the bounds of the Gaussian (as determined by the
 *  specified radius). */
template< unsigned int TDimension >
bool
GaussianSpatialObject< TDimension >
::ComputeMyBoundingBoxInWorldSpace() const
{
  itkDebugMacro("Computing Guassian bounding box");

  // First we compute the bounding box in the object space
  typename BoundingBoxType::Pointer bb = BoundingBoxType::New();

  PointType    pnt1;
  PointType    pnt2;
  unsigned int i;
  for ( i = 0; i < TDimension; i++ )
    {
    pnt1[i] = m_Center[i] - m_Radius[i];
    pnt2[i] = m_Center[i] + m_Radius[i];
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
  const_cast< BoundingBoxType * >( this->GetMyBoundingBoxInWorldSpace() )
    ->SetPoints(transformedCorners);
  this->GetMyBoundingBoxInWorldSpace()->ComputeBoundingBox();

  return true;
}

/** Returns the value at one point. */
template< unsigned int TDimension >
bool
GaussianSpatialObject< TDimension >
::ValueAtInWorldSpace(const PointType & point, double & value, unsigned int depth,
    const std::string & name) const
{
  itkDebugMacro("Getting the value of the ellipse at " << point);
  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    if( IsInsideInWorldSpace(point) )
      {
      const double zsq = this->SquaredZScore(point);
      value = m_Maximum * (ScalarType)std::exp(-zsq / 2.0);
      return true;
      }
    }

  if( depth > 0 )
    {
    if( Superclass::ValueAtInWorldSpaceChildrenInWorldSpace(point, value, depth-1, name) )
      {
      return true;
      }
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
  using EllipseType = itk::EllipseSpatialObject< TDimension >;
  typename EllipseType::Pointer ellipse = EllipseType::New();

  ellipse->SetRadiusInWorldSpace(m_Radius);
  ellipse->SetCenterInWorldSpace(m_Center);

  ellipse->GetObjectToWorldTransform()->SetFixedParameters(
    this->GetObjectToWorldTransform()->GetFixedParameters() );
  ellipse->GetObjectToWorldTransform()->SetParameters(
    this->GetObjectToWorldTransform()->GetParameters() );

  ellipse->ComputeMyBoundingBoxInWorldSpace();

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
