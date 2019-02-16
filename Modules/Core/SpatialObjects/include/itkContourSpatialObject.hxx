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
#ifndef itkContourSpatialObject_hxx
#define itkContourSpatialObject_hxx

#include "itkContourSpatialObject.h"
#include "itkNumericTraits.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
ContourSpatialObject< TDimension >
::ContourSpatialObject()
{
  this->SetTypeName("ContourSpatialObject");

  this->GetProperty().SetRed(1);
  this->GetProperty().SetGreen(0);
  this->GetProperty().SetBlue(0);
  this->GetProperty().SetAlpha(1);

  m_InterpolationMethod = NO_INTERPOLATION;
  m_IsClosed = false;
  m_OrientationInObjectSpace = -1;
  m_AttachedToSlice = -1;
}

/** Destructor */
template< unsigned int TDimension >
ContourSpatialObject< TDimension >
::~ContourSpatialObject() = default;

/** Set the control points which are defining the contour */
template< unsigned int TDimension >
void
ContourSpatialObject< TDimension >
::SetControlPoints(ContourPointListType & points)
{
  m_ControlPoints.clear();

  typename ContourPointListType::iterator it, end;
  it = points.begin();
  while ( it != points.end() )
    {
    it->SetSpatialObject( this );
    m_ControlPoints.push_back(*it);
    it++;
    }
  this->Modified();
}

/** Print the contour spatial object */
template< unsigned int TDimension >
void
ContourSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  os << indent << "ContourSpatialObject(" << this << ")" << std::endl;
  os << indent << "#Control Points: "
     << static_cast< SizeValueType >( m_ControlPoints.size() ) << std::endl;
  os << indent << "Interpolation type: " << m_InterpolationMethod << std::endl;
  os << indent << "Contour closed: " << m_IsClosed << std::endl;
  os << indent << "Orientation In Object Space: " << m_OrientationInObjectSpace
    << std::endl;
  os << indent << "Pin to slice : " << m_AttachedToSlice << std::endl;
  Superclass::PrintSelf(os, indent);
}


/** Print the contour spatial object */
template< unsigned int TDimension >
void
ContourSpatialObject< TDimension >
::Update()
{
  switch( m_InterpolationMethod )
    {
    case NO_INTERPOLATION:
      this->SetPoints( m_ControlPoints );
      break;
    case EXPLICIT_INTERPOLATION:
      break;
    case BEZIER_INTERPOLATION:
      // TODO: Implement bezier interpolation
      {
      ExceptionObject e(__FILE__);
      e.SetLocation( "ContourSpatialObject:Update()");
      e.SetDescription( "Bezier interpolation type not yet defined." );
      throw e;
      }
      break;
    case LINEAR_INTERPOLATION:
      m_Points.clear();
      auto it = m_ControlPoints.begin();
      while( it != m_ControlPoints.end() )
        {
        auto it2 = ++it;
        if( it2 == m_ControlPoints.end() )
          {
          if( this->GetIsClosed() )
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
        for( unsigned int d=0; d<ObjectDimension; ++d )
          {
          step[d] = (pnt2[d] - pnt[d]) / m_InterpolationFactor;
          }
        PointType newPoint;
        for( unsigned int i=0; i<m_InterpolationFactor; ++i )
          {
          for( unsigned int d=0; d<ObjectDimension; ++d )
            {
            newPoint = pnt[d] + i * step[d];
            }
          }
        SpatialObjectPointType newSOPoint;
        newSOPoint = (*it);
        newSOPoint.SetPositionInWorldSpace( newPoint );
        m_Points.push_back( newSOPoint );
        }
      break;
    };

  // Call this last to compute MyBoundingBoxInWorldSpace
  Superclass::Update();
}

} // end namespace itk

#endif
