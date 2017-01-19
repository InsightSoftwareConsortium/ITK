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
#ifndef itkFrustumSpatialFunction_hxx
#define itkFrustumSpatialFunction_hxx

#include "itkFrustumSpatialFunction.h"

namespace itk
{
template< unsigned int VDimension, typename TInput >
FrustumSpatialFunction< VDimension, TInput >::FrustumSpatialFunction() :
  m_AngleZ( 0.0f ),
  m_ApertureAngleX( 0.0f ),
  m_ApertureAngleY( 0.0f ),
  m_TopPlane( 0.0f ),
  m_BottomPlane( 0.0f ),
  m_RotationPlane( RotateInXZPlane )
{
  m_Apex.Fill( 0.0f );
}

template< unsigned int VDimension, typename TInput >
FrustumSpatialFunction< VDimension, TInput >::~FrustumSpatialFunction()
{}

template< unsigned int VDimension, typename TInput >
typename FrustumSpatialFunction< VDimension, TInput >::OutputType
FrustumSpatialFunction< VDimension, TInput >
::Evaluate(const InputType & position) const
{
  typedef InputType                      PointType;
  typedef typename PointType::VectorType VectorType;

  VectorType   relativePosition = position - m_Apex;
  const double distanceToApex = relativePosition.GetNorm();

  // Check Top and Bottom planes. If the angle is negative, the top plane
  // value may be less than the bottom plane, which is still correct.
  if ( m_TopPlane <= m_BottomPlane )
    {
    if ( distanceToApex < m_TopPlane
         || distanceToApex > m_BottomPlane )
      {
      return 0;
      }
    }
  else
    {
    if ( distanceToApex > m_TopPlane
         || distanceToApex < m_BottomPlane )
      {
      return 0;
      }
    }

  if ( m_RotationPlane == RotateInXZPlane )
    {
    const double dx = relativePosition[0];
    const double dy = relativePosition[1];
    const double dz = relativePosition[2];

    const double distanceXZ = std::sqrt(dx * dx + dz * dz);

    const double deg2rad = std::atan(1.0) / 45.0;

    // Check planes along Y
    const double angleY = std::atan2(dy, distanceXZ);
    if ( std::fabs(angleY) > m_ApertureAngleY * deg2rad )
      {
      return 0;
      }

    // Check planes along X
    const double angleX = std::atan2(dx, dz);

    if ( std::cos(angleX  + ( 180.0 + m_AngleZ ) * deg2rad)  <
         std::cos(deg2rad * m_ApertureAngleX) )
      {
      return 0;
      }

    return 1;
    }
  else if ( m_RotationPlane == RotateInYZPlane )
    {
    const double dx = relativePosition[0];
    const double dy = relativePosition[1];
    const double dz = relativePosition[2];

    const double distanceYZ = std::sqrt(dy * dy + dz * dz);

    const double deg2rad = std::atan(1.0) / 45.0;

    // Check planes along X
    const double angleX = std::atan2(dx, distanceYZ);
    if ( std::fabs(angleX) > m_ApertureAngleX * deg2rad )
      {
      return 0;
      }

    // Check planes along Y
    const double angleY = std::atan2(dy, dz);

    if ( std::cos(angleY  + ( 180.0 + m_AngleZ ) * deg2rad)  <
         std::cos(deg2rad * m_ApertureAngleY) )
      {
      return 0;
      }

    return 1;
    }
  else
    {
    itkExceptionMacro(<< "Rotation plane not set or set to an unsupported value!");
    }
  return 0;
}

template< unsigned int VDimension, typename TInput >
void
FrustumSpatialFunction< VDimension, TInput >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Apex: " << m_Apex << std::endl;
  os << indent << "AngleZ: " << m_AngleZ << std::endl;
  os << indent << "ApertureAngleX: " << m_ApertureAngleX << std::endl;
  os << indent << "ApertureAngleY: " << m_ApertureAngleY << std::endl;
  os << indent << "TopPlane: " << m_TopPlane << std::endl;
  os << indent << "BottomPlane: " << m_BottomPlane << std::endl;
  os << indent << "RotationPlane: " << m_RotationPlane << std::endl;
}
} // end namespace itk

#endif
