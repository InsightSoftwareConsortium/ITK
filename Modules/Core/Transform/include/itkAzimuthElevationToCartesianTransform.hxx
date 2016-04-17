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
#ifndef itkAzimuthElevationToCartesianTransform_hxx
#define itkAzimuthElevationToCartesianTransform_hxx

#include "itkAzimuthElevationToCartesianTransform.h"

namespace itk
{
// Constructor with default arguments
template<typename TParametersValueType, unsigned int NDimensions>
AzimuthElevationToCartesianTransform<TParametersValueType, NDimensions>
::AzimuthElevationToCartesianTransform()
// add this construction call when deriving from itk::Transform
// :Superclass(ParametersDimension)
{
  m_MaxAzimuth = 0;
  m_MaxElevation = 0;
  m_RadiusSampleSize = 1;
  m_AzimuthAngularSeparation = 1;
  m_ElevationAngularSeparation = 1;
  m_FirstSampleDistance = 0;
  m_ForwardAzimuthElevationToPhysical = true;
}

// Destructor
template<typename TParametersValueType, unsigned int NDimensions>
AzimuthElevationToCartesianTransform<TParametersValueType, NDimensions>::
~AzimuthElevationToCartesianTransform()
{
}

// Print self
template<typename TParametersValueType, unsigned int NDimensions>
void
AzimuthElevationToCartesianTransform<TParametersValueType, NDimensions>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "x = z*tan(Azimuth)" << std::endl;
  os << indent << "y = z*tan(Elevation)" << std::endl;
  os << indent << "z = r * cos(Azimuth) "
     << " / sqrt((1 + cos(Azimuth) * cos(Azimuth) * tan(Elevation)"
     << "* tan(Elevation)))" << std::endl;
  os << indent << "Azimuth = 1 / (tan(x/z))" << std::endl;
  os << indent << "Elevation = 1 / (tan(y/z))" << std::endl;
  os << indent << "r = sqrt(x*x + y*y + z*z)" << std::endl;
  os << indent << "m_MaxAzimuth = " << m_MaxAzimuth << std::endl;
  os << indent << "m_MaxElevation = " << m_MaxElevation << std::endl;
  os << indent << "m_RadiusSampleSize = " << m_RadiusSampleSize << std::endl;
  os << indent << "m_AzimuthAngularSeparation = ";
  os << indent << m_AzimuthAngularSeparation << std::endl;
  os << indent << "m_ElevationAngularSeparation = ";
  os << indent << m_ElevationAngularSeparation << std::endl;
  os << indent << "m_FirstSampleDistance = ";
  os << indent << m_FirstSampleDistance << std::endl;
  os << indent << "m_ForwardAzimuthElevationToPhysical = ";
  os << indent << ( m_ForwardAzimuthElevationToPhysical ? "True" : "False" );
  os << indent << std::endl;
}

template<typename TParametersValueType, unsigned int NDimensions>
typename AzimuthElevationToCartesianTransform<TParametersValueType, NDimensions>
::OutputPointType
AzimuthElevationToCartesianTransform<TParametersValueType, NDimensions>::TransformPoint(const InputPointType & point) const
{
  OutputPointType result;

  if ( m_ForwardAzimuthElevationToPhysical )
    {
    result = TransformAzElToCartesian(point);
    }
  else
    {
    result = TransformCartesianToAzEl(point);
    }
  return result;
}

/** Transform a point, from azimuth-elevation to cartesian */
template<typename TParametersValueType, unsigned int NDimensions>
typename AzimuthElevationToCartesianTransform<TParametersValueType, NDimensions>
::OutputPointType
AzimuthElevationToCartesianTransform<TParametersValueType,
                                      NDimensions >
                                      ::TransformAzElToCartesian(const InputPointType & point) const
{
  OutputPointType result;
  ScalarType Azimuth = ( ( 2 * itk::Math::pi ) / 360 )
                            * ( point[0] * m_AzimuthAngularSeparation
                                - ( ( m_MaxAzimuth - 1 ) / 2.0 ) );
  ScalarType Elevation = ( ( 2 * itk::Math::pi ) / 360 )
                           * ( point[1] * m_ElevationAngularSeparation
                               - ( ( m_MaxElevation - 1 ) / 2.0 ) );
  ScalarType r = ( m_FirstSampleDistance + point[2] ) * m_RadiusSampleSize;

  ScalarType cosOfAzimuth = std::cos(Azimuth);
  ScalarType tanOfElevation = std::tan(Elevation);

  result[2] = (r * cosOfAzimuth)
               / std::sqrt(( 1 + cosOfAzimuth * cosOfAzimuth * tanOfElevation
                            * tanOfElevation ) );
  result[0] = result[2] * std::tan(Azimuth);
  result[1] = result[2] * tanOfElevation;
  return result;
}

template<typename TParametersValueType, unsigned int NDimensions>
typename AzimuthElevationToCartesianTransform<TParametersValueType, NDimensions>
::OutputPointType
AzimuthElevationToCartesianTransform<TParametersValueType, NDimensions>::TransformCartesianToAzEl(
  const OutputPointType & point) const
{
  InputPointType result;       // Converted point

  result[0] = std::atan2(point[0], point[2]) * (360 / (2 * itk::Math::pi))
            + ((m_MaxAzimuth - 1) / 2.0);
  result[1] = std::atan2(point[1], point[2]) * (360 / (2 * itk::Math::pi))
            + ((m_MaxElevation - 1) / 2.0);
  result[2] = ( ( std::sqrt(point[0] * point[0]
                           + point[1] * point[1]
                           + point[2] * point[2]) / m_RadiusSampleSize )
                - m_FirstSampleDistance );
  return result;
}

// Set parameters
template<typename TParametersValueType, unsigned int NDimensions>
void
AzimuthElevationToCartesianTransform<TParametersValueType, NDimensions>::SetAzimuthElevationToCartesianParameters(
  const double sampleSize,
  const double
  firstSampleDistance,
  const long
  maxAzimuth,
  const long
  maxElevation,
  const double
  azimuthAngleSeparation,
  const double
  elevationAngleSeparation)
{
  SetMaxAzimuth( static_cast< long >( static_cast< double >( maxAzimuth )
                                      * azimuthAngleSeparation ) );
  SetMaxElevation( static_cast< long >( static_cast< double >( maxElevation )
                                        * elevationAngleSeparation ) );
  SetRadiusSampleSize(sampleSize);
  SetAzimuthAngularSeparation(azimuthAngleSeparation);
  SetElevationAngularSeparation(elevationAngleSeparation);
  SetFirstSampleDistance(firstSampleDistance / sampleSize);
}

template<typename TParametersValueType, unsigned int NDimensions>
void
AzimuthElevationToCartesianTransform<TParametersValueType, NDimensions>::SetAzimuthElevationToCartesianParameters(
  const double sampleSize,
  const double
  firstSampleDistance,
  const long
  maxAzimuth,
  const long
  maxElevation)
{
  SetAzimuthElevationToCartesianParameters(sampleSize, firstSampleDistance,
                                           maxAzimuth, maxElevation, 1.0, 1.0);
}

template<typename TParametersValueType, unsigned int NDimensions>
void
AzimuthElevationToCartesianTransform<TParametersValueType, NDimensions>::SetForwardAzimuthElevationToCartesian()
{
  m_ForwardAzimuthElevationToPhysical = true;
}

template<typename TParametersValueType, unsigned int NDimensions>
void
AzimuthElevationToCartesianTransform<TParametersValueType, NDimensions>::SetForwardCartesianToAzimuthElevation()
{
  m_ForwardAzimuthElevationToPhysical = false;
}
} //namespace
#endif
