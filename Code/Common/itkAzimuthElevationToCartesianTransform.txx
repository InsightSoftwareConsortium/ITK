/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAzimuthElevationToCartesianTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkAzimuthElevationToCartesianTransform_txx
#define _itkAzimuthElevationToCartesianTransform_txx

#include "itkAzimuthElevationToCartesianTransform.h"

namespace itk
{

// Constructor with default arguments
template<class TScalarType, unsigned int NDimensions>
AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::
AzimuthElevationToCartesianTransform()
// add this construction call when deriving from itk::Transform
// :Superclass(SpaceDimension,ParametersDimension)
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
template<class TScalarType, unsigned int NDimensions>
AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::
~AzimuthElevationToCartesianTransform()
{
  return;
}


// Print self
template<class TScalarType, unsigned int NDimensions>
void
AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "x = z*tan(Azimuth)"  <<std::endl;
  os << indent << "y = z*tan(Elevation)"  <<std::endl;
  os << indent << "z = sqrt(r*r * cos(Azimuth)*cos(Azimuth) " 
     << " / (1 + cos(Azimuth) * cos(Azimuth) * tan(Elevation) * tan(Elevation)))"  <<std::endl;
  os << indent << "Azimuth = 1 / (tan(x/y))"  <<std::endl;
  os << indent << "Elevation = 1 / (tan(y/z))"  <<std::endl;
  os << indent << "r = sqrt(x*x + y*y + z*z)"  <<std::endl;
  os << indent << "m_MaxAzimuth = "<<m_MaxAzimuth<<std::endl;
  os << indent << "m_MaxElevation = "<<m_MaxElevation<<std::endl;
  os << indent << "m_RadiusSampleSize = "<<m_RadiusSampleSize<<std::endl;
  os << indent << "m_AzimuthAngularSeparation = "<<m_AzimuthAngularSeparation<<std::endl;
  os << indent << "m_ElevationAngularSeparation = "<<m_ElevationAngularSeparation<<std::endl;
  os << indent << "m_FirstSampleDistance = "<<m_FirstSampleDistance<<std::endl;
  os << indent << "m_ForwardAzimuthElevationToPhysical = "<< (m_ForwardAzimuthElevationToPhysical ? "True" : "False")<<std::endl;

}


template<class TScalarType, unsigned int NDimensions>
typename AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::OutputPointType
AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::
TransformPoint(const InputPointType &point) const 
{
  OutputPointType result;
  if (m_ForwardAzimuthElevationToPhysical) result = TransformAzElToCartesian(point);
  else result = TransformCartesianToAzEl(point);
  return result;
}


// Transform a point, from azimuth-elevation to cartesian
template<class TScalarType, unsigned int NDimensions>
typename AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::OutputPointType
AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::
TransformAzElToCartesian(const InputPointType &point) const 
{
  OutputPointType result;
  ScalarType Azimuth = ((2*vnl_math::pi) / 360) * (point[0]*m_AzimuthAngularSeparation 
                                                   - ((m_MaxAzimuth-1)/2.0) );
  ScalarType Elevation   = ((2*vnl_math::pi) / 360) * (point[1]*m_ElevationAngularSeparation 
                                                       - ((m_MaxElevation-1)/2.0) );
  ScalarType r = (m_FirstSampleDistance + point[2]) * m_RadiusSampleSize;

  ScalarType cosOfAzimuth = cos(Azimuth);
  ScalarType tanOfElevation = tan(Elevation);
  result[2] = sqrt((r*r*cosOfAzimuth*cosOfAzimuth)
                   / (1 + cosOfAzimuth * cosOfAzimuth * tanOfElevation * tanOfElevation));
  result[0] = result[2] * tan(Azimuth);
  result[1] = result[2] * tanOfElevation;
  return result;
}
    


// Back transform a point
template<class TScalarType, unsigned int NDimensions>
typename AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::InputPointType
AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::
BackTransform(const OutputPointType &point) const 
{
  InputPointType result; 
  if( m_ForwardAzimuthElevationToPhysical ) 
    {
    result = static_cast<InputPointType>(TransformCartesianToAzEl(point));
    }
  else 
    {
    result = static_cast<InputPointType>(TransformAzElToCartesian(point));
    }
  return result;
}



// Back transform a point
template<class TScalarType, unsigned int NDimensions>
typename AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::InputPointType
AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::
BackTransformPoint(const OutputPointType &point) const 
{
  OutputPointType result; 
  if (m_ForwardAzimuthElevationToPhysical) 
    {
    result = static_cast<InputPointType>( TransformCartesianToAzEl(point) );
    }
  else
    {
    result = static_cast<InputPointType>( TransformAzElToCartesian(point) );
    }
  return result;
}




template<class TScalarType, unsigned int NDimensions>
typename AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::OutputPointType
AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::
TransformCartesianToAzEl(const OutputPointType &point) const 
{
  InputPointType result;       // Converted point
  result[0] = (atan(point[0] / point[2])) * (360 / (2*vnl_math::pi)) + ((m_MaxAzimuth-1)/2.0);
  result[1] = (atan(point[1] / point[2])) * (360 / (2*vnl_math::pi)) + ((m_MaxElevation-1)/2.0);
  result[2] = ((sqrt( point[0] * point[0] +
                      point[1] * point[1] +
                      point[2] * point[2]) 
                / m_RadiusSampleSize)
               - m_FirstSampleDistance);
  return result;
}


// Set parameters
template<class TScalarType, unsigned int NDimensions>
void
AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::
SetAzimuthElevationToCartesianParameters(const double sampleSize, 
                                         const double firstSampleDistance,
                                         const long maxAzimuth, 
                                         const long maxElevation,
                                         const double azimuthAngleSeparation,
                                         const double elevationAngleSeparation)
{
  m_MaxAzimuth = static_cast<long>(static_cast<double>(maxAzimuth) * azimuthAngleSeparation);
  m_MaxElevation = static_cast<long>(static_cast<double>(maxElevation) * elevationAngleSeparation);
  m_RadiusSampleSize = sampleSize;
  m_AzimuthAngularSeparation = azimuthAngleSeparation;
  m_ElevationAngularSeparation = elevationAngleSeparation;
  m_FirstSampleDistance = firstSampleDistance / sampleSize;
}

template<class TScalarType, unsigned int NDimensions>
void
AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::
SetAzimuthElevationToCartesianParameters(const double sampleSize, 
                                         const double firstSampleDistance,
                                         const long maxAzimuth, 
                                         const long maxElevation )
{
  SetAzimuthElevationToCartesianParameters(sampleSize, firstSampleDistance, maxAzimuth, maxElevation, 1.0, 1.0);
}



template<class TScalarType, unsigned int NDimensions>
void
AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::
SetForwardAzimuthElevationToCartesian()
{
  m_ForwardAzimuthElevationToPhysical = true;
}



template<class TScalarType, unsigned int NDimensions>
void
AzimuthElevationToCartesianTransform<TScalarType, NDimensions>::
SetForwardCartesianToAzimuthElevation()
{
  m_ForwardAzimuthElevationToPhysical = false;
}



}//namespace
#endif
