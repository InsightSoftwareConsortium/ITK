/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAzimuthElevationToCartesianTransform.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkAzimuthElevationToCartesianTransform_txx
#define _itkAzimuthElevationToCartesianTransform_txx

#include "itkAzimuthElevationToCartesianTransform.h"

namespace itk
{

// Constructor with default arguments
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
AzimuthElevationToCartesianTransform()
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
~AzimuthElevationToCartesianTransform()
{
  return;
}


// Print self
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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


template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::OutputPointType
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
TransformPoint(const InputPointType &point) const 
{
  OutputPointType result;
  if (m_ForwardAzimuthElevationToPhysical) result = TransformAzElToCartesian(point);
  else result = TransformCartesianToAzEl(point);
  return result;
}


// Transform a point, from azimuth-elevation to cartesian
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::OutputPointType
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::InputPointType
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::InputPointType
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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




template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::OutputPointType
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
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

template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
SetAzimuthElevationToCartesianParameters(const double sampleSize, 
                  const double firstSampleDistance,
                  const long maxAzimuth, 
                  const long maxElevation )
{
  SetAzimuthElevationToCartesianParameters(sampleSize, firstSampleDistance, maxAzimuth, maxElevation, 1.0, 1.0);
}



template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
SetForwardAzimuthElevationToCartesian()
{
  m_ForwardAzimuthElevationToPhysical = true;
}



template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
AzimuthElevationToCartesianTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
SetForwardCartesianToAzimuthElevation()
{
  m_ForwardAzimuthElevationToPhysical = false;
}



}//namespace
#endif
