/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRT3DTransform.txx
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
#ifndef _itkRT3DTransform_txx
#define _itkRT3DTransform_txx

#include "itkRT3DTransform.h"

namespace itk
{

// Constructor with default arguments
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
RT3DTransform()
{
  m_MaxTheta = 0;
  m_MaxPhi = 0;
  m_RadiusSampleSize = 1;
  m_AzimuthAngularSeparation = 1;
  m_ElevationAngularSeparation = 1;
  m_FirstSampleDistance = 0;
  m_ForwardIsIndexToPhysical = true;

}

// Destructor
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
~RT3DTransform()
{
  return;
}


// Print self
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "x = z*tan(theta)"  <<std::endl;
  os << indent << "y = z*tan(phi)"  <<std::endl;
  os << indent << "z = sqrt(r*r * cos(theta)*cos(theta) / (1 + cos(theta) * cos(theta) * tan(phi) * tan(phi)))"  <<std::endl;
  os << indent << "theta = 1 / (tan(x/y))"  <<std::endl;
  os << indent << "phi = 1 / (tan(y/z))"  <<std::endl;
  os << indent << "r = sqrt(x*x + y*y + z*z)"  <<std::endl;
}


template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::OutputPointType
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
TransformPoint(const InputPointType &point) const 
{
  OutputPointType result;
  if (m_ForwardIsIndexToPhysical) result = TransformAzElToCartesian(point);
  else result = TransformCartesianToAzEl(point);
  return result;
}


// Transform a point, from azimuth-elevation to cartesian
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::OutputPointType
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
TransformAzElToCartesian(const InputPointType &point) const 
{
  OutputPointType result;
  ScalarType theta = ((2*vnl_math::pi) / 360) * (point[0] - ((m_MaxTheta-1)/2.0) );
  ScalarType phi   = ((2*vnl_math::pi) / 360) * (point[1] - ((m_MaxPhi-1)/2.0) );
  ScalarType r = (m_FirstSampleDistance + point[2]) * m_RadiusSampleSize;

  ScalarType cosOfTheta = cos(theta);
  ScalarType tanOfPhi = tan(phi);
  result[2] = sqrt((r*r*cosOfTheta*cosOfTheta)
                   / (1 + cosOfTheta * cosOfTheta * tanOfPhi * tanOfPhi));
  result[0] = result[2] * tan(theta);
  result[1] = result[2] * tanOfPhi;
  return result;
}
    


// Back transform a point
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::InputPointType
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
BackTransform(const OutputPointType &point) const 
{
  InputPointType result; 
  if( m_ForwardIsIndexToPhysical ) 
    {
    result = static_cast<InputPointType>(TransformCartesianToAzEl(point));
    }
  else 
    {
    result = static_cast<InputPointType>(TransformAzElToCartesian(point));
    }
}



// Back transform a point, from cartesian to azimuth-elevation
template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::InputPointType
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
BackTransformPoint(const OutputPointType &point) const 
{
  OutputPointType result; 
  if (m_ForwardIsIndexToPhysical) 
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
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::OutputPointType
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
TransformCartesianToAzEl(const OutputPointType &point) const 
{
  InputPointType result;       // Converted point
  result[0] = (atan(point[0] / point[2])) * (360 / (2*vnl_math::pi)) + ((m_MaxTheta-1)/2.0);
  result[1] = (atan(point[1] / point[2])) * (360 / (2*vnl_math::pi)) + ((m_MaxPhi-1)/2.0);
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
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
SetRT3DParameters(const double sampleSize, 
                  const double blanking,
                  const long maxTheta, 
                  const long maxPhi,
                  const double azimuthAngleSeparation,
                  const double elevationAngleSeparation)
{
  m_MaxPhi = maxPhi;
  m_MaxTheta = maxTheta;
  m_RadiusSampleSize = sampleSize;
  m_AzimuthAngularSeparation = azimuthAngleSeparation;
  m_ElevationAngularSeparation = elevationAngleSeparation;
  m_Blanking = blanking;
  m_FirstSampleDistance = blanking/sampleSize;
}

template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
SetRT3DParameters(const double sampleSize, 
                  const double blanking,
                  const long maxTheta, 
                  const long maxPhi )
{
  SetRT3DParameters(sampleSize, blanking, maxTheta, maxPhi, 1, 1);
}



template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
SetForwardIsIndexToPhysical()
{
  m_ForwardIsIndexToPhysical = true;
}



template<class TScalarType, unsigned int NDimensions,
         class TParameters, class TJacobianType >
void
RT3DTransform<TScalarType, NDimensions,TParameters,TJacobianType>::
SetForwardIsPhysicalToIndex()
{
  m_ForwardIsIndexToPhysical = false;
}



}//namespace
#endif
