/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkCurvatureFlowFunction.txx
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
#ifndef __itkCurvatureFlowFunction_txx_
#define __itkCurvatureFlowFunction_txx_

#include "vnl/vnl_math.h"

namespace itk {

/**
 * Constructor
 */
template<class TImage>
CurvatureFlowFunction<TImage>
::CurvatureFlowFunction()
{

  RadiusType r;
  int j;
  for( j = 0; j < ImageDimension; j++ )
    {
    r[j] = 1;
    }

  this->SetRadius(r);

  m_TimeStep  = 0.125f;

}


/**
 * Update the solution at pixels which does not lie on the
 * data boundary.
 */
template<class TImage>
CurvatureFlowFunction<TImage>::PixelType
CurvatureFlowFunction<TImage>
::ComputeUpdate(const NeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{
  
  PixelType firstderiv[ImageDimension];
  PixelType secderiv[ImageDimension];
  PixelType crossderiv[ImageDimension][ImageDimension];
  unsigned long center;
  unsigned long stride[ImageDimension];
  int i,j;

  // get the center pixel position
  center = it.Size() / 2;

  // cache the stride for each dimension
  for( i = 0; i < ImageDimension; i++ )
    {
    stride[i] = it.GetStride( (unsigned long) i );
    }

  PixelType magnitude = 0.0;
  for( i = 0; i < ImageDimension; i++ )
    {

    // compute first order derivatives
    firstderiv[i] = 0.5 * ( it.GetPixel(center + stride[i]) - 
      it.GetPixel(center - stride[i]) );

    // compute second order derivatives
    secderiv[i] = it.GetPixel(center + stride[i]) - 
      2 * it.GetPixel(center) + it.GetPixel( center - stride[i] );

    // compute cross derivatives
    for( j = i + 1; j < ImageDimension; j++ )
      {
      crossderiv[i][j] = 0.25 * (
           it.GetPixel( center - stride[i] - stride[j] ) 
         - it.GetPixel( center - stride[i] + stride[j] )
         - it.GetPixel( center + stride[i] - stride[j] ) 
         + it.GetPixel( center + stride[i] + stride[j] ) );
      }

    // accumlate the gradient magnitude
    magnitude += vnl_math_sqr( firstderiv[i] );

    }

  // compute the magnitude
  magnitude = vnl_math_sqrt( magnitude );
  if ( magnitude < 1e-9 )
    {
    return NumericTraits<PixelType>::Zero; 
    }

  // compute the update value = curvature * magnitude
  PixelType update = 0.0;
  PixelType temp;

  // accumulate dx^2 * (dyy + dzz) terms
  for( i = 0; i < ImageDimension; i++ )
    {
    temp = 0.0;
    for( j = 0; j < ImageDimension; j++ )
      {
        if( j == i ) continue;
        temp += secderiv[j];
      }
    
    update += temp * vnl_math_sqr( firstderiv[i] );
    }

  // accumlate -2 * dx * dy * dxy terms
  for( i = 0; i < ImageDimension; i++ )
    {
    for( j = i + 1; j < ImageDimension; j++ )
      {
      update -= 2 * firstderiv[i] * firstderiv[j] *
        crossderiv[i][j];
      }
    }
   
  update /= vnl_math_sqr( magnitude );

  return update;

}


/**
 * Update the solution at pixels which lies on the data boundary.
 */
template<class TImage>
CurvatureFlowFunction<TImage>::PixelType
CurvatureFlowFunction<TImage>
::ComputeUpdate(const BoundaryNeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{

  PixelType firstderiv[ImageDimension];
  PixelType secderiv[ImageDimension];
  PixelType crossderiv[ImageDimension][ImageDimension];
  unsigned long center;
  unsigned long stride[ImageDimension];
  int i,j;

  // get the center pixel position
  center = it.Size() / 2;

  // cache the stride for each dimension
  for( i = 0; i < ImageDimension; i++ )
    {
    stride[i] = it.GetStride( (unsigned long) i );
    }

  PixelType magnitude = 0.0;
  for( i = 0; i < ImageDimension; i++ )
    {

    // compute first order derivatives
    firstderiv[i] = 0.5 * ( it.GetPixel(center + stride[i]) - 
      it.GetPixel(center - stride[i]) );

    // compute second order derivatives
    secderiv[i] = it.GetPixel(center + stride[i]) - 
      2 * it.GetPixel(center) + it.GetPixel( center - stride[i] );

    // compute cross derivatives
    for( j = i + 1; j < ImageDimension; j++ )
      {
      crossderiv[i][j] = 0.25 * (
           it.GetPixel( center - stride[i] - stride[j] ) 
         - it.GetPixel( center - stride[i] + stride[j] )
         - it.GetPixel( center + stride[i] - stride[j] ) 
         + it.GetPixel( center + stride[i] + stride[j] ) );
      }

    // accumlate the gradient magnitude
    magnitude += vnl_math_sqr( firstderiv[i] );

    }

  // compute the magnitude
  magnitude = vnl_math_sqrt( magnitude );
  if ( magnitude < 1e-9 )
    {
    return NumericTraits<PixelType>::Zero; 
    }

  // compute the update value = curvature * magnitude
  PixelType update = 0.0;
  PixelType temp;

  // accumulate dx^2 * (dyy + dzz) terms
  for( i = 0; i < ImageDimension; i++ )
    {
    temp = 0.0;
    for( j = 0; j < ImageDimension; j++ )
      {
        if( j == i ) continue;
        temp += secderiv[j];
      }
    
    update += temp * vnl_math_sqr( firstderiv[i] );
    }

  // accumlate -2 * dx * dy * dxy terms
  for( i = 0; i < ImageDimension; i++ )
    {
    for( j = i + 1; j < ImageDimension; j++ )
      {
      update -= 2 * firstderiv[i] * firstderiv[j] *
        crossderiv[i][j];
      }
    }
   
  update /= vnl_math_sqr( magnitude );

  return update;

}

} // end namespace itk

#endif
