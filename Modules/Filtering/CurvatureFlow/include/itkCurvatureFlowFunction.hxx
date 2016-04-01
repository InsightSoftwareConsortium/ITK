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
#ifndef itkCurvatureFlowFunction_hxx
#define itkCurvatureFlowFunction_hxx
#include "itkCurvatureFlowFunction.h"

#include "itkMath.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TImage >
CurvatureFlowFunction< TImage >
::CurvatureFlowFunction()
{
  RadiusType   r;
  unsigned int j;

  for ( j = 0; j < ImageDimension; j++ )
    {
    r[j] = 1;
    }

  this->SetRadius(r);

  m_TimeStep  = 0.05f;
}

/**
 * Compute the global time step
 */
template< typename TImage >
typename CurvatureFlowFunction< TImage >::TimeStepType
CurvatureFlowFunction< TImage >
::ComputeGlobalTimeStep( void *itkNotUsed(gd) ) const
{
  return this->GetTimeStep();
}

/**
 * Update the solution at pixels which lies on the data boundary.
 */
template< typename TImage >
typename CurvatureFlowFunction< TImage >::PixelType
CurvatureFlowFunction< TImage >
::ComputeUpdate( const NeighborhoodType & it, void *itkNotUsed(gd),
                 const FloatOffsetType & itkNotUsed(offset) )
{
  PixelRealType firstderiv[ImageDimension];
  PixelRealType secderiv[ImageDimension];
  PixelRealType crossderiv[ImageDimension][ImageDimension] = {};
  IdentifierType center;
  IdentifierType stride[ImageDimension];
  unsigned int  i, j;

  const NeighborhoodScalesType neighborhoodScales = this->ComputeNeighborhoodScales();

  // get the center pixel position
  center = it.Size() / 2;

  // cache the stride for each dimension
  for ( i = 0; i < ImageDimension; i++ )
    {
    stride[i] = it.GetStride( (IdentifierType)i );
    }

  PixelRealType magnitudeSqr = 0.0;
  for ( i = 0; i < ImageDimension; i++ )
    {
    // compute first order derivatives
    firstderiv[i] = 0.5 * ( it.GetPixel(center + stride[i])
                            - it.GetPixel(center - stride[i]) ) * neighborhoodScales[i];

    // compute second order derivatives
    secderiv[i] = ( it.GetPixel(center + stride[i])
                    - 2 * it.GetPixel(center) + it.GetPixel(center - stride[i]) ) * itk::Math::sqr(neighborhoodScales[i]);

    // compute cross derivatives
    for ( j = i + 1; j < ImageDimension; j++ )
      {
      crossderiv[i][j] = 0.25 * (
        it.GetPixel(center - stride[i] - stride[j])
        - it.GetPixel(center - stride[i] + stride[j])
        - it.GetPixel(center + stride[i] - stride[j])
        + it.GetPixel(center + stride[i] + stride[j]) )
                         * neighborhoodScales[i] * neighborhoodScales[j];
      }

    // accumlate the gradient magnitude squared
    magnitudeSqr += itk::Math::sqr( (double)firstderiv[i] );
    }

  if ( magnitudeSqr < 1e-9 )
    {
    return NumericTraits< PixelType >::ZeroValue();
    }

  // compute the update value = mean curvature * magnitude
  PixelRealType update = 0.0;
  PixelRealType temp;

  // accumulate dx^2 * (dyy + dzz) terms
  for ( i = 0; i < ImageDimension; i++ )
    {
    temp = 0.0;
    for ( j = 0; j < ImageDimension; j++ )
      {
      if ( j == i ) { continue; }
      temp += secderiv[j];
      }

    update += temp * itk::Math::sqr( (double)firstderiv[i] );
    }

  // accumlate -2 * dx * dy * dxy terms
  for ( i = 0; i < ImageDimension; i++ )
    {
    for ( j = i + 1; j < ImageDimension; j++ )
      {
      update -= 2 * firstderiv[i] * firstderiv[j]
                * crossderiv[i][j];
      }
    }

  update /= magnitudeSqr;
  return static_cast< PixelType >( update );
}
} // end namespace itk

#endif
