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
#ifndef itkMinMaxCurvatureFlowFunction_hxx
#define itkMinMaxCurvatureFlowFunction_hxx
#include "itkMinMaxCurvatureFlowFunction.h"

#include "itkMath.h"
#include "itkNeighborhoodInnerProduct.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TImage >
MinMaxCurvatureFlowFunction< TImage >
::MinMaxCurvatureFlowFunction()
{
  m_StencilRadius = 0;
  this->SetStencilRadius(2);
}

/**
 * Set the stencil radius.
 */
template< typename TImage >
void
MinMaxCurvatureFlowFunction< TImage >
::SetStencilRadius(const RadiusValueType value)
{
  if ( m_StencilRadius == value ) { return; }

  m_StencilRadius = ( value > 1 ) ? value :  1;
  RadiusType   radius;
  unsigned int j;

  for ( j = 0; j < ImageDimension; j++ )
    {
    radius[j] = m_StencilRadius;
    }

  this->SetRadius(radius);
  this->InitializeStencilOperator();
}

/**
 * Initialize the stencil operator.
 */
template< typename TImage >
void
MinMaxCurvatureFlowFunction< TImage >
::InitializeStencilOperator()
{
  // Fill stencil operator with a sphere of radius m_StencilRadius.

  m_StencilOperator.SetRadius(m_StencilRadius);

  RadiusValueType counter[ImageDimension];
  unsigned int    j;
  RadiusValueType span = 2 * m_StencilRadius + 1;
  RadiusValueType sqrRadius = m_StencilRadius * m_StencilRadius;
  for ( j = 0; j < ImageDimension; j++ )
    {
    counter[j] = 0;
    }

  typedef typename StencilOperatorType::Iterator Iterator;
  Iterator opIter;
  Iterator opEnd  = m_StencilOperator.End();

  SizeValueType numPixelsInSphere = 0;

  for ( opIter = m_StencilOperator.Begin(); opIter < opEnd; ++opIter )
    {
    *opIter = NumericTraits< PixelType >::ZeroValue();

    RadiusValueType length = 0;
    for ( j = 0; j < ImageDimension; j++ )
      {
      length += static_cast< RadiusValueType >(
        itk::Math::sqr( static_cast< IndexValueType >( counter[j] )
                      - static_cast< IndexValueType >( m_StencilRadius ) ) );
      }
    if ( length <= sqrRadius )
      {
      *opIter = 1;
      numPixelsInSphere++;
      }

    bool carryOver = true;
    for ( j = 0; carryOver && j < ImageDimension; j++ )
      {
      counter[j] += 1;
      carryOver = false;
      if ( counter[j] == span )
        {
        counter[j] = 0;
        carryOver = true;
        }
      }
    }

  // normalize the operator so that it sums to one
  if (numPixelsInSphere != 0)
    {
    for ( opIter = m_StencilOperator.Begin(); opIter < opEnd; ++opIter )
      {
      *opIter = static_cast< PixelType >( (double)*opIter
                                          / (double)numPixelsInSphere );
      }
    }
}

/**
 * Compute the threshold by averaging the image intensity in
 * the direction perpendicular to the image gradient.
 */
template< typename TImage >
typename MinMaxCurvatureFlowFunction< TImage >::PixelType
MinMaxCurvatureFlowFunction< TImage >
::ComputeThreshold(const DispatchBase &,
                   const NeighborhoodType & it) const
{
  PixelType threshold = NumericTraits< PixelType >::ZeroValue();

  // Compute gradient
  PixelType     gradient[ImageDimension];
  PixelType     gradMagnitude;
  SizeValueType stride;
  SizeValueType center;
  unsigned int  j;

  center = it.Size() / 2;

  gradMagnitude = NumericTraits< PixelType >::ZeroValue();
  for ( j = 0; j < ImageDimension; j++ )
    {
    stride = it.GetStride( (SizeValueType)j );
    gradient[j] = 0.5 * ( it.GetPixel(center + stride)
                          - it.GetPixel(center - stride) );
    gradient[j] *= this->m_ScaleCoefficients[j];

    gradMagnitude += itk::Math::sqr( (double)gradient[j] );
    }

  if ( gradMagnitude == 0.0 ) { return threshold; }

  gradMagnitude = std::sqrt( (double)gradMagnitude );

  // Search for all position in the neighborhood perpendicular to
  // the gradient and at a distance of StencilRadius from center.

  RadiusValueType counter[ImageDimension];
  RadiusValueType span = 2 * m_StencilRadius + 1;
  for ( j = 0; j < ImageDimension; j++ )
    {
    counter[j] = 0;
    }

  typedef typename NeighborhoodType::ConstIterator Iterator;
  Iterator neighIter;
  Iterator neighEnd  = it.End();

  SizeValueType i = 0;
  SizeValueType numPixels = 0;

  for ( neighIter = it.Begin(); neighIter < neighEnd; ++neighIter, ++i )
    {
    PixelType dotProduct = NumericTraits< PixelType >::ZeroValue();
    PixelType vectorMagnitude = NumericTraits< PixelType >::ZeroValue();

    for ( j = 0; j <  ImageDimension; j++ )
      {
      IndexValueType diff = static_cast< IndexValueType >( counter[j] )
                         - static_cast< IndexValueType >( m_StencilRadius );

      dotProduct += static_cast< PixelType >( diff ) * gradient[j];
      vectorMagnitude += static_cast< PixelType >( itk::Math::sqr(diff) );
      }

    vectorMagnitude = std::sqrt( (double)vectorMagnitude );

    if ( vectorMagnitude != 0.0 )
      {
      dotProduct /= gradMagnitude * vectorMagnitude;
      }

    if ( vectorMagnitude >= m_StencilRadius && itk::Math::abs(dotProduct) < 0.262 )
      {
      threshold += it.GetPixel(i);
      numPixels++;
      }

    bool carryOver = true;
    for ( j = 0; carryOver && j < ImageDimension; j++ )
      {
      counter[j] += 1;
      carryOver = false;
      if ( counter[j] == span )
        {
        counter[j] = 0;
        carryOver = true;
        }
      }
    }

  if ( numPixels > 0 )
    {
    threshold /= static_cast< PixelType >( numPixels );
    }

  return threshold;
}

/**
 * Compute the threshold by averaging the image intensity in
 * the direction perpendicular to the image gradient.
 */
template< typename TImage >
typename MinMaxCurvatureFlowFunction< TImage >::PixelType
MinMaxCurvatureFlowFunction< TImage >
::ComputeThreshold(const Dispatch< 2 > &, const NeighborhoodType & it) const
{
  const unsigned int imageDimension = 2;

  if ( m_StencilRadius == 0 ) { return it.GetCenterPixel(); }

  PixelType threshold = NumericTraits< PixelType >::ZeroValue();

  // Compute gradient
  double        gradient[imageDimension];
  double        gradMagnitude;
  SizeValueType stride;
  SizeValueType center;
  SizeValueType position[imageDimension];

  center = it.Size() / 2;

  gradient[0] = 0.5 * ( it.GetPixel(center + 1)
                        - it.GetPixel(center - 1) );
  unsigned int k = 0;
  gradient[k] *= this->m_ScaleCoefficients[k];
  gradMagnitude = Math::sqr( gradient[k] );
  k++;

  stride = it.GetStride(1);
  gradient[k] = 0.5 * ( it.GetPixel(center + stride)
                        - it.GetPixel(center - stride) );
  gradient[k] *= this->m_ScaleCoefficients[k];
  gradMagnitude += Math::sqr( gradient[k] );

  if ( gradMagnitude == 0.0 ) { return threshold; }

  gradMagnitude = std::sqrt( (double)gradMagnitude )
                  / static_cast< PixelType >( m_StencilRadius );

  for ( unsigned int j = 0; j < imageDimension; j++ )
    {
    gradient[j] /= gradMagnitude;
    }

  // Compute first perpendicular point
  position[0] = Math::Round< SizeValueType >( (double)( m_StencilRadius - gradient[1] ) );
  position[1] = Math::Round< SizeValueType >( (double)( m_StencilRadius + gradient[0] ) );

  threshold = it.GetPixel(position[0] + stride * position[1]);

  // Compute second perpendicular point
  position[0] = Math::Round< SizeValueType >( (double)( m_StencilRadius + gradient[1] ) );
  position[1] = Math::Round< SizeValueType >( (double)( m_StencilRadius - gradient[0] ) );

  threshold += it.GetPixel(position[0] + stride * position[1]);
  threshold *= 0.5;

  return threshold;
}

/*
 * Compute the threshold by averaging the image intensity in
 * the direction perpendicular to the image gradient.
 */
template< typename TImage >
typename MinMaxCurvatureFlowFunction< TImage >::PixelType
MinMaxCurvatureFlowFunction< TImage >
::ComputeThreshold(const Dispatch< 3 > &, const NeighborhoodType & it) const
{
  const unsigned int imageDimension = 3;

  if ( m_StencilRadius == 0 ) { return it.GetCenterPixel(); }

  PixelType threshold = NumericTraits< PixelType >::ZeroValue();

  // Compute gradient
  double        gradient[imageDimension];
  double        gradMagnitude;
  SizeValueType strideY;
  SizeValueType strideZ;
  SizeValueType center;
  SizeValueType position[imageDimension];

  center  = it.Size() / 2;
  strideY = it.GetStride(1);
  strideZ = it.GetStride(2);

  gradient[0] = 0.5 * ( it.GetPixel(center + 1)
                        - it.GetPixel(center - 1) );
  unsigned int k = 0;
  gradient[k] *= this->m_ScaleCoefficients[k];
  gradMagnitude = itk::Math::sqr( gradient[k] );
  k++;

  gradient[k] = 0.5 * ( it.GetPixel(center + strideY)
                        - it.GetPixel(center - strideY) );
  gradient[k] *= this->m_ScaleCoefficients[k];
  gradMagnitude += itk::Math::sqr( gradient[k] );
  k++;

  gradient[k] = 0.5 * ( it.GetPixel(center + strideZ)
                        - it.GetPixel(center - strideZ) );
  gradient[k] *= this->m_ScaleCoefficients[k];
  gradMagnitude += Math::sqr( gradient[k] );

  if ( gradMagnitude == 0.0 ) { return threshold; }

  gradMagnitude = std::sqrt( gradMagnitude )
                  / static_cast< PixelType >( m_StencilRadius );

  for ( unsigned int j = 0; j < imageDimension; ++j )
    {
    gradient[j] /= gradMagnitude;
    }

  double theta;
  double phi;
  if ( gradient[2] > 1.0 )
    {
    gradient[2] = 1.0;
    }
  if ( gradient[2] < -1.0 )
    {
    gradient[2] = -1.0;
    }
  theta = std::acos( gradient[2] );

  if ( Math::AlmostEquals(gradient[0], NumericTraits< PixelType >::ZeroValue()) )
    {
    phi = Math::pi * 0.5;
    }
  else
    {
    phi = std::atan( gradient[1] / gradient[0] );
    }

  double cosTheta = std::cos(theta);
  double sinTheta = std::sin(theta);
  double cosPhi   = std::cos(phi);
  double sinPhi   = std::sin(phi);

  double rSinTheta       = m_StencilRadius * sinTheta;
  double rCosThetaCosPhi = m_StencilRadius * cosTheta * cosPhi;
  double rCosThetaSinPhi = m_StencilRadius * cosTheta * sinPhi;
  double rSinPhi         = m_StencilRadius * sinPhi;
  double rCosPhi         = m_StencilRadius * cosPhi;

  // Point 1: angle = 0;
  position[0] = Math::Round< SizeValueType >(m_StencilRadius + rCosThetaCosPhi);
  position[1] = Math::Round< SizeValueType >(m_StencilRadius + rCosThetaSinPhi);
  position[2] = Math::Round< SizeValueType >(m_StencilRadius - rSinTheta);

  threshold += it.GetPixel(position[0]
                           + strideY * position[1] + strideZ * position[2]);

  // Point 2: angle = 90;
  position[0] = Math::Round< SizeValueType >(m_StencilRadius - rSinPhi);
  position[1] = Math::Round< SizeValueType >(m_StencilRadius + rCosPhi);
  position[2] = m_StencilRadius;

  threshold += it.GetPixel(position[0]
                           + strideY * position[1] + strideZ * position[2]);

  // Point 3: angle = 180;
  position[0] = Math::Round< SizeValueType >(m_StencilRadius - rCosThetaCosPhi);
  position[1] = Math::Round< SizeValueType >(m_StencilRadius - rCosThetaSinPhi);
  position[2] = Math::Round< SizeValueType >(m_StencilRadius + rSinTheta);

  threshold += it.GetPixel(position[0]
                           + strideY * position[1] + strideZ * position[2]);

  // Point 4: angle = 270;
  position[0] = Math::Round< SizeValueType >(m_StencilRadius + rSinPhi);
  position[1] = Math::Round< SizeValueType >(m_StencilRadius - rCosPhi);
  position[2] = m_StencilRadius;

  threshold += it.GetPixel(position[0]
                           + strideY * position[1] + strideZ * position[2]);

  threshold *= 0.25;
  return threshold;
}

/*
 * Update the solution at pixels which lies on the data boundary.
 */
template< typename TImage >
typename MinMaxCurvatureFlowFunction< TImage >::PixelType
MinMaxCurvatureFlowFunction< TImage >
::ComputeUpdate(const NeighborhoodType & it, void *globalData,
                const FloatOffsetType & offset)
{
  PixelType update = this->Superclass::ComputeUpdate(
    it, globalData, offset);

  if ( update == 0.0 )
    {
    return update;
    }

  PixelType threshold;
  threshold = this->ComputeThreshold(Dispatch< ImageDimension >(), it);

  NeighborhoodInnerProduct< ImageType > innerProduct;
  PixelType                             avgValue = innerProduct(it, m_StencilOperator);

  if ( avgValue < threshold )
    {
    return ( std::max(update, NumericTraits< PixelType >::ZeroValue()) );
    }
  else
    {
    return ( std::min(update, NumericTraits< PixelType >::ZeroValue()) );
    }
}
} // end namespace itk

#endif
