/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMinMaxCurvatureFlowFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMinMaxCurvatureFlowFunction_txx_
#define __itkMinMaxCurvatureFlowFunction_txx_
#include "itkMinMaxCurvatureFlowFunction.h"

#include "vnl/vnl_math.h"
#include "itkNeighborhoodInnerProduct.h"

namespace itk {

/*
 * Constructor
 */
template<class TImage>
MinMaxCurvatureFlowFunction<TImage>
::MinMaxCurvatureFlowFunction()
{
  m_StencilRadius = 0;
  this->SetStencilRadius( 2 );

}


/*
 * Set the stencil radius.
 */
template<class TImage>
void
MinMaxCurvatureFlowFunction<TImage>
::SetStencilRadius( const RadiusValueType value )
{
  if ( m_StencilRadius == value ) { return; }

  m_StencilRadius = (value > 1) ? value:  1;
  RadiusType radius;
  unsigned int j;

  for ( j = 0; j < ImageDimension; j++ )
    {
    radius[j] = m_StencilRadius;
    } 

  this->SetRadius( radius );
  this->InitializeStencilOperator();

}


/*
 * Initialize the stencil operator.
 */
template<class TImage>
void
MinMaxCurvatureFlowFunction<TImage>
::InitializeStencilOperator()
{

  // Fill stencil operator with a sphere of radius m_StencilRadius.

  m_StencilOperator.SetRadius( m_StencilRadius );

  RadiusValueType counter[ImageDimension];
  unsigned int j;
  RadiusValueType span = 2 * m_StencilRadius + 1;
  RadiusValueType sqrRadius = m_StencilRadius * m_StencilRadius;
  for ( j = 0; j < ImageDimension; j++ )
    {
    counter[j] = 0;
    }

  typedef typename StencilOperatorType::Iterator Iterator;
  Iterator opIter;
  Iterator opEnd  = m_StencilOperator.End();
  
  unsigned long numPixelsInSphere = 0;

  for ( opIter = m_StencilOperator.Begin(); opIter < opEnd; ++opIter )
    {

    *opIter = NumericTraits<PixelType>::Zero;

    RadiusValueType length = 0;
    for ( j = 0; j < ImageDimension; j++ )
      {
      length += static_cast<RadiusValueType>( 
        vnl_math_sqr( static_cast<signed long>(counter[j]) - 
        static_cast<signed long>(m_StencilRadius) ) ); 
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
  for ( opIter = m_StencilOperator.Begin(); opIter < opEnd; ++opIter )
    {
    *opIter = static_cast<PixelType>( (double) *opIter /
      (double) numPixelsInSphere );
    }

}



/*
 * Compute the threshold by averaging the image intensity in 
 * the direction perpendicular to the image gradient.
 */
template<class TImage>
typename MinMaxCurvatureFlowFunction<TImage>::PixelType
MinMaxCurvatureFlowFunction<TImage>
::ComputeThreshold(const DispatchBase &,
                   const NeighborhoodType &it) const
{

  PixelType threshold = NumericTraits<PixelType>::Zero;

  // Compute gradient
  PixelType gradient[ImageDimension];
  PixelType gradMagnitude;
  unsigned long stride;
  unsigned long center;
  unsigned int j;

  center = it.Size()/2;

  gradMagnitude = NumericTraits<PixelType>::Zero;
  for ( j = 0; j < ImageDimension; j++ )
    {
    stride = it.GetStride( (unsigned long) j );
    gradient[j] = 0.5 * ( it.GetPixel( center + stride ) -
      it.GetPixel( center - stride ) );
    gradMagnitude += vnl_math_sqr( gradient[j] );
    }

  if ( gradMagnitude == 0.0 ) { return threshold; }

  gradMagnitude = vcl_sqrt( gradMagnitude );

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

  unsigned long i = 0;  
  unsigned long numPixels = 0;

  for ( neighIter = it.Begin(); neighIter < neighEnd; ++neighIter, ++i )
    {

    PixelType dotProduct = NumericTraits<PixelType>::Zero;
    PixelType vectorMagnitude = NumericTraits<PixelType>::Zero;

    for ( j = 0; j <  ImageDimension; j++ )
      {
      signed long diff = static_cast<signed long>( counter[j] ) -
        static_cast<signed long>( m_StencilRadius );

      dotProduct += static_cast<PixelType>( diff ) * gradient[j];
      vectorMagnitude += static_cast<PixelType>( vnl_math_sqr( diff ) );

      }

    vectorMagnitude = vcl_sqrt( vectorMagnitude );

    if ( vectorMagnitude != 0.0 )
      {
      dotProduct /= gradMagnitude * vectorMagnitude;
      }
    
    if ( vectorMagnitude >= m_StencilRadius && vnl_math_abs(dotProduct) < 0.262 )
      {
      threshold += it.GetPixel( i );
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
    threshold /= static_cast<PixelType>( numPixels );
    }

  return threshold;  

}


/*
 * Compute the threshold by averaging the image intensity in 
 * the direction perpendicular to the image gradient.
 */
template<class TImage>
typename MinMaxCurvatureFlowFunction<TImage>::PixelType
MinMaxCurvatureFlowFunction<TImage>
::ComputeThreshold(const Dispatch<2> &, const NeighborhoodType &it) const
{
  const signed int imageDimension = 2;
  
  if ( m_StencilRadius == 0 ) { return it.GetCenterPixel(); }

  PixelType threshold = NumericTraits<PixelType>::Zero;

  // Compute gradient
  PixelType gradient[imageDimension];
  PixelType gradMagnitude;
  unsigned long stride;
  unsigned long center;
  unsigned long position[imageDimension];
  int j;

  center = it.Size()/2;

  gradient[0] = 0.5 * ( it.GetPixel( center + 1 ) -
    it.GetPixel( center - 1) );
  gradMagnitude = vnl_math_sqr( gradient[0] );

  stride = it.GetStride( 1 );
  gradient[1] = 0.5 * ( it.GetPixel( center + stride ) -
    it.GetPixel( center - stride ) );
  gradMagnitude += vnl_math_sqr( gradient[1] );

  if ( gradMagnitude == 0.0 ) { return threshold; }

  gradMagnitude = vcl_sqrt( gradMagnitude ) /
   static_cast<PixelType>( m_StencilRadius );

  for ( j = 0; j < imageDimension; j++ )
    {
    gradient[j] /= gradMagnitude;
    }


  // Compute first perpendicular point
  position[0] = vnl_math_rnd( m_StencilRadius - gradient[1] );
  position[1] = vnl_math_rnd( m_StencilRadius + gradient[0] );
  
  threshold = it.GetPixel( position[0] + stride * position[1] );

  // Compute second perpendicular point 
  position[0] = vnl_math_rnd( m_StencilRadius + gradient[1] );
  position[1] = vnl_math_rnd( m_StencilRadius - gradient[0] );

  threshold += it.GetPixel( position[0] + stride * position[1] );
  threshold *= 0.5;

  return threshold;

}


/*
 * Compute the threshold by averaging the image intensity in 
 * the direction perpendicular to the image gradient.
 */
template<class TImage>
typename MinMaxCurvatureFlowFunction<TImage>::PixelType
MinMaxCurvatureFlowFunction<TImage>
::ComputeThreshold(const Dispatch<3> &, const NeighborhoodType &it) const
{
  const signed int imageDimension = 3;
  
  if ( m_StencilRadius == 0 ) { return it.GetCenterPixel(); }

  PixelType threshold = NumericTraits<PixelType>::Zero;

  // Compute gradient
  PixelType gradient[imageDimension];
  PixelType gradMagnitude;
  unsigned long strideY, strideZ;
  unsigned long center;
  unsigned long position[imageDimension];
  int j;

  center  = it.Size()/2;
  strideY = it.GetStride( 1 );
  strideZ = it.GetStride( 2 );

  gradient[0] = 0.5 * ( it.GetPixel( center + 1 ) -
    it.GetPixel( center - 1) );
  gradMagnitude = vnl_math_sqr( gradient[0] );

  gradient[1] = 0.5 * ( it.GetPixel( center + strideY ) -
    it.GetPixel( center - strideY ) );
  gradMagnitude += vnl_math_sqr( gradient[1] );

  gradient[2] = 0.5 * ( it.GetPixel( center + strideZ ) -
    it.GetPixel( center - strideZ ) );
  gradMagnitude += vnl_math_sqr( gradient[2] );

  if ( gradMagnitude == 0.0 ) { return threshold; }

  gradMagnitude = vcl_sqrt( gradMagnitude ) /
   static_cast<PixelType>( m_StencilRadius );

  for ( j = 0; j < imageDimension; j++ )
    {
    gradient[j] /= gradMagnitude;
    }

  double theta, phi;
  theta = acos( gradient[2] );
  if ( gradient[0] == 0 )
    {
    phi = vnl_math::pi * 0.5;
    }
  else
    {
    phi = atan( gradient[1] / gradient[0] );
    }

  double cosTheta = cos( theta );
  double sinTheta = sin( theta );
  double cosPhi   = cos( phi );
  double sinPhi   = sin( phi );

  double rSinTheta       = m_StencilRadius * sinTheta;
  double rCosThetaCosPhi = m_StencilRadius * cosTheta * cosPhi;
  double rCosThetaSinPhi = m_StencilRadius * cosTheta * sinPhi;
  double rSinPhi         = m_StencilRadius * sinPhi;
  double rCosPhi         = m_StencilRadius * cosPhi;

  // Point 1: angle = 0;
  position[0] = vnl_math_rnd( m_StencilRadius + rCosThetaCosPhi );
  position[1] = vnl_math_rnd( m_StencilRadius + rCosThetaSinPhi );
  position[2] = vnl_math_rnd( m_StencilRadius - rSinTheta );

  threshold += it.GetPixel( position[0] + 
    strideY * position[1] + strideZ * position[2] );

  // Point 2: angle = 90;
  position[0] = vnl_math_rnd( m_StencilRadius - rSinPhi );
  position[1] = vnl_math_rnd( m_StencilRadius + rCosPhi );
  position[2] = m_StencilRadius;

  threshold += it.GetPixel( position[0] + 
    strideY * position[1] + strideZ * position[2] );

  // Point 3: angle = 180;
  position[0] = vnl_math_rnd( m_StencilRadius - rCosThetaCosPhi );
  position[1] = vnl_math_rnd( m_StencilRadius - rCosThetaSinPhi );
  position[2] = vnl_math_rnd( m_StencilRadius + rSinTheta );

  threshold += it.GetPixel( position[0] + 
    strideY * position[1] + strideZ * position[2] );

  // Point 4: angle = 270;
  position[0] = vnl_math_rnd( m_StencilRadius + rSinPhi );
  position[1] = vnl_math_rnd( m_StencilRadius - rCosPhi );
  position[2] = m_StencilRadius;

  threshold += it.GetPixel( position[0] + 
    strideY * position[1] + strideZ * position[2] );
  
  threshold *= 0.25;
  return threshold;

}


/*
 * Update the solution at pixels which lies on the data boundary.
 */
template<class TImage>
typename MinMaxCurvatureFlowFunction<TImage>::PixelType
MinMaxCurvatureFlowFunction<TImage>
::ComputeUpdate(const NeighborhoodType &it, void * globalData,
                const FloatOffsetType& offset) const
{

  PixelType update = this->Superclass::ComputeUpdate(
    it, globalData, offset );

  if ( update == 0.0 )
    {
    return update;
    }

  PixelType threshold;
  threshold = this->ComputeThreshold( Dispatch<ImageDimension>(), it);

  NeighborhoodInnerProduct<ImageType> innerProduct;
  PixelType avgValue = innerProduct( it, m_StencilOperator );

  if ( avgValue < threshold )
    {
    return ( vnl_math_max( update, NumericTraits<PixelType>::Zero ) );
    }
  else
    {
    return ( vnl_math_min( update, NumericTraits<PixelType>::Zero ) );
    }

}


} // end namespace itk

#endif
