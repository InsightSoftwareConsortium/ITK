/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkGaussianInterpolateImageFunction.hxx,v $
  Language:  C++
  Date:      $Date: $
  Version:   $Revision: $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGaussianInterpolateImageFunction_hxx
#define __itkGaussianInterpolateImageFunction_hxx

#include "itkGaussianInterpolateImageFunction.h"

#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

/**
 * Constructor
 */
template<typename TImageType, typename TCoordRep>
GaussianInterpolateImageFunction<TImageType, TCoordRep>
::GaussianInterpolateImageFunction()
{
  this->m_Alpha = 1.0;
  this->m_Sigma.Fill( 1.0 );
}

/**
 * Standard "PrintSelf" method
 */
template<typename TImageType, typename TCoordRep>
void
GaussianInterpolateImageFunction<TImageType, TCoordRep>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
  os << indent << "Alpha: " << this->m_Alpha << std::endl;
  os << indent << "Sigma: " << this->m_Sigma << std::endl;
}

template<typename TImageType, typename TCoordRep>
void
GaussianInterpolateImageFunction<TImageType, TCoordRep>
::ComputeBoundingBox()
{
  if( !this->GetInputImage() )
    {
    return;
    }

  typename InputImageType::ConstPointer input = this->GetInputImage();
  typename InputImageType::SpacingType spacing = input->GetSpacing();
  typename InputImageType::SizeType size = input->GetBufferedRegion().GetSize();

  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    this->m_BoundingBoxStart[d] = -0.5;
    this->m_BoundingBoxEnd[d] = static_cast<RealType>( size[d] ) - 0.5;
    this->m_ScalingFactor[d] = 1.0 / ( vnl_math::sqrt2 * this->m_Sigma[d] / spacing[d] );
    this->m_CutoffDistance[d] = this->m_Sigma[d] * this->m_Alpha / spacing[d];
    }
}

template<typename TImageType, typename TCoordRep>
typename GaussianInterpolateImageFunction<TImageType, TCoordRep>
::OutputType
GaussianInterpolateImageFunction<TImageType, TCoordRep>
::EvaluateAtContinuousIndex( const ContinuousIndexType & cindex, OutputType *grad ) const
{
  vnl_vector<RealType> erfArray[ImageDimension];
  vnl_vector<RealType> gerfArray[ImageDimension];

  // Compute the ERF difference arrays
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    bool evaluateGradient = false;
    if( grad )
      {
      evaluateGradient = true;
      }
    this->ComputeErrorFunctionArray( d, cindex[d], erfArray[d],
      gerfArray[d], evaluateGradient );
    }

  RealType sum_me = 0.0;
  RealType sum_m = 0.0;
  ArrayType dsum_me;
  ArrayType dsum_m;
  ArrayType dw;

  dsum_m.Fill( 0.0 );
  dsum_me.Fill( 0.0 );
  dw.Fill( 0.0 );

  // Loop over the voxels in the region identified
  ImageRegion<ImageDimension> region;
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    int boundingBoxSize = static_cast<int>(
      this->m_BoundingBoxEnd[d] - this->m_BoundingBoxStart[d] + 0.5 );
    int begin = vnl_math_max( 0, static_cast<int>( vcl_floor( cindex[d] -
      this->m_BoundingBoxStart[d] - this->m_CutoffDistance[d] ) ) );
    int end = vnl_math_min( boundingBoxSize, static_cast<int>( vcl_ceil(
      cindex[d] - this->m_BoundingBoxStart[d] + this->m_CutoffDistance[d] ) ) );
    region.SetIndex( d, begin );
    region.SetSize( d, end - begin );
    }

  ImageRegionConstIteratorWithIndex<InputImageType> It(
    this->GetInputImage(), region );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    unsigned int j = It.GetIndex()[0];
    RealType w = erfArray[0][j];
    if( grad )
      {
      dw[0] = gerfArray[0][j];
      for( unsigned int d = 1; d < ImageDimension; d++ )
        {
        dw[d] = erfArray[0][j];
        }
      }
    for( unsigned int d = 1; d < ImageDimension; d++)
      {
      j = It.GetIndex()[d];
      w *= erfArray[d][j];
      if( grad )
        {
        for( unsigned int q = 0; q < ImageDimension; q++ )
          {
          if( d == q )
            {
            dw[q] *= gerfArray[d][j];
            }
          else
            {
            dw[q] *= erfArray[d][j];
            }
          }
        }
      }
    RealType V = It.Get();
    sum_me += V * w;
    sum_m += w;
    if( grad )
      {
      for( unsigned int q = 0; q < ImageDimension; q++ )
        {
        dsum_me[q] += V * dw[q];
        dsum_m[q] += dw[q];
        }
      }
    }
  RealType rc = sum_me / sum_m;

  if( grad )
    {
    for( unsigned int q = 0; q < ImageDimension; q++ )
      {
      grad[q] = ( dsum_me[q] - rc * dsum_m[q] ) / sum_m;
      grad[q] /= -vnl_math::sqrt2 * this->m_Sigma[q];
      }
    }

  return rc;
}

template<typename TImageType, typename TCoordRep>
void
GaussianInterpolateImageFunction<TImageType, TCoordRep>
::ComputeErrorFunctionArray( unsigned int dimension, RealType cindex,
  vnl_vector<RealType> &erfArray, vnl_vector<RealType> &gerfArray,
  bool evaluateGradient ) const
{
  // Determine the range of voxels along the line where to evaluate erf
  int boundingBoxSize = static_cast<int>(
    this->m_BoundingBoxEnd[dimension] - this->m_BoundingBoxStart[dimension] +
    0.5 );
  int begin = vnl_math_max( 0, static_cast<int>( vcl_floor( cindex -
    this->m_BoundingBoxStart[dimension] -
    this->m_CutoffDistance[dimension] ) ) );
  int end = vnl_math_min( boundingBoxSize, static_cast<int>( vcl_ceil( cindex -
    this->m_BoundingBoxStart[dimension] +
    this->m_CutoffDistance[dimension] ) ) );

  erfArray.set_size( boundingBoxSize );
  gerfArray.set_size( boundingBoxSize );

  // Start at the first voxel
  RealType t = ( this->m_BoundingBoxStart[dimension] - cindex +
    static_cast<RealType>( begin ) ) * this->m_ScalingFactor[dimension];
  RealType e_last = vnl_erf( t );
  RealType g_last = 0.0;
  if( evaluateGradient )
    {
    g_last = vnl_math::two_over_sqrtpi * vcl_exp( -vnl_math_sqr( t ) );
    }

  for( int i = begin; i < end; i++ )
    {
    t += this->m_ScalingFactor[dimension];
    RealType e_now = vnl_erf( t );
    erfArray[i] = e_now - e_last;
    if( evaluateGradient )
      {
      RealType g_now = vnl_math::two_over_sqrtpi * vcl_exp( -vnl_math_sqr( t ) );
      gerfArray[i] = g_now - g_last;
      g_last = g_now;
      }
    e_last = e_now;
    }
}

} // namespace itk

#endif
