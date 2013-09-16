/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkLabelImageGaussianInterpolateImageFunction.hxx,v $
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
#ifndef __itkLabelImageGaussianInterpolateImageFunction_hxx
#define __itkLabelImageGaussianInterpolateImageFunction_hxx

#include "itkLabelImageGaussianInterpolateImageFunction.h"

namespace itk
{

/**
 * Constructor
 */
template<typename TInputImage, typename TCoordRep, typename TPixelCompare>
LabelImageGaussianInterpolateImageFunction<TInputImage, TCoordRep, TPixelCompare>
::LabelImageGaussianInterpolateImageFunction()
{
}

template<typename TInputImage, typename TCoordRep, typename TPixelCompare>
typename LabelImageGaussianInterpolateImageFunction<TInputImage, TCoordRep, TPixelCompare>
::OutputType
LabelImageGaussianInterpolateImageFunction<TInputImage, TCoordRep, TPixelCompare>
::EvaluateAtContinuousIndex( const ContinuousIndexType & cindex, OutputType * itkNotUsed( grad )  ) const
{
  vnl_vector<RealType> erfArray[ImageDimension];
  vnl_vector<RealType> gerfArray[ImageDimension];

  // Compute the ERF difference arrays
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    const bool evaluateGradient = false;
    this->ComputeErrorFunctionArray( d, cindex[d], erfArray[d],
      gerfArray[d], evaluateGradient );
    }

  // Loop over the voxels in the region identified
  ImageRegion<ImageDimension> region;
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    const int boundingBoxSize = static_cast<int>(
      this->m_BoundingBoxEnd[d] - this->m_BoundingBoxStart[d] + 0.5 );
    const int begin = vnl_math_max( 0, static_cast<int>( vcl_floor( cindex[d] -
      this->m_BoundingBoxStart[d] - this->m_CutoffDistance[d] ) ) );
    const int end = vnl_math_min( boundingBoxSize, static_cast<int>( vcl_ceil(
      cindex[d] - this->m_BoundingBoxStart[d] + this->m_CutoffDistance[d] ) ) );
    region.SetIndex( d, begin );
    region.SetSize( d, end - begin );
    }

  RealType wmax = 0.0;
  OutputType Vmax = NumericTraits<OutputType>::Zero;

  // Create a map object to store weights for each label encountered
  // inside the search region. This is not as efficient as having a
  // linear list of labels, but probably not a huge deal compared to
  // having to evaluate the erf function
  typedef std::map<OutputType, RealType, TPixelCompare> WeightMapType;
  typedef typename std::map<OutputType, RealType, TPixelCompare>::iterator WeightMapIteratorType;
  WeightMapType weightMap;

  ImageRegionConstIteratorWithIndex<InputImageType> It( this->GetInputImage(), region );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    unsigned int j = It.GetIndex()[0];
    RealType w = erfArray[0][j];
    for( unsigned int d = 1; d < ImageDimension; d++)
      {
      j = It.GetIndex()[d];
      w *= erfArray[d][j];
      }

    const OutputType V = It.Get();
    WeightMapIteratorType it = weightMap.find( V );
    RealType wtest = 0.0;

    if( it != weightMap.end() )
      {
      it->second += w;
      wtest = it->second;
      }
    else
      {
      weightMap.insert( std::make_pair( V, w ) );
      wtest = w;
      }

    //Keep track of the max value
    if( wtest > wmax )
      {
      wmax = wtest;
      Vmax = V;
      }
    }
  return Vmax;
}

} // namespace itk

#endif
