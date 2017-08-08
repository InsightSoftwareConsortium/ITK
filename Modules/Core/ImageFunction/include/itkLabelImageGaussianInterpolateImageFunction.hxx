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
/*=========================================================================
*
*  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
*
*  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
*
*  For complete copyright, license and disclaimer of warranty information
*  please refer to the NOTICE file at the top of the ITK source tree.
*
*=========================================================================*/

#ifndef itkLabelImageGaussianInterpolateImageFunction_hxx
#define itkLabelImageGaussianInterpolateImageFunction_hxx

#include "itkLabelImageGaussianInterpolateImageFunction.h"

namespace itk
{

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
      this->GetBoundingBoxEnd()[d] - this->GetBoundingBoxStart()[d] + 0.5 );
    const int begin = std::max( 0, static_cast<int>( std::floor( cindex[d] -
      this->GetBoundingBoxStart()[d] - this->GetCutOffDistance()[d] ) ) );
    const int end = std::min( boundingBoxSize, static_cast<int>( std::ceil(
      cindex[d] - this->GetBoundingBoxStart()[d] + this->GetCutOffDistance()[d] ) ) );
    region.SetIndex( d, begin );
    region.SetSize( d, end - begin );
    }

  RealType wmax = 0.0;
  OutputType Vmax = NumericTraits<OutputType>::ZeroValue();

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

    // Keep track of the max value
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
