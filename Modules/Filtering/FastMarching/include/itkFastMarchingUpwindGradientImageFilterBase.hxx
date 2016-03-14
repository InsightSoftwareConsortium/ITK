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
#ifndef itkFastMarchingUpwindGradientImageFilterBase_hxx
#define itkFastMarchingUpwindGradientImageFilterBase_hxx

#include "itkFastMarchingUpwindGradientImageFilterBase.h"
#include "itkImageRegionIterator.h"
#include "itkNumericTraits.h"
#include "itkMath.h"
#include <algorithm>

namespace itk
{
/**
 *
 */
template< typename TInput, typename TOutput >
FastMarchingUpwindGradientImageFilterBase< TInput, TOutput >
::FastMarchingUpwindGradientImageFilterBase()
{
  GradientImagePointer GradientImage = GradientImageType::New();
  this->SetNthOutput( 1, GradientImage.GetPointer() );
}

template< typename TInput, typename TOutput >
typename FastMarchingUpwindGradientImageFilterBase< TInput, TOutput >::GradientImageType*
FastMarchingUpwindGradientImageFilterBase< TInput, TOutput >
::GetGradientImage()
{
  return dynamic_cast< GradientImageType* >( this->ProcessObject::GetOutput( 1 ) );
}

/**
 *
 */
template< typename TInput, typename TOutput >
void
FastMarchingUpwindGradientImageFilterBase< TInput, TOutput >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

/**
 *
 */
template< typename TInput, typename TOutput >
void
FastMarchingUpwindGradientImageFilterBase< TInput, TOutput >::
InitializeOutput(OutputImageType *output)
{
  Superclass::InitializeOutput(output);

  // allocate memory for the GradientImage if requested
  GradientImagePointer GradientImage = this->GetGradientImage();

  GradientImage->CopyInformation( this->GetInput() );
  GradientImage->SetBufferedRegion( output->GetBufferedRegion() );
  GradientImage->Allocate();

  typedef ImageRegionIterator< GradientImageType > GradientIterator;

  GradientIterator gradientIt( GradientImage,
                               GradientImage->GetBufferedRegion() );

  GradientPixelType zeroGradient;
  typedef typename GradientPixelType::ValueType GradientPixelValueType;
  zeroGradient.Fill(NumericTraits< GradientPixelValueType >::ZeroValue());

  gradientIt.GoToBegin();

  while( !gradientIt.IsAtEnd() )
    {
    gradientIt.Set(zeroGradient);
    ++gradientIt;
    }
}


template< typename TInput, typename TOutput >
void
FastMarchingUpwindGradientImageFilterBase< TInput, TOutput >::
UpdateNeighbors(
  OutputImageType* oImage,
  const NodeType& iNode )
{
  Superclass::UpdateNeighbors( oImage, iNode );

  this->ComputeGradient( oImage, iNode );
}

/**
 *
 */
template< typename TInput, typename TOutput >
void
FastMarchingUpwindGradientImageFilterBase< TInput, TOutput >
::ComputeGradient( OutputImageType* oImage,
                  const NodeType& iNode )
{
  NodeType neighIndex = iNode;

  OutputPixelType centerPixel;
  OutputPixelType dx_forward;
  OutputPixelType dx_backward;
  GradientPixelType gradientPixel;

  const OutputPixelType ZERO = NumericTraits< OutputPixelType >::ZeroValue();

  OutputSpacingType spacing = oImage->GetSpacing();

  unsigned int xStride[ itkGetStaticConstMacro( ImageDimension ) ];

  centerPixel = oImage->GetPixel( iNode );

  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    neighIndex = iNode;

    // Set stride of one in each direction
    xStride[j] = 1;

    // Compute one-sided finite differences with alive neighbors
    // (the front can only come from there)
    dx_backward = ZERO;
    neighIndex[j] = iNode[j] - xStride[j];

    if ( !( ( neighIndex[j] > this->m_LastIndex[j] ) ||
            ( neighIndex[j] < this->m_StartIndex[j] ) ) )
      {
      if ( this->GetLabelValueForGivenNode(neighIndex) == Traits::Alive )
        {
        dx_backward = centerPixel - oImage->GetPixel(neighIndex);
        }
      }

    dx_forward = ZERO;
    neighIndex[j] = iNode[j] + xStride[j];

    if ( !( ( neighIndex[j] > this->m_LastIndex[j] ) ||
            ( neighIndex[j] < this->m_StartIndex[j] ) ) )
      {
      if ( this->GetLabelValueForGivenNode(neighIndex) == Traits::Alive )
        {
        dx_forward = oImage->GetPixel(neighIndex) - centerPixel;
        }
      }

    // Compute upwind finite differences
    if ( std::max(dx_backward, -dx_forward) < ZERO )
      {
      gradientPixel[j] = ZERO;
      }
    else
      {
      if ( dx_backward > -dx_forward )
        {
        gradientPixel[j] = dx_backward;
        }
      else
        {
        gradientPixel[j] = dx_forward;
        }
      }

    gradientPixel[j] /= spacing[j];
    }

  GradientImagePointer GradientImage = this->GetGradientImage();
  GradientImage->SetPixel(iNode, gradientPixel);
}
} // namespace itk

#endif
