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
#ifndef itkAccumulateImageFilter_hxx
#define itkAccumulateImageFilter_hxx

#include "itkAccumulateImageFilter.h"
#include "itkImageRegionIterator.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
AccumulateImageFilter< TInputImage, TOutputImage >
::AccumulateImageFilter()
{
  m_AccumulateDimension = InputImageDimension - 1;
  m_Average = false;
}

template< typename TInputImage, typename TOutputImage >
void
AccumulateImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
{
  itkDebugMacro("GenerateOutputInformation Start");

  typename TOutputImage::RegionType outputRegion;
  typename TInputImage::IndexType inputIndex;
  typename TInputImage::SizeType inputSize;
  typename TOutputImage::SizeType outputSize;
  typename TOutputImage::IndexType outputIndex;
  typename TInputImage::SpacingType inSpacing;
  typename TInputImage::PointType inOrigin;
  typename TOutputImage::SpacingType outSpacing;
  typename TOutputImage::DirectionType outDirection;
  typename TOutputImage::PointType outOrigin;

  // Get pointers to the input and output
  typename Superclass::OutputImagePointer output = this->GetOutput();
  typename Superclass::InputImagePointer input = const_cast< TInputImage * >( this->GetInput() );

  if ( !input || !output )
    {
    return;
    }

  inputIndex = input->GetLargestPossibleRegion().GetIndex();
  inputSize = input->GetLargestPossibleRegion().GetSize();
  inSpacing = input->GetSpacing();
  outDirection = input->GetDirection();
  inOrigin = input->GetOrigin();

  // Set the LargestPossibleRegion of the output.
  // Reduce the size of the accumulated dimension.
  for ( unsigned int i = 0; i < InputImageDimension; i++ )
    {
    if ( i != m_AccumulateDimension )
      {
      outputSize[i]  = inputSize[i];
      outputIndex[i] = inputIndex[i];
      outSpacing[i] = inSpacing[i];
      outOrigin[i]  = inOrigin[i];
      }
    else
      {
      outputSize[i]  = 1;
      outputIndex[i] = 0;
      outSpacing[i] = inSpacing[i] * inputSize[i];
      outOrigin[i]  = inOrigin[i] + ( i - 1 ) * inSpacing[i] / 2;
      }
    }

  outputRegion.SetSize(outputSize);
  outputRegion.SetIndex(outputIndex);
  output->SetOrigin(outOrigin);
  output->SetSpacing(outSpacing);
  output->SetDirection(outDirection);
  output->SetLargestPossibleRegion(outputRegion);

  itkDebugMacro("GenerateOutputInformation End");
}

template< typename TInputImage, typename TOutputImage >
void
AccumulateImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  itkDebugMacro("GenerateInputRequestedRegion Start");
  Superclass::GenerateInputRequestedRegion();

  if ( this->GetInput() )
    {
    typename TInputImage::RegionType RequestedRegion;
    typename TInputImage::SizeType inputSize;
    typename TInputImage::IndexType inputIndex;
    typename TInputImage::SizeType inputLargSize;
    typename TInputImage::IndexType inputLargIndex;
    typename TOutputImage::SizeType outputSize;
    typename TOutputImage::IndexType outputIndex;

    outputIndex = this->GetOutput()->GetRequestedRegion().GetIndex();
    outputSize = this->GetOutput()->GetRequestedRegion().GetSize();
    inputLargSize = this->GetInput()->GetLargestPossibleRegion().GetSize();
    inputLargIndex = this->GetInput()->GetLargestPossibleRegion().GetIndex();

    for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
      {
      if ( i != m_AccumulateDimension )
        {
        inputSize[i] = outputSize[i];
        inputIndex[i] = outputIndex[i];
        }
      else
        {
        inputSize[i] = inputLargSize[i];
        inputIndex[i] = inputLargIndex[i];
        }
      }

    RequestedRegion.SetSize(inputSize);
    RequestedRegion.SetIndex(inputIndex);
    InputImagePointer input = const_cast< TInputImage * >( this->GetInput() );
    input->SetRequestedRegion (RequestedRegion);
    }

  itkDebugMacro("GenerateInputRequestedRegion End");
}

/**
 * GenerateData Performs the accumulation
 */
template< typename TInputImage, typename TOutputImage >
void
AccumulateImageFilter< TInputImage, TOutputImage >
::GenerateData(void)
{
  if ( m_AccumulateDimension >= TInputImage::ImageDimension )
    {
    itkExceptionMacro(
      << "AccumulateImageFilter: invalid dimension to accumulate. AccumulateDimension = " << m_AccumulateDimension);
    }

  typedef typename TOutputImage::PixelType                          OutputPixelType;
  typedef typename NumericTraits< OutputPixelType >::AccumulateType AccumulateType;

  typename Superclass::InputImageConstPointer inputImage = this->GetInput();
  typename TOutputImage::Pointer outputImage = this->GetOutput();
  outputImage->SetBufferedRegion( outputImage->GetRequestedRegion() );
  outputImage->Allocate();

// Accumulate over the Nth dimension ( = m_AccumulateDimension)
// and divide by the size of the accumulated dimension.
  typedef ImageRegionIterator< TOutputImage > outputIterType;
  outputIterType outputIter( outputImage, outputImage->GetBufferedRegion() );
  typedef ImageRegionConstIterator< TInputImage > inputIterType;

  typename TInputImage::RegionType AccumulatedRegion;
  typename TInputImage::SizeType AccumulatedSize = inputImage->GetLargestPossibleRegion().GetSize();
  typename TInputImage::IndexType AccumulatedIndex = inputImage->GetLargestPossibleRegion().GetIndex();

  typename TInputImage::SizeValueType  SizeAccumulateDimension = AccumulatedSize[m_AccumulateDimension];
  double                               SizeAccumulateDimensionDouble = static_cast< double >( SizeAccumulateDimension );
  typename TInputImage::IndexValueType IndexAccumulateDimension = AccumulatedIndex[m_AccumulateDimension];
  for ( unsigned int i = 0; i < InputImageDimension; i++ )
    {
    if ( i != m_AccumulateDimension )
      {
      AccumulatedSize[i] = 1;
      }
    }
  AccumulatedRegion.SetSize(AccumulatedSize);
  outputIter.GoToBegin();
  while ( !outputIter.IsAtEnd() )
    {
    typename TOutputImage::IndexType OutputIndex = outputIter.GetIndex();
    for ( unsigned int i = 0; i < InputImageDimension; i++ )
      {
      if ( i != m_AccumulateDimension )
        {
        AccumulatedIndex[i] = OutputIndex[i];
        }
      else
        {
        AccumulatedIndex[i] = IndexAccumulateDimension;
        }
      }
    AccumulatedRegion.SetIndex(AccumulatedIndex);
    inputIterType inputIter(inputImage, AccumulatedRegion);
    inputIter.GoToBegin();
    AccumulateType Value = NumericTraits< AccumulateType >::ZeroValue();
    while ( !inputIter.IsAtEnd() )
      {
      Value += static_cast< AccumulateType >( inputIter.Get() );
      ++inputIter;
      }
    if ( m_Average )
      {
      outputIter.Set( static_cast< OutputPixelType >( Value / SizeAccumulateDimensionDouble ) );
      }
    else
      {
      outputIter.Set ( static_cast< OutputPixelType >( Value ) );
      }
    ++outputIter;
    }
}

template< typename TInputImage, typename TOutputImage >
void
AccumulateImageFilter< TInputImage, TOutputImage >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "AccumulateDimension: " << m_AccumulateDimension << std::endl;
  os << indent << "Average: " << ( m_Average ? "On" : "Off" ) << std::endl;
}
} // end namespace itk

#endif
