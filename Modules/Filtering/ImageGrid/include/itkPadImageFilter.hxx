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
#ifndef __itkPadImageFilter_txx
#define __itkPadImageFilter_txx

#include "itkPadImageFilter.h"

namespace itk
{
/**
 *
 */
template< class TInputImage, class TOutputImage >
PadImageFilter< TInputImage, TOutputImage >
::PadImageFilter()
{
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_PadLowerBound[j] = 0;
    m_PadUpperBound[j] = 0;
    }
}

/**
 *
 */
template< class TInputImage, class TOutputImage >
void
PadImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Output Pad Lower Bounds: [";

  if ( ImageDimension >= 1 )
    {
    os << m_PadLowerBound[0];
    }
  for ( unsigned int j = 1; j < ImageDimension; j++ )
    {
    os << ", " << m_PadLowerBound[j];
    }
  os << "]" << std::endl;

  os << indent << "Output Pad Upper Bounds: [";
  if ( ImageDimension >= 1 )
    {
    os << m_PadUpperBound[0];
    }
  for ( unsigned int j = 1; j < ImageDimension; j++ )
    {
    os << ", " << m_PadUpperBound[j];
    }
  os << "]" << std::endl;
}

/**
 * PadImageFilter needs a smaller input requested region than
 * output requested region.  As such, PadImageFilter needs to
 * provide an implementation for GenerateInputRequestedRegion() in
 * order to inform the pipeline execution model.
 *
 * \sa ProcessObject::GenerateInputRequestedRegion()
 */
template< class TInputImage, class TOutputImage >
void
PadImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  // Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  typename Superclass::InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // we need to compute the input requested region (size and start index)
  unsigned int i;
  const typename TOutputImage::SizeType & outputRequestedRegionSize =
    outputPtr->GetRequestedRegion().GetSize();
  const typename TOutputImage::IndexType & outputRequestedRegionStartIndex =
    outputPtr->GetRequestedRegion().GetIndex();
  const typename TInputImage::SizeType & inputWholeRegionSize =
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType & inputWholeRegionStartIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();

  typename TInputImage::SizeType inputRequestedRegionSize;
  typename TInputImage::IndexType inputRequestedRegionStartIndex;

  OffsetValueType sizeTemp;

  for ( i = 0; i < TInputImage::ImageDimension; i++ )
    {
    if ( outputRequestedRegionStartIndex[i] <= inputWholeRegionStartIndex[i] )
      {
      inputRequestedRegionStartIndex[i] = inputWholeRegionStartIndex[i];
      }
    else
      {
      inputRequestedRegionStartIndex[i] =
        outputRequestedRegionStartIndex[i];
      }

    if ( ( inputWholeRegionStartIndex[i] + static_cast< OffsetValueType >( inputWholeRegionSize[i] ) ) <=
         ( outputRequestedRegionStartIndex[i] + static_cast< OffsetValueType >( outputRequestedRegionSize[i] ) ) )
      {
      sizeTemp = static_cast< OffsetValueType >( inputWholeRegionSize[i] )
                 + inputWholeRegionStartIndex[i] - inputRequestedRegionStartIndex[i];
      }
    else
      {
      sizeTemp = static_cast< OffsetValueType >( outputRequestedRegionSize[i] )
                 + outputRequestedRegionStartIndex[i] - inputRequestedRegionStartIndex[i];
      }

    //
    // The previous statements correctly handle overlapped regions where
    // at least some of the pixels from the input image end up reflected
    // in the output.  When there is no overlap, the size will be negative.
    // In that case we arbitrarily pick the start of the input region
    // as the start of the output region and zero for the size.
    //
    if ( sizeTemp < 0 )
      {
      inputRequestedRegionSize[i] = 0;
      inputRequestedRegionStartIndex[i] = inputWholeRegionStartIndex[i];
      }
    else
      {
      inputRequestedRegionSize[i] = sizeTemp;
      }
    }

  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetSize(inputRequestedRegionSize);
  inputRequestedRegion.SetIndex(inputRequestedRegionStartIndex);

  inputPtr->SetRequestedRegion(inputRequestedRegion);
}

/**
 * PadImageFilter produces an image which is a different resolution
 * than its input image.  As such, PadImageFilter needs to provide an
 * implementation for GenerateOutputInformation() in order to inform
 * the pipeline execution model.  The original documentation of this
 * method is below.
 *
 * \sa ProcessObject::GenerateOutputInformaton()
 */
template< class TInputImage, class TOutputImage >
void
PadImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  typename Superclass::InputImageConstPointer inputPtr  = this->GetInput();
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();

  if ( !outputPtr || !inputPtr )
    {
    return;
    }

  // we need to compute the output image size, and the
  // output image start index
  unsigned int i;
  typename TOutputImage::SizeType outputSize;
  typename TOutputImage::IndexType outputStartIndex;
  typename TInputImage::SizeType inputSize;
  typename TInputImage::IndexType inputStartIndex;

  inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  inputStartIndex = inputPtr->GetLargestPossibleRegion().GetIndex();

  for ( i = 0; i < TOutputImage::ImageDimension; i++ )
    {
    outputSize[i] = inputSize[i] + m_PadLowerBound[i] + m_PadUpperBound[i];
    outputStartIndex[i] = inputStartIndex[i] - static_cast< OffsetValueType >( m_PadLowerBound[i] );
    }

  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}
} // end namespace itk

#endif
