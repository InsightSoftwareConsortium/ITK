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
#ifndef itkProjectionImageFilter_hxx
#define itkProjectionImageFilter_hxx

#include "itkProjectionImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkProgressReporter.h"
#include "itkImageLinearConstIteratorWithIndex.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage, typename TAccumulator >
ProjectionImageFilter< TInputImage, TOutputImage, TAccumulator >
::ProjectionImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  m_ProjectionDimension = InputImageDimension - 1;
}

template< typename TInputImage, typename TOutputImage, typename TAccumulator >
void
ProjectionImageFilter< TInputImage, TOutputImage, TAccumulator >
::GenerateOutputInformation()
{
  itkDebugMacro("GenerateOutputInformation Start");

  if ( m_ProjectionDimension >= TInputImage::ImageDimension )
    {
    itkExceptionMacro(<< "Invalid ProjectionDimension. ProjectionDimension is "
                      << m_ProjectionDimension
                      << " but input ImageDimension is "
                      << TInputImage::ImageDimension);
    }

  typename TOutputImage::RegionType outputRegion;
  typename TInputImage::IndexType inputIndex;
  typename TInputImage::SizeType inputSize;
  typename TInputImage::DirectionType inDirection;
  typename TOutputImage::SizeType outputSize;
  typename TOutputImage::IndexType outputIndex;
  typename TInputImage::SpacingType inSpacing;
  typename TInputImage::PointType inOrigin;
  typename TOutputImage::SpacingType outSpacing;
  typename TOutputImage::PointType outOrigin;
  typename TOutputImage::DirectionType outDirection;

  // Get pointers to the input and output
  typename Superclass::OutputImagePointer output = this->GetOutput();
  typename Superclass::InputImagePointer input =
    const_cast< TInputImage * >( this->GetInput() );

  inputIndex = input->GetLargestPossibleRegion().GetIndex();
  inputSize = input->GetLargestPossibleRegion().GetSize();
  inSpacing = input->GetSpacing();
  inOrigin = input->GetOrigin();
  inDirection = input->GetDirection();

  // Set the LargestPossibleRegion of the output.
  // Reduce the size of the accumulated dimension.

  if ( static_cast< unsigned int >( InputImageDimension ) ==
       static_cast< unsigned int >( OutputImageDimension ) )
    {
    for ( unsigned int i = 0; i < InputImageDimension; i++ )
      {
      if ( i != m_ProjectionDimension )
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
      // Can't directly copy the matrices: In the case the dimensions
      // are different, this part of the function still needs to be able
      // to compile.
      for ( unsigned int j = 0; j < InputImageDimension; j++ )
        {
        outDirection[i][j]  = inDirection[i][j];
        }
      }
    }
  else
    {
    // Then OutputImageDimension = InputImageDimension - 1
    for ( unsigned int i = 0; i < OutputImageDimension; i++ )
      {
      unsigned int pos = i;
      if ( i == m_ProjectionDimension )
        {
        pos = InputImageDimension - 1;
        }
      outputSize[i]  = inputSize[pos];
      outputIndex[i] = inputIndex[pos];
      outSpacing[i] = inSpacing[pos];
      outOrigin[i]  = inOrigin[pos];
      }
    outDirection.SetIdentity();
    }

  outputRegion.SetSize(outputSize);
  outputRegion.SetIndex(outputIndex);
  output->SetOrigin(outOrigin);
  output->SetSpacing(outSpacing);
  output->SetDirection(outDirection);
  output->SetLargestPossibleRegion(outputRegion);

  itkDebugMacro("GenerateOutputInformation End");
}

template< typename TInputImage, typename TOutputImage, typename TAccumulator >
void
ProjectionImageFilter< TInputImage, TOutputImage, TAccumulator >
::GenerateInputRequestedRegion()
{
  itkDebugMacro("GenerateInputRequestedRegion Start");

  if ( m_ProjectionDimension >= TInputImage::ImageDimension )
    {
    itkExceptionMacro(<< "Invalid ProjectionDimension "
                      << m_ProjectionDimension
                      << " but ImageDimension is "
                      << TInputImage::ImageDimension);
    }

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

    if ( static_cast< unsigned int >( InputImageDimension ) ==
         static_cast< unsigned int >( OutputImageDimension ) )
      {
      for ( unsigned int i = 0; i < TInputImage::ImageDimension; i++ )
        {
        if ( i != m_ProjectionDimension )
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
      }
    else
      {
      for ( unsigned int i = 0; i < OutputImageDimension; i++ )
        {
        if ( i != m_ProjectionDimension )
          {
          inputSize[i] = outputSize[i];
          inputIndex[i] = outputIndex[i];
          }
        else
          {
          // the size of the output image on this dimension is the size
          // of the input image on the removed dimension
          inputSize[InputImageDimension - 1] = outputSize[i];
          inputIndex[InputImageDimension - 1] = outputIndex[i];
          }
        }
      inputSize[m_ProjectionDimension] =
        inputLargSize[m_ProjectionDimension];
      inputIndex[m_ProjectionDimension] =
        inputLargIndex[m_ProjectionDimension];
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
template< typename TInputImage, typename TOutputImage, typename TAccumulator >
void
ProjectionImageFilter< TInputImage, TOutputImage, TAccumulator >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  if ( m_ProjectionDimension >= TInputImage::ImageDimension )
    {
    itkExceptionMacro(<< "Invalid ProjectionDimension "
                      << m_ProjectionDimension
                      << " but ImageDimension is "
                      << TInputImage::ImageDimension);
    }

  // use the output image to report the progress: there is no need to
  // call CompletedPixel() for all input pixel
  ProgressReporter progress( this, threadId,
                             outputRegionForThread.GetNumberOfPixels() );

  typedef typename TOutputImage::PixelType OutputPixelType;

  // get some values, just to be easier to manipulate
  typename Superclass::InputImageConstPointer inputImage = this->GetInput();

  typename TInputImage::RegionType inputRegion =
    inputImage->GetLargestPossibleRegion();

  typename TInputImage::SizeType inputSize = inputRegion.GetSize();
  typename TInputImage::IndexType inputIndex = inputRegion.GetIndex();

  typename TOutputImage::Pointer outputImage = this->GetOutput();
  typename TOutputImage::RegionType outputRegion =
    outputImage->GetLargestPossibleRegion();

  typename TOutputImage::SizeType outputSizeForThread =
    outputRegionForThread.GetSize();
  typename TOutputImage::IndexType outputIndexForThread =
    outputRegionForThread.GetIndex();

  // compute the input region for this thread
  typename TInputImage::RegionType inputRegionForThread = inputRegion;
  typename TInputImage::SizeType inputSizeForThread = inputSize;
  typename TInputImage::IndexType inputIndexForThread = inputIndex;

  if ( static_cast< unsigned int >( InputImageDimension ) ==
       static_cast< unsigned int >( OutputImageDimension ) )
    {
    for ( unsigned int i = 0; i < InputImageDimension; i++ )
      {
      if ( i != m_ProjectionDimension )
        {
        inputSizeForThread[i] = outputSizeForThread[i];
        inputIndexForThread[i] = outputIndexForThread[i];
        }
      }
    }
  else
    {
    for ( unsigned int i = 0; i < OutputImageDimension; i++ )
      {
      if ( i != m_ProjectionDimension )
        {
        inputSizeForThread[i] = outputSizeForThread[i];
        inputIndexForThread[i] = outputIndexForThread[i];
        }
      else
        {
        // the size of the output image on this dimension is the size
        // of the input image on the removed dimension
        inputSizeForThread[InputImageDimension - 1] = outputSizeForThread[i];
        inputIndexForThread[InputImageDimension - 1] = outputIndexForThread[i];
        }
      }
    inputSizeForThread[m_ProjectionDimension] =
      inputSize[m_ProjectionDimension];
    inputIndexForThread[m_ProjectionDimension] =
      inputIndex[m_ProjectionDimension];
    }
  inputRegionForThread.SetSize(inputSizeForThread);
  inputRegionForThread.SetIndex(inputIndexForThread);

  SizeValueType projectionSize = inputSize[m_ProjectionDimension];

  // create the iterators for input and output image
  typedef ImageLinearConstIteratorWithIndex< TInputImage > InputIteratorType;
  InputIteratorType iIt(inputImage, inputRegionForThread);
  iIt.SetDirection(m_ProjectionDimension);
  iIt.GoToBegin();

  // instantiate the accumulator
  AccumulatorType accumulator = this->NewAccumulator(projectionSize);

  // ok, everything is ready... lets the linear iterator do its job !
  while ( !iIt.IsAtEnd() )
    {
    // init the accumulator before a new set of pixels
    accumulator.Initialize();

    while ( !iIt.IsAtEndOfLine() )
      {
      accumulator( iIt.Get() );
      ++iIt;
      }

    // move the output iterator and set the output value
    typename TOutputImage::IndexType oIdx;
    typename TInputImage::IndexType iIdx = iIt.GetIndex();

    if ( static_cast< unsigned int >( InputImageDimension ) ==
         static_cast< unsigned int >( OutputImageDimension ) )
      {
      for ( unsigned int i = 0; i < InputImageDimension; i++ )
        {
        if ( i != m_ProjectionDimension )
          {
          oIdx[i] = iIdx[i];
          }
        else
          {
          oIdx[i] = 0;
          }
        }
      }
    else
      {
      for ( unsigned int i = 0; i < OutputImageDimension; i++ )
        {
        if ( i != m_ProjectionDimension )
          {
          oIdx[i] = iIdx[i];
          }
        else
          {
          oIdx[i] = iIdx[InputImageDimension - 1];
          }
        }
      }

    outputImage->SetPixel( oIdx,
                           static_cast< OutputPixelType >( accumulator.GetValue() ) );

    // one more line done !
    progress.CompletedPixel();

    // continue with the next one
    iIt.NextLine();
    }
}

template< typename TInputImage, typename TOutputImage, typename TAccumulator >
TAccumulator
ProjectionImageFilter< TInputImage, TOutputImage, TAccumulator >::NewAccumulator(SizeValueType size) const
{
  return TAccumulator(size);
}

template< typename TInputImage, typename TOutputImage, typename TAccumulator >
void
ProjectionImageFilter< TInputImage, TOutputImage, TAccumulator >::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ProjectionDimension: " << m_ProjectionDimension << std::endl;
}
} // end namespace itk

#endif
