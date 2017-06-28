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
#ifndef itkBinShrinkImageFilter_hxx
#define itkBinShrinkImageFilter_hxx

#include "itkBinShrinkImageFilter.h"
#include "itkImageScanlineIterator.h"
#include "itkProgressReporter.h"
#include <numeric>
#include <functional>

namespace itk
{

template< class TInputImage, class TOutputImage >
BinShrinkImageFilter< TInputImage, TOutputImage >
::BinShrinkImageFilter()
{
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_ShrinkFactors[j] = 1;
    }
}

template< class TInputImage, class TOutputImage >
void
BinShrinkImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Shrink Factor: ";
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    os << m_ShrinkFactors[j] << " ";
    }
  os << std::endl;
}

template< class TInputImage, class TOutputImage >
void
BinShrinkImageFilter< TInputImage, TOutputImage >
::SetShrinkFactors(unsigned int factor)
{
  unsigned int j;

  for ( j = 0; j < ImageDimension; j++ )
    {
    if ( factor != m_ShrinkFactors[j] ) { break; }
    }
  if ( j < ImageDimension )
    {
    this->Modified();
    for ( j = 0; j < ImageDimension; j++ )
      {
      m_ShrinkFactors[j] = factor;
      if ( m_ShrinkFactors[j] < 1 )
        {
        m_ShrinkFactors[j] = 1;
        }
      }
    }
}

template< class TInputImage, class TOutputImage >
void
BinShrinkImageFilter< TInputImage, TOutputImage >
::SetShrinkFactor(unsigned int i, unsigned int factor)
{
  if ( m_ShrinkFactors[i] == factor )
    {
    return;
    }

  this->Modified();
  m_ShrinkFactors[i] = factor;
}

template <class TInputImage, class TOutputImage>
void
BinShrinkImageFilter<TInputImage,TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       ThreadIdType threadId)
{
  itkDebugMacro(<<"Actually executing on region:" << outputRegionForThread);

  // Get the input and output pointers
  const InputImageType * inputPtr = this->GetInput();
  OutputImageType *      outputPtr = this->GetOutput();

  typedef typename InputImageType::PixelType                 InputPixelType;
  typedef typename OutputImageType::PixelType                OutputPixelType;
  typedef typename NumericTraits< InputPixelType >::RealType AccumulatePixelType;

  typedef ImageScanlineConstIterator< InputImageType > InputConstIteratorType;
  typedef ImageScanlineIterator< OutputImageType >     OutputIteratorType;

  InputConstIteratorType inputIterator(inputPtr, inputPtr->GetRequestedRegion() );
  OutputIteratorType     outputIterator(outputPtr, outputRegionForThread);

  // Set up shaped neighbor hood by defining the offsets
  OutputOffsetType negativeOffset, positiveOffset, iOffset;

  negativeOffset[0] = 0;
  positiveOffset[0] = 0;
  for ( unsigned int i=1; i < TInputImage::ImageDimension; ++i)
    {
    negativeOffset[i] = 0;
    positiveOffset[i] = this->GetShrinkFactors()[i]-1;
    }

  std::vector<OutputOffsetType> offsets;
  iOffset = negativeOffset;
  while (iOffset[TInputImage::ImageDimension-1] <= positiveOffset[TInputImage::ImageDimension-1])
    {
    offsets.push_back(iOffset);
    ++iOffset[0];
    for ( unsigned int i=0; i < TInputImage::ImageDimension - 1; ++i )
      {
      if (iOffset[i] > positiveOffset[i])
        {
        iOffset[i] = negativeOffset[i];
        ++iOffset[i+1];
        }
      }
    }

  // allocate acumulate line
  const size_t         ln =  outputRegionForThread.GetSize(0);
  AccumulatePixelType *accBuffer = ITK_NULLPTR;
  accBuffer = new AccumulatePixelType[ln];

  try
    {
    // convert the shrink factor for convenient multiplication
    typename TOutputImage::SizeType  factorSize;
    for ( unsigned int i=0; i < TInputImage::ImageDimension; ++i )
      {
      factorSize[i] = this->GetShrinkFactors()[i];
      }

    const size_t numSamples = std::accumulate( this->GetShrinkFactors().Begin(),
                                               this->GetShrinkFactors().End(),
                                               size_t(1),
                                               std::multiplies<size_t>() );
    const double inumSamples = 1.0 / (double)numSamples;

    const unsigned int numberOfLinesToProcess = outputRegionForThread.GetNumberOfPixels() /
      outputRegionForThread.GetSize(0);
    ProgressReporter progress(this, threadId, numberOfLinesToProcess );

    while ( !outputIterator.IsAtEnd() )
      {
      const OutputIndexType outputIndex = outputIterator.GetIndex();

      typename std::vector<OutputOffsetType>::const_iterator offset = offsets.begin();
      const InputIndexType startInputIndex = outputIndex * factorSize;

      inputIterator.SetIndex( startInputIndex+*offset );
      for( size_t i = 0; i < ln; ++i )
        {
        accBuffer[i] = inputIterator.Get();
        ++inputIterator;

        for ( size_t j = 1; j < factorSize[0]; ++j )
          {
          assert( !inputIterator.IsAtEndOfLine() );
          accBuffer[i] += inputIterator.Get();
          ++inputIterator;
          }
        }

      while ( ++offset != offsets.end() )
        {
        inputIterator.SetIndex( startInputIndex+*offset );
        // Note: If the output image is small then we might not split
        // the fastest direction. So we may not actually be at the start
        // of the line...
        //inputIterator.GoToBeginOfLine();

        for( size_t i = 0; i < ln; ++i )
          {
          for( size_t j = 0; j < factorSize[0]; ++j)
            {
            assert( !inputIterator.IsAtEndOfLine() );
            accBuffer[i] += inputIterator.Get();
            ++inputIterator;
            }
          }
        }

      for ( size_t j = 0; j <ln; ++j)
        {
        assert(!outputIterator.IsAtEndOfLine() );
        // this statement is made to work with RGB pixel types
        accBuffer[j] = accBuffer[j] * inumSamples;

        outputIterator.Set( RoundIfInteger< OutputPixelType >( accBuffer[j] ) );
        ++outputIterator;
        }

      outputIterator.NextLine();

      // Although the method name is CompletedPixel(),
      // this is being called after each line is processed
      progress.CompletedPixel();
      }

    }
  catch(...)
    {
    delete [] accBuffer;
    throw;
    }
  delete [] accBuffer;

}

template <class TInputImage, class TOutputImage>
void
BinShrinkImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImageType * inputPtr =
    const_cast< InputImageType * >( this->GetInput() );
  const OutputImageType * outputPtr = this->GetOutput();

  itkAssertInDebugAndIgnoreInReleaseMacro( inputPtr != ITK_NULLPTR );
  itkAssertInDebugAndIgnoreInReleaseMacro( outputPtr );

  // Compute the input requested region (size and start index)
  // Use the image transformations to insure an input requested region
  // that will provide the proper range
  const typename TOutputImage::SizeType& outputRequestedRegionSize = outputPtr->GetRequestedRegion().GetSize();
  const typename TOutputImage::IndexType& outputRequestedRegionStartIndex = outputPtr->GetRequestedRegion().GetIndex();

  typename TInputImage::IndexType  inputIndex0;
  typename TInputImage::SizeType   inputSize;

  for ( unsigned int i=0; i < TInputImage::ImageDimension; ++i )
    {
    inputIndex0[i] = outputRequestedRegionStartIndex[i]*m_ShrinkFactors[i];
    inputSize[i] = outputRequestedRegionSize[i]*m_ShrinkFactors[i];
    }

  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetIndex( inputIndex0 );
  inputRequestedRegion.SetSize( inputSize );

  // actually if we need to crop an exceptions should be thrown!
  //inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() );

  if ( !inputPtr->GetLargestPossibleRegion().IsInside( inputRequestedRegion.GetIndex() ) ||
       !inputPtr->GetLargestPossibleRegion().IsInside( inputRequestedRegion.GetUpperIndex() ) )
    {
    itkExceptionMacro( "Unexpected error calculating RR");
    }

  itkDebugMacro( "InputRequestedRegion: " << inputRequestedRegion );
  inputPtr->SetRequestedRegion( inputRequestedRegion );
}

template <class TInputImage, class TOutputImage>
void
BinShrinkImageFilter<TInputImage,TOutputImage>
::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  const InputImageType * inputPtr = this->GetInput();
  OutputImageType * outputPtr = this->GetOutput();

  itkAssertInDebugAndIgnoreInReleaseMacro( inputPtr );
  itkAssertInDebugAndIgnoreInReleaseMacro( outputPtr != ITK_NULLPTR );

  // Compute the output spacing, the output image size, and the
  // output image start index
  const typename TInputImage::SpacingType & inputSpacing = inputPtr->GetSpacing();
  const typename TInputImage::SizeType &   inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType &  inputStartIndex = inputPtr->GetLargestPossibleRegion().GetIndex();

  ContinuousIndex<double,ImageDimension> inputIndexOutputOrigin;

  typename TOutputImage::SpacingType outputSpacing(inputSpacing);
  typename TOutputImage::SizeType outputSize;
  typename TOutputImage::PointType outputOrigin;
  typename TOutputImage::IndexType outputStartIndex;

  for ( unsigned int i = 0; i < TOutputImage::ImageDimension; i++ )
    {
    outputSpacing[i] *= m_ShrinkFactors[i];

    inputIndexOutputOrigin[i] = 0.5*(m_ShrinkFactors[i]-1);

    outputStartIndex[i] = Math::Ceil<SizeValueType>(inputStartIndex[i]/static_cast<double>( m_ShrinkFactors[i]) );

    // Round down so that all output pixels fit input input region
    outputSize[i] =
      Math::Floor<SizeValueType>( (double)(inputSize[i] - outputStartIndex[i]*m_ShrinkFactors[i]+inputStartIndex[i])
                                  / (double)m_ShrinkFactors[i]);

    if ( outputSize[i] < 1 )
      {
      itkExceptionMacro("InputImage is too small! An output pixel does not map to a whole input bin.");
      }

    }

  inputPtr->TransformContinuousIndexToPhysicalPoint(inputIndexOutputOrigin, outputOrigin);

  outputPtr->SetSpacing(outputSpacing);
  outputPtr->SetOrigin(outputOrigin);

  // Set region
  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);

}

} // end namespace itk

#endif
