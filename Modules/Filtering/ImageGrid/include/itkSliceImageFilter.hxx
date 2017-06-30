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
#ifndef itkSliceImageFilter_hxx
#define itkSliceImageFilter_hxx

#include "itkSliceImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"
#include "itkContinuousIndex.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"

namespace itk
{

template< class TInputImage, class TOutputImage >
SliceImageFilter< TInputImage, TOutputImage >
::SliceImageFilter()
{
  m_Start.Fill(NumericTraits<IndexValueType>::min());
  m_Stop.Fill(NumericTraits<IndexValueType>::max());
  m_Step.Fill(1);
}

template< class TInputImage, class TOutputImage >
void
SliceImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Start: " << m_Start << std::endl;
  os << indent << "Stop: " << m_Stop << std::endl;
  os << indent << "Step: " << m_Step << std::endl;

}


template< class TInputImage, class TOutputImage >
void
SliceImageFilter< TInputImage, TOutputImage >
::SetStart( typename TInputImage::IndexType::IndexValueType start)
{
  unsigned int j;

  for ( j = 0; j < ImageDimension; ++j )
    {
    if ( start != m_Start[j] )
      {
      this->Modified();
      m_Start.Fill( start );
      return;
      }
    }
}


template< class TInputImage, class TOutputImage >
void
SliceImageFilter< TInputImage, TOutputImage >
::SetStop(typename TInputImage::IndexType::IndexValueType  stop)
{
  unsigned int j;

  for ( j = 0; j < ImageDimension; ++j )
    {
    if ( stop != m_Stop[j] )
      {
      this->Modified();
      m_Stop.Fill( stop );
      return;
      }
    }
}


template< class TInputImage, class TOutputImage >
void
SliceImageFilter< TInputImage, TOutputImage >
::SetStep(int step)
{
  unsigned int j;

  for ( j = 0; j < ImageDimension; ++j )
    {
    if ( step != m_Step[j] )
      {
      this->Modified();
      m_Step.Fill( step );
      return;
      }
    }

}


template< class TInputImage, class TOutputImage >
void
SliceImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  // Get the input and output pointers
  InputImageConstPointer inputPtr = this->GetInput();
  OutputImagePointer     outputPtr = this->GetOutput();

  // Support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  const typename TInputImage::SizeType &inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType &inputIndex = inputPtr->GetLargestPossibleRegion().GetIndex();

  // clamp start
  InputIndexType start;
  for ( unsigned int i = 0; i < TOutputImage::ImageDimension; ++i )
    {
    start[i] = std::max( m_Start[i], inputIndex[i] );
    start[i] = std::min( start[i], static_cast<IndexValueType>(inputIndex[i] + inputSize[i]-1) );
    }

  // Define/declare an iterator that will walk the output region for this
  // thread.
  typedef ImageRegionIteratorWithIndex< TOutputImage > OutputIterator;
  OutputIterator outIt(outputPtr, outputRegionForThread);

  OutputIndexType destIndex;
  InputIndexType srcIndex;

  while ( !outIt.IsAtEnd() )
    {
    // Determine the index and physical location of the output pixel
    destIndex = outIt.GetIndex();


    for( unsigned int i = 0; i < TOutputImage::ImageDimension; ++i )
      {
      srcIndex[i] = destIndex[i]*m_Step[i] + start[i];
      }

    // Copy the input pixel to the output
    outIt.Set( inputPtr->GetPixel(srcIndex) );
    ++outIt;

    progress.CompletedPixel();
    }
}


template< class TInputImage, class TOutputImage >
void
SliceImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{


  // Get pointers to the input and output
  InputImagePointer  inputPtr = const_cast< TInputImage * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();

  const typename TOutputImage::SizeType & outputRequestedRegionSize = outputPtr->GetRequestedRegion().GetSize();
  const typename TOutputImage::IndexType & outputRequestedRegionStartIndex = outputPtr->GetRequestedRegion().GetIndex();

  const typename TInputImage::SizeType  &inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType &inputIndex = inputPtr->GetLargestPossibleRegion().GetIndex();

  // clamp start
  InputIndexType start;
  for ( unsigned int i = 0; i < TOutputImage::ImageDimension; ++i )
    {
    // clamp to valid index range and don't include one past end, so
    // that a zero size RR would be valid
    start[i] = std::max( m_Start[i], inputIndex[i] );
    start[i] = std::min( start[i], static_cast<IndexValueType>(inputIndex[i] + inputSize[i] -1) );
    }


  typename TInputImage::SizeType inputRequestedRegionSize;
  inputRequestedRegionSize.Fill(0);
  for ( unsigned int i=0; i < TInputImage::ImageDimension; ++i )
    {
    if ( outputRequestedRegionSize[i] > 0 )
      {
      inputRequestedRegionSize[i] = (outputRequestedRegionSize[i] - 1 ) * itk::Math::abs(m_Step[i]) + 1;
      }
    }

  InputIndexType  inputRequestedRegionIndex;
  for ( unsigned int i=0; i < TOutputImage::ImageDimension; ++i )
    {
    inputRequestedRegionIndex[i] = outputRequestedRegionStartIndex[i] * m_Step[i] + start[i];

    // if reversing, go to the lower ending index - 1
    if (m_Step[i] < 0)
      {
      inputRequestedRegionIndex[i] -= inputRequestedRegionSize[i] - 1;
      }
    }


  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetIndex(inputRequestedRegionIndex);
  inputRequestedRegion.SetSize(inputRequestedRegionSize);

  // test if input RR is completely inside input largest region
  if ( inputRequestedRegion.GetNumberOfPixels() > 0 &&
       !inputPtr->GetLargestPossibleRegion().IsInside( inputRequestedRegion ) )
    {
    itkExceptionMacro( "Logic Error: incorrect computation of RequestedRegion" );
    }

  inputPtr->SetRequestedRegion(inputRequestedRegion);
  return;

}


template< class TInputImage, class TOutputImage >
void
SliceImageFilter< TInputImage, TOutputImage >
::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  InputImageConstPointer inputPtr  = this->GetInput();
  OutputImagePointer     outputPtr = this->GetOutput();

  // Compute the output spacing, the output image size, and the
  // output image start index
  const typename TInputImage::SpacingType &inputSpacing = inputPtr->GetSpacing();
  const typename TInputImage::SizeType &inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType &inputIndex = inputPtr->GetLargestPossibleRegion().GetIndex();
  typename TInputImage::IndexType inputStartIndex;

  typename TOutputImage::SpacingType outputSpacing;
  typename TOutputImage::SizeType outputSize;

  typename TOutputImage::IndexType outputStartIndex;
  outputStartIndex.Fill(0);

  for ( unsigned int i = 0; i < TOutputImage::ImageDimension; ++i )
    {
    outputSpacing[i] = inputSpacing[i] * itk::Math::abs(m_Step[i]);

    // clamp start
    // Based on the sign of the step include 1 after the end.
    IndexValueType start = std::max( m_Start[i], inputIndex[i] - int(m_Step[i]<0));
    start = std::min( start,  static_cast<IndexValueType>(inputIndex[i] + inputSize[i]) - int(m_Step[i]<0) );

    // clamp stop
    // Based on the sign of the step include 1 after the end.
    IndexValueType stop = std::max( m_Stop[i], inputIndex[i] - int(m_Step[i]<0) );
    stop = std::min( stop,  static_cast<IndexValueType>(inputIndex[i] + inputSize[i]) - int(m_Step[i]<0));

    // If both the numerator and the denominator have the same sign,
    // then the range is a valid and non-zero sized. Truncation is the
    // correct rounding for these positive values.
    if ( (m_Step[i] > 0 && stop > start) ||
         ( m_Step[i] < 0 && stop < start ) )
      {
      outputSize[i] = (stop-start)/m_Step[i];
      }
    else
      {
      outputSize[i] = 0u;
      }

    // If the step is negative, then the start is still the index of
    // the output origin
    inputStartIndex[i] = start;

    }

  const typename TInputImage::DirectionType & inputDirection = inputPtr->GetDirection();
  typename TInputImage::DirectionType flipMatrix;

  // Need a matrix to model the reversing of directions, this should
  // maintain the physical location of the pixels
  for ( unsigned int j = 0; j < ImageDimension; ++j )
    {
    flipMatrix[j][j] = itk::Math::sgn0(m_Step[j]);
    }

  outputPtr->SetDirection(inputDirection * flipMatrix);
  outputPtr->SetSpacing(outputSpacing);

  typename TOutputImage::PointType outputOrigin;
  inputPtr->TransformIndexToPhysicalPoint(inputStartIndex, outputOrigin);
  outputPtr->SetOrigin(outputOrigin);

  // Set region
  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}


template< class TInputImage, class TOutputImage >
void
SliceImageFilter< TInputImage, TOutputImage >
::VerifyInputInformation()
{

  Superclass::VerifyInputInformation();

  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    if ( m_Step[i] == 0 )
      {
      itkExceptionMacro( "Step size is zero " << m_Step << "!" );
      }
    }

}

} // end namespace itk

#endif
