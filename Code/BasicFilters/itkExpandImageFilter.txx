/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExpandImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkExpandImageFilter_txx
#define _itkExpandImageFilter_txx

#include "itkExpandImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkExceptionObject.h"
#include "itkObjectFactory.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 * Default constructor
 */
template <class TInputImage, class TOutputImage>
ExpandImageFilter<TInputImage,TOutputImage>
::ExpandImageFilter()
{

  // Set default factors to 1
  for(unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_ExpandFactors[j] = 1;
    }

  // Setup the default interpolator
  typename DefaultInterpolatorType::Pointer interp =
    DefaultInterpolatorType::New();

  m_Interpolator = static_cast<InterpolatorType*>(
    interp.GetPointer() );

  // Set default padding value to zero
  m_EdgePaddingValue = NumericTraits<OutputPixelType>::Zero;

}


/**
 * Standard "PrintSelf" method
 */
template <class TInputImage, class TOutputImage>
void
ExpandImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  unsigned int j;
  os << indent << "ExpandFactors: [" ;
  for( j = 0; j < ImageDimension - 1; j++ )
    {
    os << m_ExpandFactors[j] << ", ";
    }
  os << m_ExpandFactors[j] << "]" << std::endl;
  
  os << indent << "Interpolator: ";
  os << m_Interpolator.GetPointer() << std::endl;

  os << indent << "EdgePaddingValue: "
     << static_cast<NumericTraits<OutputPixelType>::PrintType>(m_EdgePaddingValue)
     << std::endl;
  os << indent << "EdgePaddingValue: ";
  os << m_EdgePaddingValue << std::endl;

}


/**
 * Set expand factors from an array of unsigned int.
 */
template <class TInputImage, class TOutputImage>
void
ExpandImageFilter<TInputImage,TOutputImage>
::SetExpandFactors(
const unsigned int factors[] )
{

  unsigned int j;
  for( j = 0; j < ImageDimension; j++ )
    {
    if( factors[j] != m_ExpandFactors[j] ) break;
    }
  if( j < ImageDimension )
    {
    this->Modified();
    for( j = 0; j < ImageDimension; j++ )
      {
      m_ExpandFactors[j] = factors[j];
      if( m_ExpandFactors[j] < 1 ) m_ExpandFactors[j] = 1;
      }
    }

}


/**
 * Set expand factors from a single unsigned int
 */
template <class TInputImage, class TOutputImage>
void
ExpandImageFilter<TInputImage,TOutputImage>
::SetExpandFactors(
const unsigned int factor )
{

  unsigned int j;
  for( j = 0; j < ImageDimension; j++ )
    {
    if( factor != m_ExpandFactors[j] ) break;
    }
  if( j < ImageDimension )
    {
    this->Modified();
    for( j = 0; j < ImageDimension; j++ )
      {
      m_ExpandFactors[j] = factor;
      if( m_ExpandFactors[j] < 1 ) m_ExpandFactors[j] = 1;
      }
    }

}


/**
 * BeforeThreadedGenerateData
 */
template <class TInputImage, class TOutputImage>
void
ExpandImageFilter<TInputImage,TOutputImage>
::BeforeThreadedGenerateData()
{

  if( !m_Interpolator || !this->GetInput() )
    {
    itkExceptionMacro(<< "Interpolator and/or Input not set");
    throw ExceptionObject(__FILE__,__LINE__ );
    }

  // Connect input image to interpolator
  m_Interpolator->SetInputImage( this->GetInput() );

}


/**
 * ThreadedGenerateData
 */
template <class TInputImage, class TOutputImage>
void
ExpandImageFilter<TInputImage,TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                         int threadId)
{
  int i;

  // Get the input and output pointers
  OutputImagePointer outputPtr = this->GetOutput();

  // Iterator for walking the output
  typedef
    ImageRegionIteratorWithIndex<TOutputImage> OutputIterator;

  OutputIterator outIt( outputPtr, outputRegionForThread );

  // Define a few indices that will be used to translate from an input 
  // pixel to and output pixel
  typename TOutputImage::IndexType outputIndex;
  typename InterpolatorType::ContinuousIndexType inputIndex;

  typedef typename TOutputImage::PixelType OutputPixelType;

  // Support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
  
  // Walk the output region, and interpolate the input image
  for( i = 0; !outIt.IsAtEnd(); ++outIt, i++ )
    {
    // Determine the index of the output pixel
    outputIndex = outIt.GetIndex();

    // Determine the input pixel location associated with this output pixel.
    // Don't need to check for division by zero because the factors are
    // clamped to be minimum for 1.
    for( unsigned int j = 0; j < ImageDimension; j++ )
      {
      inputIndex[j] = (double) outputIndex[j] /
        (double) m_ExpandFactors[j];
      }
    
    // interpolate value and write to output
    if( m_Interpolator->IsInsideBuffer( inputIndex ) )
      {
      outIt.Set( static_cast<OutputPixelType>( 
        m_Interpolator->EvaluateAtContinuousIndex( inputIndex ) ) );
      }
    else
      {
      outIt.Set( m_EdgePaddingValue );
      }
    progress.CompletedPixel();
    } 
 
}


/**
 * GenerateInputRequesteRegion 
 */
template <class TInputImage, class TOutputImage>
void
ExpandImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // Get pointers to the input and output
  InputImagePointer inputPtr = 
      const_cast< TInputImage * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // We need to compute the input requested region (size and start index)
  unsigned int i;
  const typename TOutputImage::SizeType& outputRequestedRegionSize
    = outputPtr->GetRequestedRegion().GetSize();
  const typename TOutputImage::IndexType& outputRequestedRegionStartIndex
    = outputPtr->GetRequestedRegion().GetIndex();

  typename TInputImage::SizeType  inputRequestedRegionSize;
  typename TInputImage::IndexType inputRequestedRegionStartIndex;

  /**
   * inputRequestedSize = (outputRequestedSize / ExpandFactor) + 1)
   * The extra 1 above is to take care of edge effects when streaming.
   */
  for (i = 0; i < TInputImage::ImageDimension; i++)
    {
    inputRequestedRegionSize[i]
      = (long) ceil( (double)outputRequestedRegionSize[i] / 
          (double) m_ExpandFactors[i] ) + 1;

    inputRequestedRegionStartIndex[i]
      = (long) floor( (double)outputRequestedRegionStartIndex[i] / 
          (double)m_ExpandFactors[i] );
    }


  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetSize( inputRequestedRegionSize );
  inputRequestedRegion.SetIndex( inputRequestedRegionStartIndex );

  // Make sure the requested region is within largest possible.
  inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() );

  // Set the input requested region.
  inputPtr->SetRequestedRegion( inputRequestedRegion );

}


/**
 * GenerateOutputInformation
 */
template <class TInputImage, class TOutputImage>
void
ExpandImageFilter<TInputImage,TOutputImage>
::GenerateOutputInformation()
{

  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // Get pointers to the input and output
  InputImagePointer inputPtr = 
      const_cast< TInputImage * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // We need to compute the output spacing, the output image size, and the
  // output image start index
  const double  *inputSpacing = inputPtr->GetSpacing();
  const typename TInputImage::SizeType&   inputSize
    = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType&  inputStartIndex
    = inputPtr->GetLargestPossibleRegion().GetIndex();

  float    outputSpacing[TOutputImage::ImageDimension];
  typename TOutputImage::SizeType     outputSize;
  typename TOutputImage::IndexType    outputStartIndex;

  for (unsigned int i = 0; i < TOutputImage::ImageDimension; i++)
    {
    outputSpacing[i] = inputSpacing[i] / (float) m_ExpandFactors[i];
    outputSize[i] = inputSize[i] * (unsigned long) m_ExpandFactors[i];
    outputStartIndex[i] = inputStartIndex[i] * (long) m_ExpandFactors[i];
    }

  outputPtr->SetSpacing( outputSpacing );

  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize( outputSize );
  outputLargestPossibleRegion.SetIndex( outputStartIndex );

  outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );

}


} // end namespace itk

#endif
