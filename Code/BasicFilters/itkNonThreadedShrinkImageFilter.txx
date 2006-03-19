/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNonThreadedShrinkImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNonThreadedShrinkImageFilter_txx
#define _itkNonThreadedShrinkImageFilter_txx

#include "itkNonThreadedShrinkImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
NonThreadedShrinkImageFilter<TInputImage,TOutputImage>
::NonThreadedShrinkImageFilter()
{
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_ShrinkFactors[j] = 1;
    }
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
NonThreadedShrinkImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Shrink Factor: ";
  for( unsigned int j = 0; j < ImageDimension; j++ )
    {
    os << m_ShrinkFactors[j] << " ";
    } 
  os << std::endl;
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
NonThreadedShrinkImageFilter<TInputImage,TOutputImage>
::SetShrinkFactors(unsigned int factors[])
{
  unsigned int j;
  for( j = 0; j < ImageDimension; j++ )
    {
    if( factors[j] != m_ShrinkFactors[j] ) break;
    }
  if( j < ImageDimension )
    {
    this->Modified();
    for( j = 0; j < ImageDimension; j++ )
      {
      m_ShrinkFactors[j] = factors[j];
      if( m_ShrinkFactors[j] < 1 ) 
        {
        m_ShrinkFactors[j] = 1;
        }
      }
    }
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
NonThreadedShrinkImageFilter<TInputImage,TOutputImage>
::SetShrinkFactors(unsigned int factor)
{
  unsigned int j;
  for( j = 0; j < ImageDimension; j++ )
    {
    if( factor != m_ShrinkFactors[j] ) break;
    }
  if( j < ImageDimension )
    {
    this->Modified();
    for( j = 0; j < ImageDimension; j++ )
      {
      m_ShrinkFactors[j] = factor;
      if( m_ShrinkFactors[j] < 1 ) 
        {
        m_ShrinkFactors[j] = 1;
        }
      }
    }
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
NonThreadedShrinkImageFilter<TInputImage,TOutputImage>
::GenerateData()
{
  itkDebugMacro(<<"Actually executing");

  // Get the input and output pointers
  typename Superclass::InputImageConstPointer  inputPtr = this->GetInput();
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();

  // Since we are providing a GenerateData() method, we need to allocate the
  // output buffer memory (if we provided a ThreadedGenerateData(), then
  // the memory would have already been allocated for us).
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Define/declare an iterator that will walk the output region
  typedef
    ImageRegionIterator<TOutputImage> OutputIterator;

  OutputIterator outIt = OutputIterator(outputPtr,
                                        outputPtr->GetRequestedRegion());

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  typename TOutputImage::IndexType outputIndex;
  typename TInputImage::IndexType inputIndex;
  typename TInputImage::SizeType factorSize;

  for (unsigned int i=0; i < TInputImage::ImageDimension; i++)
    {
    factorSize[i] = m_ShrinkFactors[i];
    }

  // support progress methods/callbacks
  ProgressReporter progress(this, 0, outputPtr->GetRequestedRegion().GetNumberOfPixels());

  // walk the output image, and sample the input image
  while ( !outIt.IsAtEnd() ) 
    {
    // determine the index of the output pixel
    outputIndex = outIt.GetIndex();

    // determine the input pixel location associated with this output pixel
    inputIndex = outputIndex * factorSize;

    // copy the input pixel to the output
    outIt.Set( inputPtr->GetPixel(inputIndex) );
    ++outIt;

    progress.CompletedPixel();
    }
}



/** 
 *
 */
template <class TInputImage, class TOutputImage>
void 
NonThreadedShrinkImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer  inputPtr = const_cast<TInputImage *> (this->GetInput());
  OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // we need to compute the input requested region (size and start index)
  const typename TOutputImage::SizeType& outputRequestedRegionSize
    = outputPtr->GetRequestedRegion().GetSize();
  const typename TOutputImage::IndexType& outputRequestedRegionStartIndex
    = outputPtr->GetRequestedRegion().GetIndex();
  
  typename TInputImage::SizeType  inputRequestedRegionSize;
  typename TInputImage::IndexType inputRequestedRegionStartIndex;
  
  for (unsigned int i = 0; i < TInputImage::ImageDimension; i++)
    {
    inputRequestedRegionSize[i]
      = outputRequestedRegionSize[i] * m_ShrinkFactors[i];
    inputRequestedRegionStartIndex[i]
      = outputRequestedRegionStartIndex[i] * (long)m_ShrinkFactors[i];
    }

  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetSize( inputRequestedRegionSize );
  inputRequestedRegion.SetIndex( inputRequestedRegionStartIndex );

  inputPtr->SetRequestedRegion( inputRequestedRegion );
}

/** 
 *
 */
template <class TInputImage, class TOutputImage>
void 
NonThreadedShrinkImageFilter<TInputImage,TOutputImage>
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  typename Superclass::InputImageConstPointer inputPtr = this->GetInput();
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();

  // we need to compute the output spacing, the output image size, and the
  // output image start index
  const typename TInputImage::SpacingType&
    inputSpacing = inputPtr->GetSpacing();
  const typename TInputImage::SizeType&   inputSize
    = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType&  inputStartIndex
    = inputPtr->GetLargestPossibleRegion().GetIndex();

  typename TOutputImage::SpacingType      outputSpacing;
  typename TOutputImage::SizeType         outputSize;
  typename TOutputImage::IndexType        outputStartIndex;
  
  for (unsigned int i = 0; i < TOutputImage::ImageDimension; i++)
    {
    outputSpacing[i] = inputSpacing[i] * (double) m_ShrinkFactors[i];
    outputSize[i] = (unsigned int)
      vcl_floor(((double)(inputSize[i] - m_ShrinkFactors[i] + 1))
             / (double) m_ShrinkFactors[i]);
    outputStartIndex[i] = (int)
      vcl_ceil((double) inputStartIndex[i] / (double) m_ShrinkFactors[i] );
    }

  outputPtr->SetSpacing( outputSpacing );

  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize( outputSize );
  outputLargestPossibleRegion.SetIndex( outputStartIndex );

  outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );
}

} // end namespace itk

#endif
