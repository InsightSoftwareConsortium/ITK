/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShrinkImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkShrinkImageFilter_txx
#define _itkShrinkImageFilter_txx

#include "itkShrinkImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
ShrinkImageFilter<TInputImage,TOutputImage>
::ShrinkImageFilter()
{
  for( int j = 0; j < ImageDimension; j++ )
    {
    m_ShrinkFactors[j] = 1;
    }
}
  

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ShrinkImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Shrink Factor: ";
  for( int j = 0; j < ImageDimension; j++ )
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
ShrinkImageFilter<TInputImage,TOutputImage>
::SetShrinkFactors(unsigned int factors[])
{
  int j = 0;
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
ShrinkImageFilter<TInputImage,TOutputImage>
::SetShrinkFactors(unsigned int factor)
{
  int j = 0;
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
ShrinkImageFilter<TInputImage,TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  int i;
  
  itkDebugMacro(<<"Actually executing");
  
  // Get the input and output pointers
  InputImageConstPointer  inputPtr = this->GetInput();
  OutputImagePointer      outputPtr = this->GetOutput();
  
  // Define/declare an iterator that will walk the output region for this
  // thread.
  typedef
    ImageRegionIterator<TOutputImage> OutputIterator;
  
  OutputIterator outIt(outputPtr, outputRegionForThread);
  
  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  typename TOutputImage::IndexType outputIndex;
  typename TInputImage::IndexType inputIndex;
  typename TOutputImage::SizeType factorSize;
  
  for (i=0; i < TInputImage::ImageDimension; i++)
    {
    factorSize[i] = m_ShrinkFactors[i];
    }
  
  // support progress methods/callbacks
  unsigned long updateVisits = 0;
  unsigned long totalPixels = 0;
  if ( threadId == 0 )
    {
    totalPixels = outputRegionForThread.GetNumberOfPixels();
    updateVisits = totalPixels / 10;
    if( updateVisits < 1 ) updateVisits = 1;
    }
  
  // walk the output region, and sample the input image
  for (i=0; !outIt.IsAtEnd(); ++outIt, i++ )
    {
    if ( threadId == 0 && !(i % updateVisits ) )
        {
        this->UpdateProgress((float)i / (float)totalPixels);
        }
    
    // determine the index of the output pixel
    outputIndex = outIt.GetIndex();
    
    // determine the input pixel location associated with this output pixel
    inputIndex = outputIndex * factorSize;
    
    // copy the input pixel to the output
    outIt.Set( inputPtr->GetPixel(inputIndex) );
    }
}



/** 
 *
 */
template <class TInputImage, class TOutputImage>
void 
ShrinkImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer  inputPtr = 
      const_cast< TInputImage * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();
  
  if ( !inputPtr || !outputPtr )
    {
    return;
    }
  
  // we need to compute the input requested region (size and start index)
  int i;
  const typename TOutputImage::SizeType& outputRequestedRegionSize
    = outputPtr->GetRequestedRegion().GetSize();
  const typename TOutputImage::IndexType& outputRequestedRegionStartIndex
    = outputPtr->GetRequestedRegion().GetIndex();
  
  typename TInputImage::SizeType  inputRequestedRegionSize;
  typename TInputImage::IndexType inputRequestedRegionStartIndex;
  
  for (i = 0; i < TInputImage::ImageDimension; i++)
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
ShrinkImageFilter<TInputImage,TOutputImage>
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();
  
  // get pointers to the input and output
  InputImageConstPointer  inputPtr  = this->GetInput();
  OutputImagePointer      outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }
  
  // we need to compute the output spacing, the output image size, and the
  // output image start index
  int i;
  const double             *inputSpacing = inputPtr->GetSpacing();
  const typename TInputImage::SizeType&   inputSize
    = inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TInputImage::IndexType&  inputStartIndex
    = inputPtr->GetLargestPossibleRegion().GetIndex();
  
  float    outputSpacing[TOutputImage::ImageDimension];
  typename TOutputImage::SizeType     outputSize;
  typename TOutputImage::IndexType    outputStartIndex;
  
  for (i = 0; i < TOutputImage::ImageDimension; i++)
    {
    
    outputSpacing[i] = inputSpacing[i] * (float) m_ShrinkFactors[i];
    outputSize[i] = (unsigned long)
      floor( (float) inputSize[i] / (float) m_ShrinkFactors[i]);
    if( outputSize[i] < 1 )
      {
      outputSize[i] = 1;
      }
    
    outputStartIndex[i] = (long)
      ceil( (float) inputStartIndex[i] / (float) m_ShrinkFactors[i] );
    }
  
  outputPtr->SetSpacing( outputSpacing );
  
  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize( outputSize );
  outputLargestPossibleRegion.SetIndex( outputStartIndex );
  
  outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );
}

} // end namespace itk

#endif
