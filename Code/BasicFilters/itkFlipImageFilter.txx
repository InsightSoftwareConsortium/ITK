/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFlipImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkFlipImageFilter_txx
#define _itkFlipImageFilter_txx

#include "itkFlipImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkExceptionObject.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 * Constructor
 */
template <class TImage>
FlipImageFilter<TImage>
::FlipImageFilter()
{

  m_FlipAxes.Fill( false );

}


/**
 * PrintSelf
 */
template <class TImage>
void 
FlipImageFilter<TImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "FlipAxes: " << m_FlipAxes << std::endl;

}


/**
 * The output image meta information is obtained by permuting
 * the input image meta information.
 */
template <class TImage>
void
FlipImageFilter<TImage>
::GenerateOutputInformation()
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  InputImagePointer inputPtr = 
      const_cast< TImage * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();

  const double *inputSpacing = inputPtr->GetSpacing();
  const double *inputOrigin = inputPtr->GetOrigin();
  const typename TImage::SizeType& inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TImage::IndexType& inputStartIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();
    
  double outputOrigin[ImageDimension];

  unsigned int j;

  for ( j = 0; j < ImageDimension; j++ )
    {
    if ( m_FlipAxes[j] )
      {
        outputOrigin[j] = - 1 * inputOrigin[j] -
          inputSpacing[j] * ( 2 * static_cast<double>( inputStartIndex[j] ) + 
          static_cast<double>( inputSize[j] ) - 1.0 );
      }
    else
      {
      outputOrigin[j] = inputOrigin[j];
      }
    }

  outputPtr->SetOrigin( outputOrigin );

  
}


/**
 * The required input requested region is obtained by permuting
 * the index and size of the output requested region
 */
template <class TImage>
void
FlipImageFilter<TImage>
::GenerateInputRequestedRegion()
{
  // call the superclass's implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer inputPtr = 
      const_cast< TImage * >( this->GetInput());
  OutputImagePointer outputPtr = this->GetOutput();

  const typename TImage::SizeType& outputRequestedSize =
    outputPtr->GetRequestedRegion().GetSize();
  const typename TImage::IndexType& outputRequestedIndex =
    outputPtr->GetRequestedRegion().GetIndex();
  
  const typename TImage::SizeType & outputLargestPossibleSize =
    outputPtr->GetLargestPossibleRegion().GetSize();
  const typename TImage::IndexType& outputLargestPossibleIndex =
    outputPtr->GetLargestPossibleRegion().GetIndex();
  
  IndexType inputRequestedIndex;

  unsigned int j;
  for ( j = 0; j < ImageDimension; j++ )
    {
    if ( m_FlipAxes[j] )
      {
      inputRequestedIndex[j] =
          2 * outputLargestPossibleIndex[j] 
        + static_cast<IndexValueType>( outputLargestPossibleSize[j] )
        - static_cast<IndexValueType>( outputRequestedSize[j] ) 
        - outputRequestedIndex[j];
      }
    else
      {
      inputRequestedIndex[j] = outputRequestedIndex[j];
      }
    }

  typename TImage::RegionType inputRequestedRegion;
  inputRequestedRegion.SetSize( outputRequestedSize );
  inputRequestedRegion.SetIndex( inputRequestedIndex );

  inputPtr->SetRequestedRegion( inputRequestedRegion );


}


/**
 *
 */
template <class TImage>
void
FlipImageFilter<TImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{

  unsigned long i;
  unsigned int j;

  // Get the input and output pointers
  InputImageConstPointer inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();

  // Setup output region iterator
  typedef ImageRegionIteratorWithIndex<TImage> OutputIterator;
  OutputIterator outIt(outputPtr, outputRegionForThread);

  typename TImage::IndexType outputIndex;
  typename TImage::IndexType inputIndex;

  // support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
    
  const typename TImage::SizeType & outputLargestPossibleSize =
    outputPtr->GetLargestPossibleRegion().GetSize();
  const typename TImage::IndexType& outputLargestPossibleIndex =
    outputPtr->GetLargestPossibleRegion().GetIndex();

  IndexValueType offset[ImageDimension];
  for ( j = 0; j < ImageDimension; j++ )
    {
    if ( m_FlipAxes[j] )
      {
      offset[j] = 2 * outputLargestPossibleIndex[j] 
         + static_cast<IndexValueType>( outputLargestPossibleSize[j] ) - 1;
      } 
    }

  // walk the output region, and sample the input image
  for ( i = 0; !outIt.IsAtEnd(); ++outIt, i++ )
    {
    // determine the index of the output pixel
    outputIndex = outIt.GetIndex();
    
    // determine the input pixel location associated with this output pixel
    for ( j = 0; j < ImageDimension; j++ )
      {
      if ( m_FlipAxes[j] )
        {
        inputIndex[j] = - 1 * outputIndex[j] + offset[j];
        }
      else 
        {
        inputIndex[j] = outputIndex[ j ];
        }
      }
    
    // copy the input pixel to the output
    outIt.Set( inputPtr->GetPixel(inputIndex) );
    progress.CompletedPixel();
    }

}


} // namespace itk

#endif
