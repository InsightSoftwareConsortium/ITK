/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPermuteAxesImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPermuteAxesImageFilter_txx
#define _itkPermuteAxesImageFilter_txx

#include "itkPermuteAxesImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkExceptionObject.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 * Constructor
 */
template <class TImage>
PermuteAxesImageFilter<TImage>
::PermuteAxesImageFilter()
{
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    m_Order[j] = j;
    m_InverseOrder[m_Order[j]] = j;
    }

}


/**
 * PrintSelf
 */
template <class TImage>
void 
PermuteAxesImageFilter<TImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  unsigned int j;

  os << indent << "Order: [";
  for( j = 0; j < ImageDimension - 1; j++ )
    {
    os << m_Order[j] << ", ";
    }
  os << m_Order[j] << "]" << std::endl;

  os << indent << "InverseOrder: [";
  for( j = 0; j < ImageDimension - 1; j++ )
    {
    os << m_InverseOrder[j] << ", ";
    }
  os << m_InverseOrder[j] << "]" << std::endl;


}


/**
 * Set the permutation order
 */
template <class TImage>
void
PermuteAxesImageFilter<TImage>
::SetOrder( const PermuteOrderArrayType& order )
{

  unsigned int j; 

  // check if it the same as current
  if ( m_Order == order ) return;

  // check that input is a rearrangement of the
  // numbers from 0 to ImageDimension - 1
  FixedArray<bool,ImageDimension> used;
  used.Fill( false );

  for ( j = 0; j < ImageDimension; j++ )
    {
    if ( order[j] > ImageDimension - 1 )
      {
      ExceptionObject err(__FILE__, __LINE__);
      err.SetLocation( "PermuteAxesImageFilter::SetOrder" );
      err.SetDescription( "Order indices is out of range" );
      throw err;
      }
    else if ( used[order[j]] )
      {
      ExceptionObject err(__FILE__, __LINE__);
      err.SetLocation( "PermuteAxesImageFilter::SetOrder" );
      err.SetDescription( "Order indices must not repeat" );
      throw err;
      }
    used[order[j]] = true;
    }

  // copy to member variable
  this->Modified();
  m_Order = order;
  for ( j = 0; j < ImageDimension; j++ )
    {
    m_InverseOrder[m_Order[j]] = j;
    }
  
}


/**
 * The output image meta information is obtained by permuting
 * the input image meta information.
 */
template <class TImage>
void
PermuteAxesImageFilter<TImage>
::GenerateOutputInformation()
{
  // call the superclass's implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  typename Superclass::InputImageConstPointer inputPtr = this->GetInput();
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();

  const double *inputSpacing = inputPtr->GetSpacing();
  const double *inputOrigin = inputPtr->GetOrigin();
  const typename TImage::SizeType& inputSize =
    inputPtr->GetLargestPossibleRegion().GetSize();
  const typename TImage::IndexType& inputStartIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();
    

  double outputSpacing[ImageDimension];
  double outputOrigin[ImageDimension];
  typename TImage::SizeType outputSize;
  typename TImage::IndexType outputStartIndex;

  unsigned int j;

  for ( j = 0; j < ImageDimension; j++ )
    {
    outputSpacing[j] = inputSpacing[m_Order[j]];
    outputOrigin[j]  = inputOrigin[m_Order[j]];
    outputSize[j]    = inputSize[m_Order[j]];
    outputStartIndex[j] = inputStartIndex[m_Order[j]];
    }

  outputPtr->SetSpacing( outputSpacing );
  outputPtr->SetOrigin( outputOrigin );

  typename TImage::RegionType outputRegion;
  outputRegion.SetSize( outputSize );
  outputRegion.SetIndex( outputStartIndex );
   
  outputPtr->SetLargestPossibleRegion( outputRegion );
  
}


/**
 * The required input requested region is obtained by permuting
 * the index and size of the output requested region
 */
template <class TImage>
void
PermuteAxesImageFilter<TImage>
::GenerateInputRequestedRegion()
{
  // call the superclass's implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer inputPtr = 
    const_cast< TImage * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();

  const typename TImage::SizeType& outputSize =
    outputPtr->GetRequestedRegion().GetSize();
  const typename TImage::IndexType& outputIndex =
    outputPtr->GetRequestedRegion().GetIndex();

  typename TImage::SizeType inputSize;
  typename TImage::IndexType inputIndex;

  unsigned int j;
  for ( j = 0; j < ImageDimension; j++ )
    {
    inputSize[j] = outputSize[m_InverseOrder[j]];
    inputIndex[j] = outputIndex[m_InverseOrder[j]];
    }

  typename TImage::RegionType inputRegion;
  inputRegion.SetSize( inputSize );
  inputRegion.SetIndex( inputIndex );

  inputPtr->SetRequestedRegion( inputRegion );

}


/**
 *
 */
template <class TImage>
void
PermuteAxesImageFilter<TImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  unsigned long i;
  unsigned int j;

  // Get the input and output pointers
  typename Superclass::InputImageConstPointer inputPtr = this->GetInput();
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();

  // Setup output region iterator
  typedef ImageRegionIteratorWithIndex<TImage> OutputIterator;
  OutputIterator outIt(outputPtr, outputRegionForThread);

  typename TImage::IndexType outputIndex;
  typename TImage::IndexType inputIndex;

  // support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
    
  // walk the output region, and sample the input image
  for ( i = 0; !outIt.IsAtEnd(); ++outIt, i++ )
    {
    // determine the index of the output pixel
    outputIndex = outIt.GetIndex();
    
    // determine the input pixel location associated with this output pixel
    for ( j = 0; j < ImageDimension; j++ )
      {
      inputIndex[j] = outputIndex[ m_InverseOrder[j] ];
      }
    
    // copy the input pixel to the output
    outIt.Set( inputPtr->GetPixel(inputIndex) );
    progress.CompletedPixel();
    }

}


} // namespace itk

#endif
