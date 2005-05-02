/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCheckerBoardImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#ifndef _itkCheckerBoardImageFilter_txx
#define _itkCheckerBoardImageFilter_txx

#include "itkCheckerBoardImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkObjectFactory.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 * Initialize new instance
 */
template <class TImage>
CheckerBoardImageFilter<TImage>
::CheckerBoardImageFilter()
{
  m_CheckerPattern.Fill( 4 );
}


/**
 * Print out a description of self
 *
 * \todo Add details about this class
 */
template <class TImage>
void 
CheckerBoardImageFilter<TImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Checker pattern: " << m_CheckerPattern << std::endl;
  return;
}


/**
 * Connect one of the operands for checkerboard operation
 */
template <class TImage >
void
CheckerBoardImageFilter<TImage>
::SetInput1( const InputImageType * image1 ) 
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput(0, const_cast<InputImageType *>( image1 ));
}


/**
 * Connect one of the operands for checkerboard operation
 */
template <class TImage >
void
CheckerBoardImageFilter<TImage>
::SetInput2( const InputImageType * image2 ) 
{
  // Process object is not const-correct so the const casting is required.
  this->SetNthInput(1, const_cast<InputImageType *>( image2 ));
}


/**
 * ThreadedGenerateData
 */
template <class TImage>
void 
CheckerBoardImageFilter<TImage>
::ThreadedGenerateData(
  const ImageRegionType& outputRegionForThread, int threadId)
{
  unsigned int i;
  
  itkDebugMacro(<<"Actually executing");

  // Get the output pointers
  OutputImagePointer      outputPtr = this->GetOutput();
  InputImageConstPointer  input1Ptr = this->GetInput(0);
  InputImageConstPointer  input2Ptr = this->GetInput(1);

  // Create an iterator that will walk the output region for this thread.
  typedef ImageRegionIteratorWithIndex< OutputImageType > OutputIterator;
  typedef ImageRegionConstIteratorWithIndex< InputImageType > InputIterator;

  OutputIterator outItr( outputPtr, outputRegionForThread );
  InputIterator  in1Itr( input1Ptr, outputRegionForThread );
  InputIterator  in2Itr( input2Ptr, outputRegionForThread );

  outItr.GoToBegin();
  in1Itr.GoToBegin();
  in2Itr.GoToBegin();

  // Support for progress methods/callbacks
  ProgressReporter progress( this, threadId, 
                             outputRegionForThread.GetNumberOfPixels());
        
  typename InputImageType::SizeType size = 
        input2Ptr->GetLargestPossibleRegion().GetSize();

  PatternArrayType factors;

  for(unsigned int d=0; d<ImageDimension; d++)
    {
    factors[d] = size[d] / m_CheckerPattern[d];
    }

  typedef typename InputImageType::PixelType PixelType;
  typedef typename InputImageType::IndexType IndexType;
  
  PixelType pixval;
 
  // Walk the output region
  while ( !outItr.IsAtEnd() )
    {
    const IndexType & index = outItr.GetIndex();

    unsigned int sum = 0;

    for(unsigned int i=0; i<ImageDimension; i++)
      {
      sum += static_cast< unsigned int >( index[i] / factors[i] );
      }

    if( sum & 1 )
      {
      pixval = in2Itr.Get();
      }
    else 
      {
      pixval = in1Itr.Get();
      }

    outItr.Set( pixval );      

    progress.CompletedPixel();

    ++outItr;
    ++in1Itr;
    ++in2Itr;
    }

  return;
}


} // end namespace itk

#endif
