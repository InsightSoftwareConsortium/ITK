/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkResampleImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#ifndef _itkResampleImageFilter_txx
#define _itkResampleImageFilter_txx

#include "itkResampleImageFilter.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 * Initialize new instance to obviously undefined state
 */
template <class TInputImage, class TOutputImage, class TTransform,
    class TInterpolator>
ResampleImageFilter<TInputImage,TOutputImage, TTransform, TInterpolator>
::ResampleImageFilter()
{
  for (int i = 0; i < NDimensions; i++)
    m_Size[i] = 0;
  m_Transform = 0;
  m_Interpolator = 0;
  m_DefaultPixelValue = 0;
}


/**
 * Print out a description of self
 *
 * \todo Add details about this class
 */
template <class TInputImage, class TOutputImage, class TTransform,
    class TInterpolator>
void 
ResampleImageFilter<TInputImage,TOutputImage, TTransform, TInterpolator>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  std::cout << "DefaultPixelValue: " << m_DefaultPixelValue << std::endl;
  return;
}


/**
 * ThreadedGenerateData
 */
template <class TInputImage, class TOutputImage, class TTransform,
    class TInterpolator>
void 
ResampleImageFilter<TInputImage,TOutputImage, TTransform, TInterpolator>
::ThreadedGenerateData(
  const OutputImageRegionType& outputRegionForThread,
  int threadId)
{
  int i;
  
  itkDebugMacro(<<"Actually executing");

  // Get the input and output pointers
  InputImagePointer  inputPtr  = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();

  // Create an iterator that will walk the output region for this thread.
  typedef
    ImageRegionIterator<TOutputImage> OutputIterator;

  OutputIterator outIt(outputPtr, outputRegionForThread);

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  IndexType outputIndex;         // Index to current output pixel
  PointType outputPoint;         // Coordinates of current output pixel
  PointType inputPoint;          // Coordinates of current input pixel

  // Configure the interpolator
  m_Interpolator->SetInputImage(this->GetInput());

  // Estimate total work for progress methods/callbacks
  unsigned long updateVisits = 0;
  if ( threadId == 0 )
    {
    updateVisits = 
      outputPtr->GetRequestedRegion().GetNumberOfPixels()/10;
    }
        
  // Walk the output region
  for (i=0; !outIt.IsAtEnd(); ++outIt, i++ )

    // Update progress
    {
    if ( threadId == 0 && !(i % updateVisits ) )
      {
      this->UpdateProgress((float)i/(float(updateVisits)*10.0));
      }
    
    // Determine the index of the current output pixel
    outputIndex = outIt.GetIndex();
    for (int ii = 0; ii < NDimensions; ++ii)
      outputPoint[ii] = outputIndex[ii];

    // Compute corresponding input pixel position
    inputPoint = m_Transform->TransformPoint(outputPoint);

    // Evaluate input at right position and copy to the output
    if( m_Interpolator->IsInsideBuffer(inputPoint) )
    {
      const PixelType pixval = 
        static_cast<PixelType>( m_Interpolator->Evaluate(inputPoint));
      outIt.Set( pixval );      
    }
    else
    {
      outIt.Set(m_DefaultPixelValue); // default background value
    }
  }

  return;
}


/** 
 * Inform pipeline of necessary input image region
 *
 * Determining the actual input region is non-trivial, especially
 * when we cannot assume anything about the transform being used.
 * So we do the easy thing and request the entire input image.
 */
template <class TInputImage, class TOutputImage, class TTransform,
    class TInterpolator>
void 
ResampleImageFilter<TInputImage,TOutputImage, TTransform, TInterpolator>
::GenerateInputRequestedRegion()
{
  // call the superclass's implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer  inputPtr  = this->GetInput();
  if ( !inputPtr )
    {
    return;
    }

  // Request the entire input image
  InputImageRegionType inputRegion;
  inputRegion = inputPtr->GetLargestPossibleRegion();
  inputPtr->SetLargestPossibleRegion(inputRegion);
  inputPtr->SetRequestedRegion(inputRegion);

  return;
}


/** 
 * Inform pipeline of required output region
 */
template <class TInputImage, class TOutputImage, class TTransform,
    class TInterpolator>
void 
ResampleImageFilter<TInputImage,TOutputImage, TTransform, TInterpolator>
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // get pointers to the input and output
  OutputImagePointer outputPtr = this->GetOutput();
  if ( !outputPtr )
    {
    return;
    }

  // Set the size of the output region
  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize( m_Size );
  outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );

  return;
}

} // end namespace itk

#endif
