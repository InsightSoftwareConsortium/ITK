/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkInterpolateImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkInterpolateImageFilter_txx
#define _itkInterpolateImageFilter_txx
#include "itkInterpolateImageFilter.h"

#include "itkLinearInterpolateImageFunction.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage>
InterpolateImageFilter<TInputImage,TOutputImage>
::InterpolateImageFilter()
{

  // The filter requires two inputs
  this->SetNumberOfRequiredInputs( 2 );

  // Set default interpolator to linear
  typedef LinearInterpolateImageFunction<IntermediateImageType>
    LinearInterpolatorType;
  typename LinearInterpolatorType::Pointer interpolator =
    LinearInterpolatorType::New();

  m_Interpolator = static_cast<InterpolatorType *>( interpolator.GetPointer() );

  // Set default distance to 0,5
  m_Distance = 0.5;

  m_IntermediateImage = NULL;

}


/**
 * Set the second image
 */
template <class TInputImage, class TOutputImage>
void 
InterpolateImageFilter<TInputImage,TOutputImage>
::SetInput2( const InputImageType * image )
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(1, 
          const_cast< InputImageType * >( image ) );

}


/**
 * Get the second image
 */
template <class TInputImage, class TOutputImage>
const InterpolateImageFilter<TInputImage,TOutputImage>::InputImageType *
InterpolateImageFilter<TInputImage,TOutputImage>
::GetInput2()
{
  if (this->GetNumberOfInputs() < 2)
    {
    return 0;
    }
  
  return static_cast<const TInputImage * >
                     (this->ProcessObject::GetInput(1) );
}


/**
 * Print out a description of self
 */
template <class TInputImage, class TOutputImage>
void 
InterpolateImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{

  this->Superclass::PrintSelf(os,indent);

  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "Distance: " << m_Distance << std::endl;

}



/**
 * Setup state of filter before multi-threading.
 * InterpolatorType::SetInputImage is not thread-safe and hence
 * has to be setup before ThreadedGenerateData
 */
template <class TInputImage, class TOutputImage>
void 
InterpolateImageFilter<TInputImage,TOutputImage>
::BeforeThreadedGenerateData()
{

  if( !m_Interpolator )
    {
    itkExceptionMacro(<< "Interpolator not set");
    }

  // Create intermediate image
  typedef typename IntermediateImageType::RegionType IntermediateImageRegionType;
  typedef typename TOutputImage::RegionType ImageRegionType;

  ImageRegionType outputRegion = this->GetOutput()->GetRequestedRegion();

  IntermediateImageRegionType intermediateRegion; 

  typedef ImageToImageFilterDetail::ImageRegionCopier<ImageDimension+1,ImageDimension>
    RegionCopierType;
  RegionCopierType regionCopier;
  regionCopier( intermediateRegion, outputRegion );

  intermediateRegion.SetIndex( ImageDimension, 0 );
  intermediateRegion.SetSize( ImageDimension, 2 );

  m_IntermediateImage = IntermediateImageType::New();
  m_IntermediateImage->SetRegions( intermediateRegion );
  m_IntermediateImage->Allocate();

  // Fill intermediate image
  intermediateRegion.SetIndex( ImageDimension, 0 );
  intermediateRegion.SetSize( ImageDimension, 1 );

  ImageRegionConstIteratorWithIndex<TInputImage> inIter( this->GetInput1(), outputRegion );
  ImageRegionIteratorWithIndex<IntermediateImageType> outIter( m_IntermediateImage,
    intermediateRegion );

  while( !inIter.IsAtEnd() )
    {
    outIter.Set( inIter.Get() );
    ++inIter;
    ++outIter;
    }


  intermediateRegion.SetIndex( ImageDimension, 1 );
  intermediateRegion.SetSize( ImageDimension, 2 );

  inIter = ImageRegionConstIteratorWithIndex<TInputImage>( this->GetInput2(), outputRegion );
  outIter = ImageRegionIteratorWithIndex<IntermediateImageType>( m_IntermediateImage,
    intermediateRegion );

  while( !inIter.IsAtEnd() )
    {
    outIter.Set( inIter.Get() );
    ++inIter;
    ++outIter;
    }


  // Connect input image to interpolator
  m_Interpolator->SetInputImage( m_IntermediateImage );

}


/**
 * AfterThreadedGenerateData
 */
template <class TInputImage, class TOutputImage>
void 
InterpolateImageFilter<TInputImage,TOutputImage>
::AfterThreadedGenerateData()
{

 // Clean up intermediat memory usage
 m_IntermediateImage = NULL;

}

/**
 * ThreadedGenerateData
 */
template <class TInputImage, class TOutputImage>
void 
InterpolateImageFilter<TInputImage,TOutputImage>
::ThreadedGenerateData(
  const OutputImageRegionType& outputRegionForThread,
  int threadId)
{
  
  // Get the output pointers
  OutputImagePointer      outputPtr = this->GetOutput();

  // Create an iterator that will walk the output region for this thread.
  typedef
    ImageRegionIteratorWithIndex<TOutputImage> OutputIterator;

  OutputIterator outIt(outputPtr, outputRegionForThread);

  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TOutputImage::IndexType IndexType;
  typedef typename InterpolatorType::ContinuousIndexType ContinuousIndexType;

  IndexType outputIndex;                         // Index to current output pixel
  ContinuousIndexType intermediateIndex;         // Coordinates of current intermediate image pixel

  // Estimate total work for progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
          
  // Walk the output region
  while ( !outIt.IsAtEnd() )
    {
    // Determine the intermediate image index
    outputIndex = outIt.GetIndex();
    for (unsigned int j = 0; j< ImageDimension; ++j)
    {
    intermediateIndex[j] = (double) outputIndex[j];
    }
    intermediateIndex[ImageDimension] = m_Distance;

    // Evaluate input at right position and copy to the output
    if( m_Interpolator->IsInsideBuffer(intermediateIndex) )
    {
      outIt.Set( static_cast<OutputPixelType>( 
        m_Interpolator->EvaluateAtContinuousIndex( intermediateIndex ) ) );      
    }
    else
    {
      // should never be in here
      itkExceptionMacro( << "Index not within the intermediate buffer" );
    }

    ++outIt;
    progress.CompletedPixel();
    }

}


} // end namespace itk

#endif
