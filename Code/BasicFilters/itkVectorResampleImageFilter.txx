/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorResampleImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#ifndef _itkVectorResampleImageFilter_txx
#define _itkVectorResampleImageFilter_txx

#include "itkVectorResampleImageFilter.h"
#include "itkObjectFactory.h"
#include "itkIdentityTransform.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 * Initialize new instance
 */
template <class TInputImage, class TOutputImage>
VectorResampleImageFilter<TInputImage, TOutputImage>
::VectorResampleImageFilter()
{
  m_OutputSpacing.Fill(1.0);
  m_OutputOrigin.Fill(0.0);
  m_Size.Fill( 0 );
  m_OutputStartIndex.Fill( 0 );

  m_Transform = IdentityTransform<double, ImageDimension>::New();
  m_Interpolator = VectorLinearInterpolateImageFunction<InputImageType, double>::New();
  m_DefaultPixelValue.Fill(0);
}


/**
 * Print out a description of self
 *
 * \todo Add details about this class
 */
template <class TInputImage, class TOutputImage>
void 
VectorResampleImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "DefaultPixelValue: "
     << static_cast<typename NumericTraits<PixelType>::PrintType>(m_DefaultPixelValue)
     << std::endl;
  os << indent << "Size: " << m_Size << std::endl;
  os << indent << "OutputStartIndex: " << m_OutputStartIndex << std::endl;
  os << indent << "OutputSpacing: " << m_OutputSpacing << std::endl;
  os << indent << "OutputOrigin: " << m_OutputOrigin << std::endl;
  os << indent << "Transform: " << m_Transform.GetPointer() << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;

  return;
}



/**
 * Set the output image spacing.
 */
template <class TInputImage, class TOutputImage>
void 
VectorResampleImageFilter<TInputImage,TOutputImage>
::SetOutputSpacing(const double* spacing)
{
  SpacingType s(spacing);
  this->SetOutputSpacing( s );
}


/**
 * Set the output image origin.
 */
template <class TInputImage, class TOutputImage>
void 
VectorResampleImageFilter<TInputImage,TOutputImage>
::SetOutputOrigin(const double* origin)
{
  PointType p(origin);
  this->SetOutputOrigin( p );
}


/**
 * Set up state of filter before multi-threading.
 * InterpolatorType::SetInputImage is not thread-safe and hence
 * has to be set up before ThreadedGenerateData
 */
template <class TInputImage, class TOutputImage>
void 
VectorResampleImageFilter<TInputImage,TOutputImage>
::BeforeThreadedGenerateData()
{

  if( !m_Interpolator )
    {
    itkExceptionMacro(<< "Interpolator not set");
    }

  // Connect input image to interpolator
  m_Interpolator->SetInputImage( this->GetInput() );
}

/**
 * Set up state of filter after multi-threading.
 */
template <class TInputImage, class TOutputImage>
void 
VectorResampleImageFilter<TInputImage,TOutputImage>
::AfterThreadedGenerateData()
{
  //Disconnect input image from interpolator
  m_Interpolator->SetInputImage( NULL );
}

/**
 * ThreadedGenerateData
 */
template <class TInputImage, class TOutputImage>
void 
VectorResampleImageFilter<TInputImage,TOutputImage>
::ThreadedGenerateData(
  const OutputImageRegionType& outputRegionForThread,
  int threadId)
{
  unsigned int i;
  
  itkDebugMacro(<<"Actually executing");

  // Get the output pointers
  OutputImagePointer      outputPtr = this->GetOutput();

  // Create an iterator that will walk the output region for this thread.
  typedef
    ImageRegionIterator<TOutputImage> OutputIterator;

  OutputIterator outIt(outputPtr, outputRegionForThread);

  typedef typename VectorLinearInterpolateImageFunction<InputImageType, double>::OutputType OutputType;

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  IndexType outputIndex;         // Index to current output pixel
  PointType outputPoint;         // Coordinates of current output pixel
  PointType inputPoint;          // Coordinates of current input pixel

  const unsigned int numberOfComponents = PixelType::GetNumberOfComponents();

  // Support for progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels(), 10);
        

  // Walk the output region
  for (i=0; !outIt.IsAtEnd(); ++outIt, i++ )
    {
    // Determine the index of the current output pixel
    outputIndex = outIt.GetIndex();
    outputPtr->TransformIndexToPhysicalPoint( outputIndex, outputPoint );

    // Compute corresponding input pixel position
    inputPoint = m_Transform->TransformPoint(outputPoint);

    // Evaluate input at right position and copy to the output
    if( m_Interpolator->IsInsideBuffer(inputPoint) )
      {
      PixelType   pixval; 
      OutputType  output = m_Interpolator->Evaluate( inputPoint );
      for( unsigned int i=0; i< numberOfComponents; i++ )
        {
        pixval[i] = static_cast<PixelComponentType>( output[i] );
        }
      outIt.Set( pixval );      
      }
    else
      {
      outIt.Set(m_DefaultPixelValue); // default background value
      }

    progress.CompletedPixel();
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
template <class TInputImage, class TOutputImage>
void 
VectorResampleImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  // call the superclass's implementation of this method
  Superclass::GenerateInputRequestedRegion();

  if ( !this->GetInput() )
    {
    return;
    }

  // get pointers to the input and output
  InputImagePointer  inputPtr  = 
    const_cast< TInputImage *>( this->GetInput() );

  // Request the entire input image
  InputImageRegionType inputRegion;
  inputRegion = inputPtr->GetLargestPossibleRegion();
  inputPtr->SetRequestedRegion(inputRegion);

  return;
}


/** 
 * Inform pipeline of required output region
 */
template <class TInputImage, class TOutputImage>
void 
VectorResampleImageFilter<TInputImage,TOutputImage>
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
  outputLargestPossibleRegion.SetIndex( m_OutputStartIndex );
  outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );

  // Set spacing and origin
  outputPtr->SetSpacing( m_OutputSpacing );
  outputPtr->SetOrigin( m_OutputOrigin );

  return;
}



/** 
 * Verify if any of the components has been modified.
 */
template <class TInputImage, class TOutputImage>
unsigned long 
VectorResampleImageFilter<TInputImage,TOutputImage>
::GetMTime( void ) const
{
  unsigned long latestTime = Object::GetMTime(); 

  if( m_Transform )
    {
    if( latestTime < m_Transform->GetMTime() )
      {
      latestTime = m_Transform->GetMTime();
      }
    }

  if( m_Interpolator )
    {
    if( latestTime < m_Interpolator->GetMTime() )
      {
      latestTime = m_Interpolator->GetMTime();
      }
    }

  return latestTime;
}



} // end namespace itk

#endif
