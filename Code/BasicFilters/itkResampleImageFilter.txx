/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkResampleImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/


#ifndef _itkResampleImageFilter_txx
#define _itkResampleImageFilter_txx

#include "itkResampleImageFilter.h"
#include "itkObjectFactory.h"
#include "itkIdentityTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkProgressReporter.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{

/**
 * Initialize new instance
 */
template <class TInputImage, class TOutputImage, class TInterpolatorPrecisionType>
ResampleImageFilter<TInputImage, TOutputImage,TInterpolatorPrecisionType>
::ResampleImageFilter()
{
  m_OutputSpacing.Fill(1.0);
  m_OutputOrigin.Fill(0.0);
  m_OutputDirection.SetIdentity();

  m_UseReferenceImage = false;

  m_Size.Fill( 0 );
  m_OutputStartIndex.Fill( 0 );
  
  m_Transform = IdentityTransform<TInterpolatorPrecisionType, ImageDimension>::New();
  m_Interpolator = LinearInterpolateImageFunction<InputImageType, TInterpolatorPrecisionType>::New();
  m_DefaultPixelValue = 0;
}


/**
 * Print out a description of self
 *
 * \todo Add details about this class
 */
template <class TInputImage, class TOutputImage, class TInterpolatorPrecisionType>
void 
ResampleImageFilter<TInputImage, TOutputImage,TInterpolatorPrecisionType>
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
  os << indent << "OutputDirection: " << m_OutputDirection << std::endl;
  os << indent << "Transform: " << m_Transform.GetPointer() << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;
  os << indent << "UseReferenceImage: " << (m_UseReferenceImage ? "On" : "Off") << std::endl;
  if (m_ReferenceImage)
    {
    os << indent << "ReferenceImage: " << m_ReferenceImage.GetPointer() << std::endl;
    }
  else
    {
    os << indent << "ReferenceImage: 0" << std::endl;
    }

  return;
}



/**
 * Set the output image spacing.
 */
template <class TInputImage, class TOutputImage, class TInterpolatorPrecisionType>
void 
ResampleImageFilter<TInputImage,TOutputImage,TInterpolatorPrecisionType>
::SetOutputSpacing(
  const double spacing[Self::ImageDimension] )
{
  SpacingType s(spacing);
  this->SetOutputSpacing( s );
}


/**
 * Set the output image origin.
 */
template <class TInputImage, class TOutputImage, class TInterpolatorPrecisionType>
void 
ResampleImageFilter<TInputImage,TOutputImage,TInterpolatorPrecisionType>
::SetOutputOrigin(
  const double origin[Self::ImageDimension] )
{
  PointType p(origin);
  this->SetOutputOrigin( p );
}


/**
 * Set up state of filter before multi-threading.
 * InterpolatorType::SetInputImage is not thread-safe and hence
 * has to be set up before ThreadedGenerateData
 */
template <class TInputImage, class TOutputImage, class TInterpolatorPrecisionType>
void 
ResampleImageFilter<TInputImage,TOutputImage,TInterpolatorPrecisionType>
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
template <class TInputImage, class TOutputImage, class TInterpolatorPrecisionType>
void 
ResampleImageFilter<TInputImage,TOutputImage,TInterpolatorPrecisionType>
::AfterThreadedGenerateData()
{
  // Disconnect input image from the interpolator
  m_Interpolator->SetInputImage( NULL );

}

/**
 * ThreadedGenerateData
 */
template <class TInputImage, class TOutputImage, class TInterpolatorPrecisionType>
void 
ResampleImageFilter<TInputImage,TOutputImage,TInterpolatorPrecisionType>
::ThreadedGenerateData(
  const OutputImageRegionType& outputRegionForThread,
  int threadId)
{
  unsigned int i;
  
  itkDebugMacro(<<"Actually executing");

  // Get the output pointers
  OutputImagePointer      outputPtr = this->GetOutput();

  // Create an iterator that will walk the output region for this thread.
  typedef ImageRegionIteratorWithIndex<TOutputImage> OutputIterator;

  OutputIterator outIt(outputPtr, outputRegionForThread);

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  PointType outputPoint;         // Coordinates of current output pixel
  PointType inputPoint;          // Coordinates of current input pixel

  // Support for progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
        
  typedef typename InterpolatorType::OutputType OutputType;

  // Walk the output region
  const PixelType minValue =  itk::NumericTraits<PixelType >::NonpositiveMin();
  const PixelType maxValue =  itk::NumericTraits<PixelType >::max();

  outIt.GoToBegin();

  while ( !outIt.IsAtEnd() )
    {
    // Determine the index of the current output pixel
    const IndexType & outputIndex = outIt.GetIndex();
    outputPtr->TransformIndexToPhysicalPoint( outputIndex, outputPoint );

    // Compute corresponding input pixel position
    inputPoint = m_Transform->TransformPoint(outputPoint);

    // Evaluate input at right position and copy to the output
    if( m_Interpolator->IsInsideBuffer(inputPoint) )
      {
      PixelType pixval;
      const OutputType value = m_Interpolator->Evaluate(inputPoint);
      if( value < static_cast<OutputType>( minValue ) )
        {
        pixval = minValue;
        }
      else if( value > static_cast<OutputType>( maxValue ) )
        {
        pixval = maxValue;
        }
      else 
        {
        pixval = static_cast<PixelType>( value );
        }
      outIt.Set( pixval );      
      }
    else
      {
      outIt.Set(m_DefaultPixelValue); // default background value
      }

    progress.CompletedPixel();
    ++outIt;
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
template <class TInputImage, class TOutputImage, class TInterpolatorPrecisionType>
void 
ResampleImageFilter<TInputImage,TOutputImage,TInterpolatorPrecisionType>
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
  inputPtr->SetLargestPossibleRegion(inputRegion);
  inputPtr->SetRequestedRegion(inputRegion);

  return;
}


/** 
 * Inform pipeline of required output region
 */
template <class TInputImage, class TOutputImage, class TInterpolatorPrecisionType>
void 
ResampleImageFilter<TInputImage,TOutputImage,TInterpolatorPrecisionType>
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
  if (m_UseReferenceImage && m_ReferenceImage)
    {
    outputPtr->SetLargestPossibleRegion( m_ReferenceImage->GetLargestPossibleRegion() );
    }
  else
    {
    typename TOutputImage::RegionType outputLargestPossibleRegion;
    outputLargestPossibleRegion.SetSize( m_Size );
    outputLargestPossibleRegion.SetIndex( m_OutputStartIndex );
    outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );
    }

  // Set spacing and origin
  if (m_UseReferenceImage && m_ReferenceImage)
    {
    outputPtr->SetSpacing( m_ReferenceImage->GetSpacing() );
    outputPtr->SetOrigin( m_ReferenceImage->GetOrigin() );
    outputPtr->SetDirection( m_ReferenceImage->GetDirection() );
    }
  else
    {
    outputPtr->SetSpacing( m_OutputSpacing );
    outputPtr->SetOrigin( m_OutputOrigin );
    outputPtr->SetDirection( m_OutputDirection );
    }
  return;
}



/** 
 * Verify if any of the components has been modified.
 */
template <class TInputImage, class TOutputImage, class TInterpolatorPrecisionType>
unsigned long 
ResampleImageFilter<TInputImage,TOutputImage,TInterpolatorPrecisionType>
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
