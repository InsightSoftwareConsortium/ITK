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
#include "itkAffineTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 * Initialize new instance
 */
template <class TInputImage, class TOutputImage>
ResampleImageFilter<TInputImage, TOutputImage>
::ResampleImageFilter()
{
  for (unsigned int i = 0; i < ImageDimension; i++)
    {
    m_Size[i] = 0;
    m_OutputSpacing[i] = 1.0;
    m_OutputOrigin[i] = 0.0;
    }
  
  m_Transform = AffineTransform<double, ImageDimension>::New();
  m_Interpolator = LinearInterpolateImageFunction<InputImageType, double>::New();
  m_DefaultPixelValue = 0;
}


/**
 * Print out a description of self
 *
 * \todo Add details about this class
 */
template <class TInputImage, class TOutputImage>
void 
ResampleImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  unsigned int j;
  
 os << indent << "DefaultPixelValue: "
    << static_cast<typename NumericTraits<PixelType>::PrintType>(m_DefaultPixelValue)
    << std::endl;
  os << indent << "Size: [";
  for( j = 0; j < ImageDimension - 1; j++ )
    {
    os << m_Size[j] << ", ";
    }
  os << m_Size[j] << "]" << std::endl;

  os << indent << "OutputSpacing: [";
  for( j = 0; j < ImageDimension - 1; j++ )
    {
    os << m_OutputSpacing[j] << ", ";
    }
  os << m_OutputSpacing[j] << "]" << std::endl;

  os << indent << "OutputOrigin: [";
  for( j = 0; j < ImageDimension - 1; j++ )
    {
    os << m_OutputOrigin[j] << ", ";
    }
  os << m_OutputOrigin[j] << "]" << std::endl;

  os << indent << "Transform: " << m_Transform.GetPointer() << std::endl;
  os << indent << "Interpolator: " << m_Interpolator.GetPointer() << std::endl;

  return;
}


/**
 * Set the output image spacing.
 */
template <class TInputImage, class TOutputImage>
void 
ResampleImageFilter<TInputImage,TOutputImage>
::SetOutputSpacing(
const double spacing[ImageDimension] )
{

  unsigned int j; 
  for ( j = 0; j < ImageDimension; j++)
    {
    if ( spacing[j] != m_OutputSpacing[j] )
      {
      break;
      }
    } 
  if ( j < ImageDimension ) 
    { 
    this->Modified(); 
    for ( j = 0; j < ImageDimension; j++)
      {
      m_OutputSpacing[j] = spacing[j];
      }
    } 

}


/**
 * Set the output image origin.
 */
template <class TInputImage, class TOutputImage>
void 
ResampleImageFilter<TInputImage,TOutputImage>
::SetOutputOrigin(
const double origin[ImageDimension] )
{

  unsigned int j; 
  for ( j = 0; j < ImageDimension; j++)
    {
    if ( origin[j] != m_OutputOrigin[j] )
      {
      break;
      }
    } 
  if ( j < ImageDimension ) 
    { 
    this->Modified(); 
    for ( j = 0; j < ImageDimension; j++)
      {
      m_OutputOrigin[j] = origin[j];
      }
    } 

}



/**
 * Setup state of filter before multi-threading.
 * InterpolatorType::SetInputImage is not thread-safe and hence
 * has to be setup before ThreadedGenerateData
 */
template <class TInputImage, class TOutputImage>
void 
ResampleImageFilter<TInputImage,TOutputImage>
::BeforeThreadedGenerateData()
{

  if( !m_Interpolator )
    {
    itkExceptionMacro(<< "Interpolator not set");
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
ResampleImageFilter<TInputImage,TOutputImage>
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

  // Define a few indices that will be used to translate from an input pixel
  // to an output pixel
  IndexType outputIndex;         // Index to current output pixel
  PointType outputPoint;         // Coordinates of current output pixel
  PointType inputPoint;          // Coordinates of current input pixel

  // Support for progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
        
  // Walk the output region
  for (i=0; !outIt.IsAtEnd(); ++outIt, i++ )
    {
    // Determine the index of the current output pixel
    outputIndex = outIt.GetIndex();
    for (unsigned int ii = 0; ii < ImageDimension; ++ii)
      {
      outputPoint[ii] = (double) outputIndex[ii] * m_OutputSpacing[ii] + 
        m_OutputOrigin[ii];
      }

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
ResampleImageFilter<TInputImage,TOutputImage>
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
template <class TInputImage, class TOutputImage>
void 
ResampleImageFilter<TInputImage,TOutputImage>
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

  // Set spacing and origin
  outputPtr->SetSpacing( m_OutputSpacing );
  outputPtr->SetOrigin( m_OutputOrigin );

  return;
}

} // end namespace itk

#endif
