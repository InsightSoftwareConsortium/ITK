/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUnaryFunctorImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkUnaryFunctorImageFilter_txx
#define _itkUnaryFunctorImageFilter_txx

#include "itkUnaryFunctorImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage, class TFunction  >
UnaryFunctorImageFilter<TInputImage,TOutputImage,TFunction>
::UnaryFunctorImageFilter()
{
  this->SetNumberOfRequiredInputs( 1 );
}

/** 
 * UnaryFunctorImageFilter can produce an image which is a different resolution
 * than its input image.  As such, UnaryFunctorImageFilter needs to provide an
 * implementation for GenerateOutputInformation() in order to inform
 * the pipeline execution model.  The original documentation of this
 * method is below.
 *
 * \sa ProcessObject::GenerateOutputInformaton() 
 */
template <class TInputImage, class TOutputImage, class TFunction>
void 
UnaryFunctorImageFilter<TInputImage,TOutputImage,TFunction>
::GenerateOutputInformation()
{
  // do not call the superclass' implementation of this method since
  // this filter allows the input the output to be of different dimensions
 
  // get pointers to the input and output
  typename Superclass::OutputImagePointer      outputPtr = this->GetOutput();
  typename Superclass::InputImageConstPointer  inputPtr  = this->GetInput();

  if ( !outputPtr || !inputPtr)
    {
    return;
    }

  // Set the output image largest possible region.  Use a RegionCopier
  // so that the input and output images can be different dimensions.
  OutputImageRegionType outputLargestPossibleRegion;
  this->CallCopyInputRegionToOutputRegion(outputLargestPossibleRegion,
                                          inputPtr->GetLargestPossibleRegion());
  outputPtr->SetLargestPossibleRegion( outputLargestPossibleRegion );

  // Set the output spacing and origin
  const ImageBase<InputImageDimension> *phyData;

  phyData
    = dynamic_cast<const ImageBase<InputImageDimension>*>(this->GetInput());

  if (phyData)
    {
    // Copy what we can from the image from spacing and origin of the input
    // This logic needs to be augmented with logic that select which
    // dimensions to copy
    unsigned int i;
    const double *inputSpacing = inputPtr->GetSpacing();
    const double *inputOrigin = inputPtr->GetOrigin();

    double outputSpacing[OutputImageDimension];
    double outputOrigin[OutputImageDimension];

    // copy the input to the output and fill the rest of the
    // output with zeros.
    for (i=0; i < InputImageDimension; ++i)
      {
      outputSpacing[i] = inputSpacing[i];
      outputOrigin[i] = inputOrigin[i];
      }
    for (; i < OutputImageDimension; ++i)
      {
      outputSpacing[i] = 1.0;
      outputOrigin[i] = 0.0;
      }

    // set the spacing and origin
    outputPtr->SetSpacing( outputSpacing );
    outputPtr->SetOrigin( outputOrigin );
    }
  else
    {
    // pointer could not be cast back down
    itkExceptionMacro(<< "itk::UnaryFunctorImageFilter::GenerateOutputInformation "
                      << "cannot cast input to "
                      << typeid(ImageBase<InputImageDimension>*).name() );
    }
}


/**
 * ThreadedGenerateData Performs the pixel-wise addition
 */
template <class TInputImage, class TOutputImage, class TFunction  >
void
UnaryFunctorImageFilter<TInputImage,TOutputImage,TFunction>
::ThreadedGenerateData( const OutputImageRegionType &outputRegionForThread,
                        int threadId)
{
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput(0);
  
  // Define the portion of the input to walk for this thread, using
  // the CallCopyOutputRegionToInputRegion method allows for the input
  // and output images to be different dimensions
  InputImageRegionType inputRegionForThread;
  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  // Define the iterators
  ImageRegionConstIterator<TInputImage>  inputIt(inputPtr, inputRegionForThread);
  ImageRegionIterator<TOutputImage> outputIt(outputPtr, outputRegionForThread);

  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());

  inputIt.GoToBegin();
  outputIt.GoToBegin();

  try  // this try is intended to catch an eventual AbortException.
    {

    while( !inputIt.IsAtEnd() ) 
      {
      outputIt.Set( m_Functor( inputIt.Get() ) );
      ++inputIt;
      ++outputIt;
      progress.CompletedPixel();  // potential exception thrown here
      }

    }
  catch( ProcessAborted  & except )
    {
    // User aborted filter excecution Here we catch an exception thrown by the
    // progress reporter and rethrow it with the correct line number and file
    // name. We also invoke AbortEvent in case some observer was interested on
    // it.
    this->InvokeEvent( AbortEvent() );
    this->ResetPipeline();
    throw ProcessAborted(__FILE__,__LINE__);
    }
}





} // end namespace itk

#endif
