/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNaryFunctorImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNaryFunctorImageFilter_txx
#define _itkNaryFunctorImageFilter_txx

#include "itkNaryFunctorImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage, class TFunction >
NaryFunctorImageFilter<TInputImage,TOutputImage,TFunction>
::NaryFunctorImageFilter()
{
  // This number will be incremented each time an image
  // is added over the two minimum required
  this->SetNumberOfRequiredInputs( 2 );
}


/**
 * ThreadedGenerateData Performs the pixel-wise addition
 */
template <class TInputImage, class TOutputImage, class TFunction>
void
NaryFunctorImageFilter<TInputImage, TOutputImage, TFunction>
::ThreadedGenerateData( const OutputImageRegionType &outputRegionForThread,
                        int threadId)
{

  const unsigned int numberOfInputImages = 
          static_cast< unsigned int >( this->GetNumberOfInputs() );
  
  OutputImagePointer outputPtr = this->GetOutput(0);
  ImageRegionIterator<TOutputImage> outputIt(outputPtr, outputRegionForThread);


  // Clear the content of the output
  outputIt.GoToBegin();
  while( !outputIt.IsAtEnd() )
    {
      outputIt.Set( itk::NumericTraits< OutputImagePixelType >::Zero );
      ++outputIt;
    }
 
  // support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());
    
  for(unsigned int inputNumber=0;
      inputNumber < numberOfInputImages; inputNumber++ )
    {
    // We use dynamic_cast since inputs are stored as DataObjects.  
    InputImagePointer inputPtr = 
          dynamic_cast<TInputImage*>( ProcessObject::GetInput( inputNumber ) );

    ImageRegionIterator<TInputImage> inputIt(inputPtr, outputRegionForThread);

    inputIt.GoToBegin();
    outputIt.GoToBegin();
    while( !inputIt.IsAtEnd() ) 
      {
      outputIt.Set( m_Functor( outputIt.Get(), inputIt.Get() ) );
      ++inputIt;
      ++outputIt;
      progress.CompletedPixel();
      }
    }
}

} // end namespace itk

#endif


