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
#include <itkImageRegionIterator.h>

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
  
  ImageRegionIterator<TInputImage>  inputIt(inputPtr, outputRegionForThread);
  ImageRegionIterator<TOutputImage> outputIt(outputPtr, outputRegionForThread);

  // support progress methods/callbacks
  unsigned long updateVisits = 0, i=0;
  if ( threadId == 0 )
    {
    updateVisits = 
      outputPtr->GetRequestedRegion().GetNumberOfPixels()/10;
    if ( updateVisits < 1 ) updateVisits = 1;
    }
        
  inputIt.GoToBegin();
  outputIt.GoToBegin();
  i = 0;
  while( !inputIt.IsAtEnd() ) 
    {
    if ( threadId == 0 && !(i % updateVisits ) )
      {
      this->UpdateProgress( static_cast<float>(i) / 
                            static_cast<float>(updateVisits * 10.0) );
      }

    outputIt.Set( m_Functor( inputIt.Get() ) );
    ++inputIt;
    ++outputIt;
    ++i;
    }
}





} // end namespace itk

#endif
