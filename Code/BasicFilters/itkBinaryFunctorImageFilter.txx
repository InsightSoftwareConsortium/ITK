/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryFunctorImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBinaryFunctorImageFilter_txx
#define _itkBinaryFunctorImageFilter_txx

#include "itkBinaryFunctorImageFilter.h"
#include <itkImageRegionIterator.h>

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage1, class TInputImage2, 
          class TOutputImage, class TFunction  >
BinaryFunctorImageFilter<TInputImage1,TInputImage2,TOutputImage,TFunction>
::BinaryFunctorImageFilter()
{
  this->SetNumberOfRequiredInputs( 2 );
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter<TInputImage1,TInputImage2,TOutputImage,TFunction>
::SetInput1( TInputImage1 * image1 ) 
{
  SetNthInput(0, image1);
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter<TInputImage1,TInputImage2,TOutputImage,TFunction>
::SetInput2( TInputImage2 * image2 ) 
{
  SetNthInput(1, image2);
}



/**
 * ThreadedGenerateData Performs the pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, class TOutputImage, class TFunction  >
void
BinaryFunctorImageFilter<TInputImage1, TInputImage2, TOutputImage, TFunction>
::ThreadedGenerateData( const OutputImageRegionType &outputRegionForThread,
                        int threadId)
{
  // We use dynamic_cast since inputs are stored as DataObjects.  The
  // ImageToImageFilter::GetInput(int) always returns a pointer to a
  // TInputImage1 so it cannot be used for the second input.
  Input1ImagePointer inputPtr1
    = dynamic_cast<TInputImage1*>((ProcessObject::GetInput(0)).GetPointer());
  Input2ImagePointer inputPtr2
    = dynamic_cast<TInputImage2*>((ProcessObject::GetInput(1)).GetPointer());
  OutputImagePointer outputPtr = this->GetOutput(0);
  
  ImageRegionIterator<TInputImage1> inputIt1(inputPtr1, outputRegionForThread);
  ImageRegionIterator<TInputImage2> inputIt2(inputPtr2, outputRegionForThread);
  ImageRegionIterator<TOutputImage> outputIt(outputPtr, outputRegionForThread);

  // support progress methods/callbacks
  unsigned long updateVisits = 0, i=0;
  if ( threadId == 0 )
    {
    updateVisits = 
      outputPtr->GetRequestedRegion().GetNumberOfPixels()/10;
    if ( updateVisits < 1 ) updateVisits = 1;
    }
        
  inputIt1.GoToBegin();
  inputIt2.GoToBegin();
  outputIt.GoToBegin();
  i = 0;
  while( !inputIt1.IsAtEnd() ) 
    {
    if ( threadId == 0 && !(i % updateVisits ) )
      {
      this->UpdateProgress((float)i/(float(updateVisits)*10.0));
      }

    outputIt.Set( m_Functor( inputIt1.Get(), inputIt2.Get() ) );
    ++inputIt1;
    ++inputIt2;
    ++outputIt;
    ++i;
    }
}

} // end namespace itk

#endif
