/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTernaryFunctorImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkTernaryFunctorImageFilter_txx
#define _itkTernaryFunctorImageFilter_txx

#include "itkTernaryFunctorImageFilter.h"
#include <itkImageRegionIterator.h>
#include <itkImageRegionConstIterator.h>

namespace itk
{

/**
 * Constructor
 */
template < class TInputImage1, class TInputImage2, 
           class TInputImage3, class TOutputImage, class TFunction  >
TernaryFunctorImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::TernaryFunctorImageFilter()
{
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryFunctorImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::SetInput1( const TInputImage1 *image1 ) 
{
  // The ProcessObject is not const-correct so the const_cast is required here
  SetNthInput( 0, const_cast<TInputImage1 *>( image1 ) );
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryFunctorImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::SetInput2( const TInputImage2 *image2 ) 
{
  // The ProcessObject is not const-correct so the const_cast is required here
  SetNthInput( 1, const_cast<TInputImage1 *>( image2 ) );
}



/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryFunctorImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::SetInput3( const TInputImage3 *image3 ) 
{
  // The ProcessObject is not const-correct so the const_cast is required here
  SetNthInput( 2, const_cast<TInputImage1 *>( image3 ) );
}


/**
 * ThreadedGenerateData function. Performs the pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryFunctorImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage, TFunction>
::ThreadedGenerateData( const OutputImageRegionType &outputRegionForThread,
                        int threadId)
{
  // We use dynamic_cast since inputs are stored as DataObjects.  The
  // ImageToImageFilter::GetInput(int) always returns a pointer to a
  // TInputImage1 so it cannot be used for the second or third input.
  Input1ImagePointer inputPtr1
    = dynamic_cast<const TInputImage1*>((ProcessObject::GetInput(0)).GetPointer());
  Input2ImagePointer inputPtr2
    = dynamic_cast<const TInputImage2*>((ProcessObject::GetInput(1)).GetPointer());
  Input3ImagePointer inputPtr3 
    = dynamic_cast<const TInputImage3*>((ProcessObject::GetInput(2)).GetPointer());
  OutputImagePointer outputPtr = this->GetOutput(0);
  
  ImageRegionConstIterator<TInputImage1> inputIt1(inputPtr1, outputRegionForThread);
  ImageRegionConstIterator<TInputImage2> inputIt2(inputPtr2, outputRegionForThread);
  ImageRegionConstIterator<TInputImage3> inputIt3(inputPtr3, outputRegionForThread);
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
  inputIt3.GoToBegin();
  outputIt.GoToBegin();
  i = 0;
  while( !inputIt1.IsAtEnd() ) 
    {
    if ( threadId == 0 && !(i % updateVisits ) )
      {
      this->UpdateProgress((float)i/(float(updateVisits)*10.0));
      }

    outputIt.Set( m_Functor(inputIt1.Get(), inputIt2.Get(), inputIt3.Get() ) );
    ++inputIt1;
    ++inputIt2;
    ++inputIt3;
    ++outputIt;
    ++i;
    }
}





} // end namespace itk

#endif
