/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTernaryImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkTernaryImageFilter_txx
#define _itkTernaryImageFilter_txx

#include "itkTernaryImageFilter.h"
#include <itkImageRegionIterator.h>

namespace itk
{

/**
 * Constructor
 */
template < class TInputImage1, class TInputImage2, 
           class TInputImage3, class TOutputImage, class TFunction  >
TernaryImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::TernaryImageFilter()
{
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::SetInput1( TInputImage1 *image1 ) 
{
  SetNthInput(0, image1 );
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::SetInput2( TInputImage2 *image2 ) 
{
  SetNthInput(1, image2 );
}



/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryImageFilter<TInputImage1,TInputImage2,TInputImage3,TOutputImage,TFunction>
::SetInput3( TInputImage3 *image3 ) 
{
  SetNthInput(2, image3 );
}


/**
 * ThreadedGenerateData function. Performs the pixel-wise addition
 */
template <class TInputImage1, class TInputImage2, 
          class TInputImage3, class TOutputImage, class TFunction  >
void
TernaryImageFilter<TInputImage1, TInputImage2, TInputImage3, TOutputImage, TFunction>
::ThreadedGenerateData( const OutputImageRegionType &outputRegionForThread,
                        int threadId)
{
  // We use dynamic_cast since inputs are stored as DataObjects.  The
  // ImageToImageFilter::GetInput(int) always returns a pointer to a
  // TInputImage1 so it cannot be used for the second or third input.
  Input1ImagePointer inputPtr1
    = dynamic_cast<TInputImage1*>((ProcessObject::GetInput(0)).GetPointer());
  Input2ImagePointer inputPtr2
    = dynamic_cast<TInputImage2*>((ProcessObject::GetInput(1)).GetPointer());
  Input3ImagePointer inputPtr3 
    = dynamic_cast<TInputImage3*>((ProcessObject::GetInput(2)).GetPointer());
  OutputImagePointer outputPtr = this->GetOutput(0);
  
  ImageRegionIterator<TInputImage1> inputIt1(inputPtr1, outputRegionForThread);
  ImageRegionIterator<TInputImage2> inputIt2(inputPtr2, outputRegionForThread);
  ImageRegionIterator<TInputImage3> inputIt3(inputPtr3, outputRegionForThread);
  ImageRegionIterator<TOutputImage> outputIt(outputPtr, outputRegionForThread);

  // support progress methods/callbacks
  unsigned long updateVisits = 0, i=0;
  if ( threadId == 0 )
    {
    updateVisits = 
      outputPtr->GetRequestedRegion().GetNumberOfPixels()/10;
    if ( updateVisits < 1 ) updateVisits = 1;
    }
        
  TFunction function;

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

    outputIt.Set( function( inputIt1.Get(), inputIt2.Get(), inputIt3.Get() ) );
    ++inputIt1;
    ++inputIt2;
    ++inputIt3;
    ++outputIt;
    ++i;
    }
}





} // end namespace itk

#endif
