/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNaryImageFilter.txx
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
#ifndef _itkNaryImageFilter_txx
#define _itkNaryImageFilter_txx

#include "itkNaryImageFilter.h"
#include <itkImageRegionIterator.h>

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage, class TFunction >
NaryImageFilter<TInputImage,TOutputImage,TFunction>
::NaryImageFilter()
{
  // This number will be incremented each time an image
  // is added over the two minimum required
  this->SetNumberOfRequiredInputs( 2 );
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage, class TOutputImage, class TFunction>
void
NaryImageFilter<TInputImage,TOutputImage,TFunction>
::SetInput( unsigned int index, TInputImage * image ) 
{
  if( index+1 > this->GetNumberOfInputs() )
  {
    this->SetNumberOfRequiredInputs( index + 1 );
  }
  SetNthInput(index, image);
}



/**
 * ThreadedGenerateData Performs the pixel-wise addition
 */
template <class TInputImage, class TOutputImage, class TFunction>
void
NaryImageFilter<TInputImage, TOutputImage, TFunction>
::ThreadedGenerateData( const OutputImageRegionType &outputRegionForThread,
                        int threadId)
{

  const unsigned int numberOfInputImages = this->GetNumberOfInputs();
  
  OutputImagePointer outputPtr = this->GetOutput(0);
  ImageRegionIterator<TOutputImage> outputIt(outputPtr, outputRegionForThread);

  unsigned long updateVisits = 0;
  unsigned long i=0;

  // Clear the content of the output
  outputIt.GoToBegin();
  while( !outputIt.IsAtEnd() )
    {
      outputIt.Set( itk::NumericTraits< OutputImagePixelType >::Zero );
      ++outputIt;
    }
 

  
  const float progressBase = static_cast<float>(updateVisits) *
                             static_cast<float>(numberOfInputImages) *
                             10.0;
  
  for(unsigned int inputNumber=0; inputNumber < numberOfInputImages; inputNumber++ )
    {
    // We use dynamic_cast since inputs are stored as DataObjects.  
    InputImagePointer inputPtr = dynamic_cast<TInputImage*>(
                      (ProcessObject::GetInput( inputNumber )).GetPointer());

    ImageRegionIterator<TInputImage> inputIt(inputPtr, outputRegionForThread);

    // support progress methods/callbacks
    if ( threadId == 0 )
      {
      updateVisits = outputPtr->GetRequestedRegion().GetNumberOfPixels()/10;
      if ( updateVisits < 1 ) updateVisits = 1;
      }
          
   
    TFunction function;

    inputIt.GoToBegin();
    outputIt.GoToBegin();
    i = 0;
    while( !inputIt.IsAtEnd() ) 
      {
      if ( threadId == 0 && !(i % updateVisits ) )
        {
        this->UpdateProgress((float)i/progressBase);
        }
      
      outputIt.Set( function( outputIt.Get(), inputIt.Get() ) );
      ++inputIt;
      ++outputIt;
      ++i;
      }
    }
}

} // end namespace itk

#endif


