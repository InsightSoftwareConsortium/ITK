/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReflectImageFilter.txx
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
#ifndef _itkReflectImageFilter_txx
#define _itkReflectImageFilter_txx

#include "itkReflectImageFilter.h"
#include <itkImageRegionIterator.h>

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage >
ReflectImageFilter<TInputImage,TOutputImage >
::ReflectImageFilter()
{
  this->SetNumberOfRequiredInputs( 1 );
}


/**
 * GenerateData Performs the reflection
 */
template <class TInputImage, class TOutputImage >
void
ReflectImageFilter<TInputImage,TOutputImage>
::GenerateData( void )
{
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput(0);

  outputPtr->SetRequestedRegion( inputPtr->GetRequestedRegion() );
  outputPtr->SetBufferedRegion( inputPtr->GetBufferedRegion() );
  outputPtr->SetLargestPossibleRegion( inputPtr->GetLargestPossibleRegion() );
  outputPtr->Allocate();

  typedef ImageLinearIteratorWithIndex<TInputImage>  InputIterator;
  typedef ImageLinearIteratorWithIndex<TOutputImage> OutputIterator;

  InputIterator  inputIt(  inputPtr,  inputPtr->GetRequestedRegion() );
  OutputIterator outputIt( outputPtr, outputPtr->GetRequestedRegion() );

  // support progress methods/callbacks
  unsigned long updateVisits = 0;
  unsigned long visitInterval = 1000;
  updateVisits = 
      outputPtr->GetRequestedRegion().GetNumberOfPixels()/visitInterval;

  if ( updateVisits < 1 ) updateVisits = 1;
        
  inputIt.SetDirection( m_Direction );
  outputIt.SetDirection( m_Direction );

  inputIt.GoToBegin();
  outputIt.GoToBegin();
  unsigned long visitCounter = 0;

  while( !inputIt.IsAtEnd() ) 
    {

    outputIt.GoToEndOfLine();
    --outputIt;
    while( !inputIt.IsAtEndOfLine() ) 
      {
      if ( !(visitCounter % updateVisits ) )
        {
        this->UpdateProgress( static_cast<float>(visitCounter) / 
              static_cast<float>(updateVisits * visitInterval) );
        }

      outputIt.Set( inputIt.Get() );
      ++inputIt;
      --outputIt;
      ++visitCounter;
      }

     inputIt.NextLine();
     outputIt.GoToEndOfLine(); // NextLine() assumes that the 
     outputIt.NextLine();      // iterator is at the end of line.
    }
}

template <class TInputImage, class TOutputImage >
void
ReflectImageFilter<TInputImage,TOutputImage>::
PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  
  os << indent << "Direction: " << m_Direction << std::endl;
}

} // end namespace itk

#endif
