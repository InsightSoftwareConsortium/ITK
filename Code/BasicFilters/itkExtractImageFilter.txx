/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkExtractImageFilter.txx
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
#ifndef _itkExtractImageFilter_txx
#define _itkExtractImageFilter_txx

#include "itkExtractImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
ExtractImageFilter<TInputImage,TOutputImage>
::ExtractImageFilter()
{
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ExtractImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "ExtractionRegion: " << m_ExtractionRegion << std::endl;
}


/** 
 * ExtractImageFilter produces an image which is a different resolution
 * than its input image.  As such, ExtractImageFilter needs to provide an
 * implementation for GenerateOutputInformation() in order to inform
 * the pipeline execution model.  The original documentation of this
 * method is below.
 *
 * \sa ProcessObject::GenerateOutputInformaton() 
 */
template <class TInputImage, class TOutputImage>
void 
ExtractImageFilter<TInputImage,TOutputImage>
::GenerateOutputInformation()
{
  // call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();
 
  // get pointers to the input and output
  OutputImagePointer outputPtr = this->GetOutput();
  InputImagePointer inputPtr = this->GetInput();

  if ( !outputPtr || !inputPtr)
    {
    return;
    }

  // Set the output image size to the same value as the extraction region.
  outputPtr->SetLargestPossibleRegion( m_ExtractionRegion );
}

  /** 
   * ExtractImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() 
   */
template <class TInputImage, class TOutputImage>
void 
ExtractImageFilter<TInputImage,TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  int i;

  itkDebugMacro(<<"Actually executing");

  // Get the input and output pointers
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput();

  // Set the requested region for the input region to the same as
  // that for the output region.
  inputPtr->SetRequestedRegion( outputRegionForThread );

  // support progress methods/callbacks
  unsigned long updateVisits = 0;
  unsigned long totalPixels = 0;
  if ( threadId == 0 )
    {
    totalPixels = 
      outputPtr->GetRequestedRegion().GetNumberOfPixels();
    updateVisits = totalPixels / 10;
    if( updateVisits < 1 ) updateVisits = 1;
    }

  // Define the iterators.
  typedef
    ImageRegionIterator<TOutputImage> OutputIterator;
  typedef 
    ImageRegionIterator<TInputImage> InputIterator;
  OutputIterator outIt(outputPtr, outputRegionForThread);
  InputIterator inIt(inputPtr, outputRegionForThread);

  // walk the output region, and sample the input image
  for (i=0; !outIt.IsAtEnd(); ++outIt, ++inIt, i++ )
    {
      if ( threadId == 0 && !(i % updateVisits ) )
        {
          this->UpdateProgress((float)i / (float)totalPixels);
        }
      
      // copy the input pixel to the output
      outIt.Set( inIt.Get());
    }
}

} // end namespace itk

#endif
