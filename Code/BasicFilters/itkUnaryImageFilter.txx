/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkUnaryImageFilter.txx
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
#ifndef _itkUnaryImageFilter_txx
#define _itkUnaryImageFilter_txx

#include "itkUnaryImageFilter.h"
#include <itkSimpleImageRegionIterator.h>

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage, class TFunction  >
UnaryImageFilter<TInputImage,TOutputImage,TFunction>
::UnaryImageFilter()
{
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage, class TOutputImage, class TFunction  >
void
UnaryImageFilter<TInputImage,TOutputImage,TFunction>
::SetInput( TInputImage * image ) 
{
  SetNthInput(0, image );
}



/**
 * GenerateData Performs the pixel-wise addition
 */
template <class TInputImage, class TOutputImage, class TFunction  >
void
UnaryImageFilter<TInputImage,TOutputImage,TFunction>
::GenerateData( void )
{
  
  typename TOutputImage::Pointer outputImage = dynamic_cast<TOutputImage *>(
                      (ProcessObject::GetOutput( 0 )).GetPointer());

  typename TInputImage::Pointer  inputImage  = dynamic_cast<TInputImage  *>(
                      (ProcessObject::GetInput(  0 )).GetPointer());

  outputImage->SetLargestPossibleRegion( 
                  inputImage->GetLargestPossibleRegion() );

  outputImage->SetBufferedRegion( 
                  inputImage->GetBufferedRegion() );

  outputImage->SetRequestedRegion( 
                  inputImage->GetRequestedRegion() );

  outputImage->Allocate();

  typename TOutputImage::RegionType region  = outputImage->GetRequestedRegion();

  SimpleImageRegionIterator< TInputImage >  it( inputImage,  region );
  SimpleImageRegionIterator< TOutputImage > ot( outputImage, region );

  it.Begin();
  ot.Begin();

  TFunction function;

  while( !it.IsAtEnd() ) 
  {
    ot.Set( function( it.Get() ) );
    ++it;
    ++ot;
  }


}





} // end namespace itk

#endif
